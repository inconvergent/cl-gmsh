(in-package :gmsh/bvh)


(veq:fvdef -make-objects-and-normals (vfx objs)
  (loop for p in objs for vv = (f@vfx p)
        collect (list p (bbox (veq:f3$mima vv))
                        (veq:f3$point (gmsh::-poly-normal vv))
                        vv)))

(veq:fvdef make-polyx ((:va 3 v0 v1 v2))
  (declare (veq:ff v0 v1 v2)) "#(v2v0x v2v0y v2v0z, ... v1v0x , ... , v0x v0y v0z)"
  (veq:~ (f3!@- v2 v0) (f3!@- v1 v0) (veq:f3 v0)))

; -----------------------------------------------------

(blurb

  BVH2 MIMA LAYOUT
  =================================================
  |00   |01   |02   |03   |04   |05   |06   |07   |
  |0miX |0maX |1miY |1maY |2miZ |2maZ |pad  |pad  |
  =================================================

  BVH4 SIMD MIMA LAYOUT
  =========================
  |00   |01   |02   |03   |
  |0miX |1miX |2miX |3miX |    == miX 0
  -------------------------
  |04   |05   |06   |07   |
  |0maX |1maX |2maX |3maX |    == maX 1
  =========================
  |08   |09   |10   |11   |
  |0miY |1miY |2miY |3miY |    == miY 2
  -------------------------
  |12   |13   |14   |15   |
  |0maY |1maY |2maY |3maY |    == maY 3
  =========================
  |16   |17   |18   |19   |
  |0miZ |1miZ |2miZ |3miZ |    == miZ 4
  -------------------------
  |20   |21   |22   |23   |
  |0maZ |1maZ |2maZ |3maZ |    == maZ 5
  =========================
  )

(defun int/make-stackless! (new-nodes)
  (declare #.*opt* (veq:pvec new-nodes))
  (veq:xlet ((stack (list 0)))
   (declare (list stack) (veq:pvec new-nodes))
   (labels ((gt (i &optional (j 0)) (declare (veq:pn i j)) (aref new-nodes (+ i j)))
            (stack-peek () (when stack (elt stack 0)))
            (stack-add (i) (push i stack))
            (stack-nxt () (pop stack)))
    (loop while stack for i of-type veq:pn = (stack-nxt)
          for pk = (stack-peek)
          do (typecase pk (veq:pn (setf (aref new-nodes (+ i 3)) pk)))
             (when (and (zerop (gt i)) (not (zerop (gt i 1))))
                   (stack-add (+ (gt i 1) 4))
                   (stack-add (gt i 1)))))
   new-nodes))


(veq:fvdef int/build-int-nodes (nodes num-nodes
                                  &key set-lvl (leap 8)
                                  &aux (res (veq:p4$zero num-nodes))
                                       (mima (veq:f$zero (* leap num-nodes))))
  (declare #.*opt* (bvh-node-vec nodes))
  ; 0   ; 1                    ; 2  ; 3
  ; num ; ref/[nxt node, poly] ; ax ; (right)
  (loop for i from 0 repeat num-nodes
        do (auxin:with-struct (bvh-node- bbox num ref lvl ax) (aref nodes i)
             ; (* ref 4) becuase there are 4 ints in a node
             (setf (veq:4$ res i)
                   (values num (if (< 0 num) ref (* ref 4)) ax (if set-lvl lvl 0)))
             (veq:$nvset (mima 6 (* i leap)) (veq:f$ bbox 0 3 1 4 2 5))))
  (values res mima))

(veq:fvdef simd/build-int-nodes (nodes mima)
  (declare #.*opt* (veq:pvec nodes) (veq:fvec mima))
  ; (old: gt == num, gt 1 == poly, gt 2 == lft)
  ; NEW SIMD4 NODE LAYOUT: LEAP 4*3 = 12
  ; new: num 1 2 3 4 ; ref (poly/nxt) 1 2 3 4 ; pad 1 2 3 4
  ; TODO: reduce simd int node size to 2?
  (veq:xlet ((stack (list)) (p!ni 0)
             (new-nodes (veq:i$val -1 (* 4 (veq:4$num nodes))))
             (new-mima (veq:f$zero (* 4 (length mima))))) ; TODO: this seems incorrect
    (declare (list stack) (veq:ivec new-nodes) (veq:fvec new-mima))
    (labels ((get-free-index () (incf ni) (1- ni))
             (out-leap (i) (* 8 i)) (out-ind (i) (/ i 8))
             (in-leap (i) (* 4 i)) (in-ind (i) (/ i 4))
             (stack-add (subnodes) (push subnodes stack))
             (stack-nxt () (pop stack))
             (leaf? (i) (declare (veq:in i)) (and (> i -1) (> (gt i) 0)))
             (gt (i &optional (j 0)) (declare (veq:pn i) (veq:pn j))
               (aref nodes (+ (* 4 i) j)))
             (gtx (i) (declare (veq:pn i))
               (if (leaf? i) (list i 0) (list (/ (gt i 1) 4) (1+ (/ (gt i 1) 4)))))
             (gt4 (i) (declare (veq:in i)) (concatenate 'list (gtx i) (gtx (1+ i))))
             (set-mima (i ni si &aux (mm (+ (* +simd-int-leap+ ni) si)) (8i (* 8 i)))
               (declare (veq:pn i ni si mm 8i))
               (setf (veq:$ new-mima (+ mm   )) (aref mima    8i   )
                     (veq:$ new-mima (+ mm  4)) (aref mima (+ 8i 1))
                     (veq:$ new-mima (+ mm  8)) (aref mima (+ 8i 2))
                     (veq:$ new-mima (+ mm 12)) (aref mima (+ 8i 3))
                     (veq:$ new-mima (+ mm 16)) (aref mima (+ 8i 4))
                     (veq:$ new-mima (+ mm 20)) (aref mima (+ 8i 5))))
             (set-leaf-node (i ni si &aux (ni* (out-leap ni)))
               (declare (veq:pn i ni si))
               (setf (aref new-nodes (+ si ni*  )) (gt i)    ; num
                     (aref new-nodes (+ si ni* 4)) (gt i 1)) ; ref/poly
               (set-mima i ni si))
             (set-node (i ni si ninxt &aux (ni* (out-leap ni)))
               (declare (veq:pn i ni si ninxt ni*))
               (setf (aref new-nodes (+ si ni*  )) 0      ; num
                     (aref new-nodes (+ si ni* 4)) ninxt) ; ref/nxt
               (set-mima i ni si))
             (set-dummy-node (ni si &aux (ni* (out-leap ni))
                                         (mm (+ (* +simd-int-leap+ ni) si)))
               (declare (veq:pn ni si ni*))
               (setf (aref new-nodes (+ si ni*))   0  ; num
                     (aref new-nodes (+ si ni* 4)) 0) ; ref
               (setf (veq:$ new-mima    mm   )   1f0
                     (veq:$ new-mima (+ mm 4))  -1f0
                     (veq:$ new-mima (+ mm 8))   1f0
                     (veq:$ new-mima (+ mm 12)) -1f0
                     (veq:$ new-mima (+ mm 16))  1f0
                     (veq:$ new-mima (+ mm 20)) -1f0)))
      (stack-add (list (get-free-index) (gt4 (/ (gt 0 1) 4))))
      (loop while stack for (ni subnodes) = (stack-nxt)
            do (loop for i of-type veq:in in subnodes
                     for si of-type veq:pn from 0 ; sub node index [0 4)
                     if (leaf? i) do (set-leaf-node i ni si)
                     else if (plusp i)
                     do (veq:xlet ((p!ninxt (get-free-index)))
                          (set-node i ni si ninxt)
                          (stack-add (list ninxt (gt4 (/ (gt i 1) 4)))))
                     else do (set-dummy-node ni si)))
      (values new-nodes new-mima ni))))

; TODO: make something general for wrncnt?
(veq:fvdef gpu/pack-bvh (bvh &key (mats gmsh::*mats*) (matpar gmsh::*matpar*))
  (declare (bvh bvh) (list matpar mats))
  "return packed bvh arrays for gpu rendering."
  (labels ((find-mat-key (mk &aux (res (cdr (assoc mk mats))))
              (if res res (progn (wrn :gpumat-key "missing mat for: ~a" mk) 0)))
           (find-color-ind (ci)
             (loop for i from 0 for (cand . rest) in matpar
                   if (eq cand ci) do (return-from find-color-ind i))
             (wrn :gpumat-ind "missing mat color for: ~a" ci) 0)
           (do-mat (mk ci) (declare (keyword mk ci))
             (values (find-mat-key mk) (find-color-ind ci)))
           (pck-mat (mat &aux (mat* (veq:i4$zero (veq:2$num mat))) (par* (veq:f4$zero 30)))
              (declare (veq:svec mat))
              (loop for (s r g b a) in matpar for i from 0
                    do (setf (veq:4$ par* i) (veq:f4 r g b a)))
              (values (i4%@$fx! mat* ((i (:va 4 x)) (veq:~ (m@do-mat (veq:2$ mat i)) 0 0)))
                      par*))
           (pck-mima (mima &aux (mima* (veq:f$copy mima)))
             (loop for i from 0 below (length mima) by 8
                   do (veq:$nvset (mima* 8 i)
                        (veq:~ (veq:$ mima (+ i 0) (+ i 2) (+ i 4)) 0f0
                        (veq:$ mima (+ i 1) (+ i 3) (+ i 5)) 0f0)))
             mima*)
           (pck-nodes (nodes &aux (nodes* (veq:i$coerce nodes)))
             (loop for i from 0 below (length nodes) by 4
                   if (zerop (aref nodes* (+ i 3))) do (setf (aref nodes* (+ i 3)) -4))
             nodes*))
   (auxin:with-struct
    (gmsh/bvh::bvh- nodes int-nodes polys mima polyfx normals mat) bvh
    (veq:~ (veq:new-stride (3 4 ff) normals) (pck-mima mima)
           (veq:new-stride (3 4 ff) polyfx)  (pck-nodes int-nodes)
           (pck-mat mat)))))

