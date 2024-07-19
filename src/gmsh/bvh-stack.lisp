(in-package :gmsh/bvh)


(veq:fvdef -make-objects-and-normals (vfx objs)
  (declare #.*opt* (function vfx) (list objs))
  (loop for p in objs for vv = (f@vfx p)
        collect (list p (bbox (veq:f3$mima vv))
                        (veq:f3$point (gmsh::-poly-normal vv))
                        vv)))

(veq:fvdef make-polyx ((:va 3 v0 v1 v2)) (declare #.*opt* (veq:ff v0 v1 v2))
  "#(v2v0x v2v0y v2v0z, ... v1v0x , ... , v0x v0y v0z)"
  (veq:~ (f3!@- v2 v0) (f3!@- v1 v0) (veq:f3 v0)))

(doc   BVH2 NODE LAYOUT (ROW MAJOR LEAP 4)
       =========================
       |00   |01   |02   |03   |
       |0num |1ref |2ax  |3rht |
       =========================

       BVH2 MIMA LAYOUT (ROW MAJOR LEAP 8)
       =================================================
       |00   |01   |02   |03   |04   |05   |06   |07   |
       |0miX |0maX |1miY |1maY |2miZ |2maZ |0    |0    |
       ================================================= )

(defun int/make-stackless! (new-nodes)
  (declare #.*opt* (veq:pvec new-nodes))
  (let ((stack (list 0)))
    (declare (list stack))
    (labels ((gt (i &optional (j 0)) (declare (veq:pn i j)) (aref new-nodes (+ i j)))
             (stack-peek () (when stack (elt stack 0)))
             (stack-add (i) (push i stack))
             (stack-nxt () (pop stack)))
     (loop while stack for i of-type veq:pn = (stack-nxt) for pk = (stack-peek)
           do (typecase pk (veq:pn (setf (aref new-nodes (+ i 3)) pk)))
              (when (and (zerop (gt i)) (not (zerop (gt i 1))))
                    (stack-add (+ (gt i 1) 4))
                    (stack-add (gt i 1)))))
    new-nodes))

(veq:fvdef int/build-int-nodes (nodes num-nodes
                                  &key set-lvl (leap 8)
                                  &aux (res (veq:p4$zero num-nodes))
                                       (mima (veq:f$zero (* leap num-nodes))))
  (declare #.*opt* (bvh-nodes nodes))
  ; 0   ; 1                    ; 2  ; 3
  ; num ; ref/[nxt node, poly] ; ax ; (right)
  (loop for i from 0 repeat num-nodes
        do (auxin:with-struct (bvh-node- bbox num ref lvl ax) (aref nodes i)
             ; (* ref 4) becuase there are 4 ints in a node
             (setf (veq:4$ res i)
                   (values num (if (< 0 num) ref (* ref 4)) ax (if set-lvl lvl 0)))
             (veq:$nvset (mima 6 (* i leap)) (veq:f$ bbox 0 3 1 4 2 5))))
  (values res mima))

(veq:fvdef gpu/pack-bvh (bvh &key (mats gmsh::*mats*) (matpar gmsh::*matpar*))
  (declare #.*opt* (bvh bvh) (list matpar mats))
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
              (values (i4%@$fx! mat* ((i (:va 4 x))
                                      (declare (ignorable x))
                                      (veq:~ (m@do-mat (veq:2$ mat i)) 0 0)))
                      par*))
           (pck-mima (mima &aux (mima* (veq:f$copy mima)))
             (loop for i from 0 below (length mima) by 8
                   do (veq:$nvset (mima* 8 i)
                        (veq:~ (veq:$ mima (+ i 0) (+ i 2) (+ i 4)) 0.0
                        (veq:$ mima (+ i 1) (+ i 3) (+ i 5)) 0.0)))
             mima*)
           ; TODO: this should be unnecessary
           (pck-nodes (nodes &aux (nodes* (veq:i$coerce nodes)))
             (loop for i from 0 below (length nodes) by 4
                   if (zerop (aref nodes* (+ i 3)))
                   do (setf (aref nodes* (+ i 3)) -4))
             nodes*))
   (auxin:with-struct
     (gmsh/bvh::bvh- nodes int-nodes polys mima polyfx normals mat) bvh
     (veq:~ (veq:new-stride (3 4 ff) normals) (pck-mima mima)
            (veq:new-stride (3 4 ff) polyfx)  (pck-nodes int-nodes)
            (pck-mat mat)))))

