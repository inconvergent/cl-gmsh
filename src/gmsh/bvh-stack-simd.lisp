(in-package :gmsh/bvh)


(blurb BVH4 SIMD MIMA LAYOUT
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
       ========================= )

(veq:fvdef simd/build-int-nodes (nodes mima)
  (declare #.*opt* (veq:pvec nodes) (veq:fvec mima))
  ; (old: gt == num, gt 1 == poly, gt 2 == lft)
  ; NEW SIMD4 NODE LAYOUT: LEAP 4*2 = 8
  ; new: num 1 2 3 4 ; ref (poly/nxt) 1 2 3 4
  (veq:xlet ((num-leaf 0)
             (num-dummy 0)
             (num-internal 0)
             (stack (list)) (p!ni 0)
             (new-nodes (veq:i$val -1 (* 4 (veq:4$num nodes))))
             (new-mima (veq:f$zero (* 4 (length mima))))) ; TODO: this seems incorrect
    (declare (list stack) (veq:ivec new-nodes) (veq:fvec new-mima))
    (labels ((get-free-index () (incf ni) (1- ni))
             (out-leap (i) (* 8 i))
             ; (out-ind (i) (/ i 8)) (in-leap (i) (* 4 i)) (in-ind (i) (/ i 4))
             (stack-add (subnodes) (push subnodes stack))
             (stack-nxt () (pop stack))
             (leaf? (i) (declare (veq:in i)) (and (> i -1) (> (gt i) 0)))
             (gt (i &optional (j 0)) (declare (veq:pn i j))
               (aref nodes (+ (* 4 i) j)))
             (gtx (i) (declare (veq:pn i))
               (if (leaf? i) (list i 0) (list (/ (gt i 1) 4) (1+ (/ (gt i 1) 4)))))
             (gt4 (i) (declare (veq:in i)) (concatenate 'list (gtx i) (gtx (1+ i))))
             (set-mima (i ni si &aux (mm (+ (* +simd-mima-leap+ ni) si)) (8i (* 8 i)))
               (declare (veq:pn i ni si mm 8i))
               (setf (veq:$ new-mima (+ mm   )) (aref mima    8i   )
                     (veq:$ new-mima (+ mm  4)) (aref mima (+ 8i 1))
                     (veq:$ new-mima (+ mm  8)) (aref mima (+ 8i 2))
                     (veq:$ new-mima (+ mm 12)) (aref mima (+ 8i 3))
                     (veq:$ new-mima (+ mm 16)) (aref mima (+ 8i 4))
                     (veq:$ new-mima (+ mm 20)) (aref mima (+ 8i 5))))
             (set-leaf-subnode (i ni si &aux (ni* (out-leap ni)))
               (declare (veq:pn i ni si))
               (incf num-leaf)
               (setf (aref new-nodes (+ si ni*  )) (gt i)    ; num
                     (aref new-nodes (+ si ni* 4)) (gt i 1)) ; ref/poly
               (set-mima i ni si))
             (set-subnode (i ni si ninxt &aux (ni* (out-leap ni)))
               (declare (veq:pn i ni si ninxt ni*))
               (incf num-internal)
               (setf (aref new-nodes (+ si ni*  )) 0      ; num
                     (aref new-nodes (+ si ni* 4)) ninxt) ; ref/nxt
               (set-mima i ni si))
             (set-dummy-subnode (ni si &aux (ni* (out-leap ni))
                                            (mm (+ (* +simd-mima-leap+ ni) si)))
               (declare (veq:pn ni si ni*))
               (incf num-dummy)
               (setf (aref new-nodes (+ si ni*))   0  ; num
                     (aref new-nodes (+ si ni* 4)) 0) ; ref
               (setf (veq:$ new-mima    mm   )   1.0
                     (veq:$ new-mima (+ mm 4))  -1.0
                     (veq:$ new-mima (+ mm 8))   1.0
                     (veq:$ new-mima (+ mm 12)) -1.0
                     (veq:$ new-mima (+ mm 16))  1.0
                     (veq:$ new-mima (+ mm 20)) -1.0)))
      (stack-add (list (get-free-index) (gt4 (/ (gt 0 1) 4))))
      (loop while stack for (ni subnodes) = (stack-nxt)
            do (loop for i of-type veq:in in subnodes
                     for si of-type veq:pn from 0 ; sub node index [0 4)
                     if (leaf? i) do (set-leaf-subnode i ni si)
                     else if (plusp i)
                       do (veq:xlet ((p!ninxt (get-free-index)))
                                    (set-subnode i ni si ninxt)
                                    (stack-add (list ninxt (gt4 (/ (gt i 1) 4)))))
                     else do (set-dummy-subnode ni si)))
      (veq:vpr num-internal num-leaf num-dummy)
      (values new-nodes new-mima ni))))

(veq:fvdef simd/compress-int-nodes (nodes mima)
  (declare #.*opt* (veq:ivec nodes) (veq:fvec mima))
  (veq:xlet ((num-leaf 0) (num-dummy 0) (num-internal 0) (num-moved 0)
             (stack (list)) (p!ni 0))
    (declare (list stack))
    (labels ((stack-add (subnodes) (push subnodes stack))
             (stack-nxt () (pop stack))
             (gt (i &optional (j 0)) (declare (veq:pn i j))
               (aref nodes (+ (* 8 i) j)))
             (leaf? (i &optional (s 0)) (declare (veq:pn i s))
               (and (> i -1) (> (gt i s) 0)))
             (internal? (i &optional (s 0)) (declare (veq:pn i s))
               (and (< (gt i s) 1) (> (gt i (+ 4 s)) 0) ))
             (find-first-leaf (ni) (declare (veq:pn ni))
               (labels ((rec (c) (declare (veq:in c))
                  (loop for si from 0 repeat 4
                        do (when (and (/= c ni) (leaf? c si))
                             (return-from find-first-leaf (list c si)))
                           (when (internal? c si) (rec (gt c (+ si 4)))))))
                  (rec ni)))
             (move-leaf (ani asi bni bsi &aux
                          (a (+ (* +simd-mima-leap+ ani) asi))
                          (b (+ (* +simd-mima-leap+ bni) bsi)))
               (declare (veq:pn ani asi bni bsi a b))
               (rotatef (aref mima (+ a 0))  (aref mima (+ b 0)))
               (rotatef (aref mima (+ a 4))  (aref mima (+ b 4)))
               (rotatef (aref mima (+ a 8))  (aref mima (+ b 8)))
               (rotatef (aref mima (+ a 12)) (aref mima (+ b 12)))
               (rotatef (aref mima (+ a 16)) (aref mima (+ b 16)))
               (rotatef (aref mima (+ a 20)) (aref mima (+ b 20)))
               (rotatef (aref nodes (+ (* 8 ani) asi)) (aref nodes (+ (* 8 bni) bsi)))
               (rotatef (aref nodes (+ (* 8 ani) asi 4)) (aref nodes (+ (* 8 bni) bsi 4)))
               ))
      (stack-add 0)
      (loop while stack for ni = (stack-nxt)
            do (loop for si from 0 repeat 4
                     if (leaf? ni si)
                         do (incf num-leaf)
                            ; (lqn:out "~&leaf ~a ~a~&" ni si)
                    else if (internal? ni si)
                         do (incf num-internal)
                            ; (lqn:out "~&internal: ~a ~a~&" ni si)
                            (stack-add (gt ni (+ si 4)))
                    else do (incf num-dummy)
                            ; (lqn:out "~&dummy: ~a ~a~&" ni si)
                            (let ((lf (find-first-leaf ni)))
                              (when lf
                                (incf num-moved)
                                (apply #'move-leaf ni si lf)))
                            ))
      (veq:vpr num-internal num-leaf num-dummy num-moved)
      (values nodes mima))))
