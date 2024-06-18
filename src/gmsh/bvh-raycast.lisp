(in-package :gmsh/bvh)

(declaim (inline raycast %int/raycast %int/simple-raycast
                         simd4/raycast* simd4/simple-raycast*))

 (veq:fvdef raycast (bvh (:va 3 org ll))
   (declare #.*opt1* (bvh bvh) (veq:ff org ll))
   "raycast from org along ll."
   (macrolet (($ (ci field)
                 `(,(auxin::psymb :gmsh/bvh
                      (string-upcase (gmsh::mkstr "bvh-node-" field))) ,ci))
              (nxt (ci) `($ ,ci :ref)) (poly (ci) `($ ,ci :ref)) ; this is the same slot
              (num (ci) `($ ,ci :num)) (bbox (ci) `($ ,ci :bbox)))
     (veq:xlet ((nodes (gmsh/bvh::bvh-nodes bvh))
                (polyfx (gmsh/bvh::bvh-polyfx bvh))
                (f3!inv (-eps-div ll)))
       (declare (bvh-node-vec nodes) (veq:fvec polyfx))
       (let ((hs 900000f0) (hi -1))
         (declare (veq:ff hs) (veq:in hi))
         (labels ((rec (ni &aux (ci (aref nodes ni)) (n (num ci)))
                    (declare (veq:pn ni n) (bvh-node ci))
                    (when (bvh2/bbox-test (bbox ci) inv org)
                      (loop for i3 of-type veq:pn from (poly ci) by 9
                            repeat n
                            for s of-type veq:ff = (polyx polyfx i3 org ll)
                            if (and (< #.*eps* s hs)
                                    (< s 1f0))
                            do (setf hs s hi i3))
                      (when (zerop n) (rec (1+ (nxt ci)))
                                      (rec (nxt ci))))))
           (rec 0)
       (values (if (< -1 hi) (the veq:pn (/ hi #.+polyleap+)) -1) hs))))))

 ; ----- INT NODES ---------------------------------------------------------

 (veq:fvdef* int/raycast (bvh (:va 3 org ll))
   (declare  #.*opt1* (bvh bvh) (veq:ff org ll))
   "raycast from org along ll. returns (values i s), where
  - i is the index (in the bvh) of the closest polygon, or -1,
  - s is the lerp along ll.
 bvh must be constructed with mode :bh2-stackless"
   (macrolet
     ((nodes- (slot) `(aref nodes (the veq:pn (+ ,slot curr)))))
     (let ((mima (bvh-mima bvh))
           (polyfx (bvh-polyfx bvh))
           (nodes (bvh-int-nodes bvh)))
       (declare (veq:fvec mima polyfx) (veq:pvec nodes))
       (veq:xlet ((f3!inv (-eps-div ll))
                  (hs 900000f0) (hi #.(1- (expt 2 32))))
         (declare (veq:ff hs) (veq:pn hi))
         (loop named lp
               with curr of-type veq:pn = 0
               do (if (int/bbox-test mima (the veq:pn (ash curr #.(int/bbox-leap))) inv org)
                      ; check nodes, depends on num
                      (progn (loop repeat (the veq:pn (nodes- 0)) ; is it faster with different repeat order?
                                   for i3 of-type veq:pn from (nodes- 1) by 9
                                   for s of-type veq:ff = (polyx polyfx i3 org ll)
                                   if  (and (< #.*eps* s 1f0)
                                            (< s hs))
                                   do (setf hs s hi i3))
                             ; traverse left
                             (if (> (nodes- 0) 0) (setf curr (nodes- 3))
                                                  (setf curr (nodes- 1)))
                             (when (< curr 1) (return-from lp)))
                      ; traverse right
                      (progn (setf curr (nodes- 3))
                             (when (< curr 1) (return-from lp)))))
         (values (if (< hi #.(1- (expt 2 32))) (the veq:pn (floor hi #.+polyleap+)) -1)
                 hs)))))

 (veq:fvdef* int/simple-raycast (bvh (:va 3 org ll))
   (declare  #.*opt1* (bvh bvh) (veq:ff org ll))
   "raycast from org along ll. returns (values i s), where
  - i is the index (in the bvh) of the closest polygon, or -1,
  - s is the lerp along ll.
 bvh must be constructed with mode :bvh2-stackless"
   (macrolet
     ((nodes- (slot) `(aref nodes (the veq:pn (+ ,slot curr)))))
     (let ((mima (bvh-mima bvh))
           (polyfx (bvh-polyfx bvh))
           (nodes (bvh-int-nodes bvh)))
       (declare (veq:fvec mima polyfx) (veq:pvec nodes))
       (veq:xlet ((f3!inv (-eps-div ll)))
         (loop named lp
               with curr of-type veq:pn = 0
               do (if (int/bbox-test mima (the veq:pn (ash curr #.(int/bbox-leap))) inv org)
                      (progn (loop repeat (the veq:pn (nodes- 0)) ; is it faster with different repeat order?
                                   for i3 of-type veq:pn from (nodes- 1) by 9
                                   if  (and (< #.*eps* (polyx polyfx i3 org ll) 1f0))
                                   do (return-from int/simple-raycast 0f0))

                             (if (> (nodes- 0) 0) (setf curr (nodes- 3))
                                                  (setf curr (nodes- 1)))
                             (when (< curr 1) (return-from lp)))

                      (progn (setf curr (nodes- 3))
                             (when (< curr 1) (return-from lp))))))
       1f0)))


(defmacro simd4/raycast (bvh &rest rest)
  (declare (symbol bvh)) "raycast bvh from org alon ll using simd."
  `(let ((nodes (gmsh/bvh::bvh-simd-nodes ,bvh))
        (mima (gmsh/bvh::bvh-simd-mima ,bvh))
        (polyfx (gmsh/bvh::bvh-polyfx ,bvh)))
     (veq:mvc #'simd4/raycast* nodes mima polyfx ,@rest)))

(defmacro simd4/simple-raycast (bvh &rest rest)
  (declare (symbol bvh)) "raycast bvh from org alon ll using simd.
returns 0f0 if it hits anything; 1f0 otherwise"
  `(let ((nodes (gmsh/bvh::bvh-simd-nodes ,bvh))
        (mima (gmsh/bvh::bvh-simd-mima ,bvh))
        (polyfx (gmsh/bvh::bvh-polyfx ,bvh)))
     (veq:mvc #'simd4/simple-raycast* nodes mima polyfx ,@rest)))

(macrolet ((nodes- (i) `(aref nodes ,i))
           (poly (c8 si) `(nodes- (+ ,c8 ,(+ 4 si)))) ; ref and nxt is same field
           (ref (c8 si) `(nodes- (+ ,c8 ,(+ 4 si))))
           (num (c8 si) `(nodes- (+ ,c8 ,si)))
           (do-node (c8 si &rest rest)
             `(let ((ref (ref ,c8 ,si)) (num (num ,c8 ,si)))
                (declare (veq:pn ref num))
                (loop repeat num ; TODO
                      for i3 of-type veq:pn from ref by 9
                      for s of-type veq:ff = (polyx polyfx i3 ,@rest)
                      if (and (< #.*eps* s 1f0) (< s hs))
                      do (setf hs s hi i3))
                (when (and (> ref 0) (< num 1)) (push-rt (the veq:pn ref)))))
             (do-simple-node (c8 si &rest rest)
               `(let ((ref (ref ,c8 ,si)) (num (num ,c8 ,si)))
                  (declare (veq:pn ref num))
                  (loop repeat num ; TODO
                        for i3 of-type veq:pn from ref by 9
                        if (< #.*eps* (polyx polyfx i3 ,@rest) #. (- 1f0 *eps*))
                        do (return-from simd4/simple-raycast* 0f0))
                  (when (and (> ref 0) (< num 1)) (push-rt (the veq:pn ref))))))

(veq:fvdef simd4/raycast* (nodes mima polyfx (:va 3 org ll))
  (declare #.*opt1* (veq:ivec nodes) (veq:fvec mima polyfx) (veq:ff org ll))
  "raycast from org along ll. returns 0f0 if something is hit, 1f0 otherwise.
bvh must be constructed with mode :bvh4-simd.
stck must be created with (gmsh:make-fast-stack), which returns an array of
type gmsh:stack."
  (veq:xlet ((f3!inv (-eps-div ll))
             (hs 900000f0) (hi #.(1- (expt 2 32))))
    (declare (veq:ff hs) (veq:pn hi) )
    (gmsh::with-fast-stack (rt :stack *stck*)
      (nil-rt)
      (push-rt 0)
      (loop while (con-rt)
             for c of-type veq:pn = (pop-rt)
             for nn of-type veq:pn =
                 (gmsh/bvh::m@simd/bvh4/bbox-test gmsh/bvh::mima
                   (the veq:pn (+ (the veq:pn (ash c #.(simd4/bbox-leap)))))
                   inv org)
             for c8 of-type veq:pn = (ash c 3)
             do (when (oddp (the veq:pn (ash nn 0))) (do-node c8 0 org ll))
                (when (oddp (the veq:pn (ash nn -1))) (do-node c8 1 org ll))
                (when (oddp (the veq:pn (ash nn -2))) (do-node c8 2 org ll))
                (when (oddp (the veq:pn (ash nn -3))) (do-node c8 3 org ll))))
          (values (if (< hi #.(1- (expt 2 32))) (the veq:pn (floor hi #.+polyleap+)) -1)
                  hs)))


(veq:fvdef simd4/simple-raycast* (nodes mima polyfx (:va 3 org ll))
  (declare #.*opt1* (veq:ivec nodes) (veq:fvec mima polyfx) (veq:ff org ll))
  "raycast from org along ll. returns 0f0 if something is hit, 1f0 otherwise.
bvh must be constructed with mode :bvh4-simd.
stck must be created with (gmsh:make-fast-stack), which returns an array of
type gmsh:stack."
  (veq:xlet ((f3!inv (-eps-div ll)))
    (gmsh::with-fast-stack (rt :stack *stck*)
      (nil-rt)
      (push-rt 0)
      (loop while (con-rt)
            for c of-type veq:pn = (pop-rt)
            for nn of-type veq:pn =
                (gmsh/bvh::m@simd/bvh4/bbox-test gmsh/bvh::mima
                  (the veq:pn (+ (the veq:pn (ash c #.(simd4/bbox-leap)))))
                  inv org)
            for c8 of-type veq:pn = (ash c 3)
            do (when (oddp (the veq:pn (ash nn 0)))  (do-simple-node c8 0 org ll))
               (when (oddp (the veq:pn (ash nn -1))) (do-simple-node c8 1 org ll))
               (when (oddp (the veq:pn (ash nn -2))) (do-simple-node c8 2 org ll))
               (when (oddp (the veq:pn (ash nn -3))) (do-simple-node c8 3 org ll))))
    1f0)))
