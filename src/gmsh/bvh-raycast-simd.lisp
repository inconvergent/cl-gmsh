(in-package :gmsh/bvh)

(defmacro /nodes- (i) `(aref nodes ,i))
(defmacro /poly (c8 si) `(/nodes- (+ ,c8 ,(+ 4 si))))
(defmacro /ref (c8 si) `(/nodes- (+ ,c8 ,(+ 4 si))))
(defmacro /num (c8 si) `(/nodes- (+ ,c8 ,si)))

(defmacro simd4/raycast (bvh &rest rest)
  (declare (symbol bvh)) "raycast bvh from org alon ll using simd."
  `(let ((nodes (gmsh/bvh::bvh-simd-nodes ,bvh))
        (mima (gmsh/bvh::bvh-simd-mima ,bvh))
        (polyfx (gmsh/bvh::bvh-polyfx ,bvh)))
     (veq:mvc #'simd4/raycast* nodes mima polyfx ,@rest)))

(defmacro simd4/simple-raycast (bvh &rest rest)
  (declare (symbol bvh)) "raycast bvh from org alon ll using simd.
returns 0.0 if it hits anything; 1.0 otherwise"
  `(let ((nodes (gmsh/bvh::bvh-simd-nodes ,bvh))
        (mima (gmsh/bvh::bvh-simd-mima ,bvh))
        (polyfx (gmsh/bvh::bvh-polyfx ,bvh)))
     (veq:mvc #'simd4/simple-raycast* nodes mima polyfx ,@rest)))

; ref and nxt is same field
(defmacro /subnode (c8 si &rest rest)
 `(let ((ref (/ref ,c8 ,si))
        (num (/num ,c8 ,si)))
    (declare (veq:pn ref num))
    (loop repeat num ; TODO
          for i3 of-type veq:pn from ref by 9
          for s of-type veq:ff = (polyx polyfx i3 ,@rest)
          if (and (< #.*eps* s 1.0) (< s hs))
          do (setf hs s hi i3))
    (when (and (> ref 0) (< num 1)) (push-rt (the veq:pn ref)))))

(defvar *all* 0)

(veq:fvdef simd4/raycast* (nodes mima polyfx (:va 3 org ll))
  (declare #.*opt1* (veq:ivec nodes) (veq:fvec mima polyfx) (veq:ff org ll))
  "raycast from org along ll. returns 0.0 if something is hit, 1.0 otherwise.
bvh must be constructed with mode :bvh4-simd.
stck must be created with (gmsh:make-fast-stack), which returns an array of
type gmsh:stack."
  (veq:xlet ((f3!inv (-eps-div ll))
             (hs 900000.0) (hi #.(1- (expt 2 32))))
    (declare (veq:ff hs) (veq:pn hi) )
    (gmsh::with-fast-stack (rt :stack *stck*)
      (nil-rt)
      (push-rt 0)
      (loop while (con-rt)
             for c of-type veq:pn = (pop-rt)
             for nn of-type veq:pn =
                  (gmsh/bvh::m@simd/bvh4/bbox-test gmsh/bvh::mima
                    (the veq:pn (ash c #.(simd4/bbox-leap)))
                   inv org)
             for c8 of-type veq:pn = (ash c 3) ; * 8
             do (when (oddp (ash nn  0)) (/subnode c8 0 org ll))
                (when (oddp (ash nn -1)) (/subnode c8 1 org ll))
                (when (oddp (ash nn -2)) (/subnode c8 2 org ll))
                (when (oddp (ash nn -3)) (/subnode c8 3 org ll))
                ))
          (values (if (< hi #.(1- (expt 2 32))) (the veq:pn (floor hi #.+polyleap+)) -1)
                  hs)))

(defmacro /simple-subnode (c8 si &rest rest)
  `(let ((ref (/ref ,c8 ,si)) (num (/num ,c8 ,si)))
     (declare (veq:pn ref num))
     (loop repeat num ; TODO
           for i3 of-type veq:pn from ref by 9
           if (< #.*eps* (polyx polyfx i3 ,@rest) #. (- 1.0 *eps*))
           do (return-from simd4/simple-raycast* 0.0))
     (when (and (> ref 0) (< num 1)) (push-rt (the veq:pn ref)))))

(veq:fvdef simd4/simple-raycast* (nodes mima polyfx (:va 3 org ll))
  (declare #.*opt1* (veq:ivec nodes) (veq:fvec mima polyfx) (veq:ff org ll))
  "raycast from org along ll. returns 0.0 if something is hit, 1.0 otherwise.
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
                  (the veq:pn (ash c #.(simd4/bbox-leap)))
                  inv org)
            for c8 of-type veq:pn = (ash c 3)
            do (when (oddp (the veq:pn (ash nn  0))) (/simple-subnode c8 0 org ll))
               (when (oddp (the veq:pn (ash nn -1))) (/simple-subnode c8 1 org ll))
               (when (oddp (the veq:pn (ash nn -2))) (/simple-subnode c8 2 org ll))
               (when (oddp (the veq:pn (ash nn -3))) (/simple-subnode c8 3 org ll))))
    1.0))

