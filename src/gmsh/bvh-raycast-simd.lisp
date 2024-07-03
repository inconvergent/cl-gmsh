(in-package :gmsh/bvh)

(defmacro /nodes- (i) `(aref nodes ,i))
(defmacro /poly (c8 si) `(/nodes- (+ ,c8 ,(+ 4 si))))
(defmacro /ref (c8 si) `(/nodes- (+ ,c8 ,(+ 4 si))))
(defmacro /num (c8 si) `(/nodes- (+ ,c8 ,si)))

(defmacro simd4/raycast (bvh &rest rest)
  (declare (symbol bvh)) "raycast bvh from org alon ll using simd."
  `(veq:mvc #'simd4/raycast*
     (bvh-simd-nodes ,bvh) (bvh-simd-mima ,bvh) (bvh-polyfx ,bvh) ,@rest))

(defmacro simd4/simple-raycast (bvh &rest rest)
  (declare (symbol bvh)) "raycast bvh from org alon ll using simd.
returns 0.0 if it hits anything; 1.0 otherwise"
  `(veq:mvc #'simd4/simple-raycast*
     (bvh-simd-nodes ,bvh) (bvh-simd-mima ,bvh) (bvh-polyfx ,bvh) ,@rest))

(defmacro /subnode (c8 si &rest rest)
 `(veq:xlet ((p!ref (/ref ,c8 ,si)) (p!num (/num ,c8 ,si)))
    (loop repeat num for i3 from ref by 9
          for s of-type veq:ff = (polyx polyfx i3 ,@rest)
          if (and (< #.*eps* s 1.0) (< s hs))
          do (setf hs s hi i3))
    (and (> ref 0) (< num 1) (rec ref))))

(veq:fvdef simd4/raycast* (nodes mima polyfx (:va 3 org ll))
  (declare #.*opt1* (veq:ivec nodes) (veq:fvec mima polyfx) (veq:ff org ll))
  "raycast from org along ll. returns 0.0 if something is hit, 1.0 otherwise.
bvh must be constructed with mode :bvh4-simd."
  (veq:xlet ((f3!inv (-eps-div ll))
             (hs 900000.0) (hi #.(1- (expt 2 32))))
    (declare (veq:ff hs) (veq:pn hi)) ; xlet misses these in /subnode
    (labels ((rec (c) (declare (veq:pn c))
               (veq:xlet ((p!nn (simd/bvh4/bbox-test mima
                                  (ash c #.(simd4/bbox-leap)) inv org))
                          (p!c8 (ash c 3)))
                 (when (oddp (ash nn  0)) (/subnode c8 0 org ll))
                 (when (oddp (ash nn -1)) (/subnode c8 1 org ll))
                 (when (oddp (ash nn -2)) (/subnode c8 2 org ll))
                 (when (oddp (ash nn -3)) (/subnode c8 3 org ll)))))
      (rec 0))
    (values (if (< hi #.(1- (expt 2 32))) (floor hi #.+polyleap+) -1)
            hs)))

(defmacro /simple-subnode (c8 si &rest rest)
  `(veq:xlet ((p!ref (/ref ,c8 ,si)) (p!num (/num ,c8 ,si)))
     (loop repeat num ; TODO
           for i3 of-type veq:pn from ref by 9
           if (< #.*eps* (polyx polyfx i3 ,@rest) #. (- 1.0 *eps*))
           do (return-from simd4/simple-raycast* 0.0))
     (and (> ref 0) (< num 1) (rec ref))))

(veq:fvdef simd4/simple-raycast* (nodes mima polyfx (:va 3 org ll))
  (declare #.*opt1* (veq:ivec nodes) (veq:fvec mima polyfx) (veq:ff org ll))
  "raycast from org along ll. returns 0.0 if something is hit, 1.0 otherwise.
bvh must be constructed with mode :bvh4-simd."
  (veq:xlet ((f3!inv (-eps-div ll)))
    (labels ((rec (c) (declare (veq:pn c))
               (veq:xlet ((p!nn (simd/bvh4/bbox-test mima
                                  (ash c #.(simd4/bbox-leap)) inv org))
                          (p!c8 (ash c 3)))
                 (when (oddp (ash nn  0)) (/simple-subnode c8 0 org ll))
                 (when (oddp (ash nn -1)) (/simple-subnode c8 1 org ll))
                 (when (oddp (ash nn -2)) (/simple-subnode c8 2 org ll))
                 (when (oddp (ash nn -3)) (/simple-subnode c8 3 org ll)))))
      (rec 0))
    1.0))

