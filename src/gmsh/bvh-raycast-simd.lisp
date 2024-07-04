(in-package :gmsh/bvh)

(defvar *simd/cnt-pol* 0)

(defmacro /nodes- (i) `(aref nodes ,i))
(defmacro /poly (c8 si) `(/nodes- (+ ,c8 ,(+ 4 si))))
(defmacro /ref (c8 si) `(/nodes- (+ ,c8 ,(+ 4 si))))
(defmacro /num (c8 si) `(/nodes- (+ ,c8 ,si)))


(defmacro simd/subnode (c8 si &rest rest)
 `(when (oddp (ash nn ,(- si)))
        (veq:xlet ((p!num (/num ,c8 ,si))
                   (p!ref (/ref ,c8 ,si)))
          (loop repeat num for i3 from ref by 9
                for s of-type veq:ff = (polyx polyfx i3 ,@rest)
                if (and (< #.*eps* s 1.0) (< s hs))
                do (setf hs s hi i3))
          (when (and (< 0 ref) (zerop num)) (rec ref)))))

(veq:fvdef simd4/raycast (bvh (:va 3 org ll))
  (declare #.*opt1* (bvh bvh) (veq:ff org ll))
  "raycast from org along ll. returns 0.0 if something is hit, 1.0 otherwise.
bvh must be constructed with mode :bvh4-simd."
  (veq:xlet ((nodes (bvh-simd-nodes bvh))
             (mima (bvh-simd-mima bvh))
             (polyfx (bvh-polyfx bvh))
             (f3!inv (-eps-div ll))
             (hs 900000.0) (hi #.(1- (expt 2 32))))
    (declare (veq:fvec mima polyfx) (veq:pvec nodes)
             (veq:ff hs) (veq:pn hi))
    (labels ((rec (c) (declare (veq:pn c))
               (let ((nn (simd/bvh4/bbox-test mima
                           (ash c #.(simd4/bbox-leap)) inv org))
                     (c8 (ash c 3)))
                (declare (veq:pn nn c8))
                (simd/subnode c8 0 org ll)
                (simd/subnode c8 1 org ll)
                (simd/subnode c8 2 org ll)
                (simd/subnode c8 3 org ll))))
      (rec 0))
    (values (if (< hi #.(1- (expt 2 32))) (floor hi #.+polyleap+) -1)
            hs)))

(defmacro simd/subnode-rec (c8 si)
  `(when (and (oddp (ash nn ,(- si))) (> (/ref ,c8 ,si) 0) (zerop (/num ,c8 ,si)))
         (rec (/ref ,c8 ,si))))

(defmacro simd/simple-subnode (c8 si &rest rest)
  `(when (oddp (ash nn ,(the fixnum (- si))))
       (loop repeat (/num ,c8 ,si)
             ; for x = (incf *simd/cnt-pol*)
             for i3 from (/ref ,c8 ,si) by 9
             if (< #.*eps* (polyx polyfx i3 ,@rest) 1.0)
             do (return-from simd4/simple-raycast 0.0))))

; (defmacro simd/simple-subnode (c8 si &rest rest)
;  `(when (oddp (ash nn ,(- si)))
;         (veq:xlet ((p!num (/num ,c8 ,si))
;                    (p!ref (/ref ,c8 ,si)))
;           (loop repeat num for i3 from ref by 9
;                 ; for x = (incf *simd/cnt-pol*)
;                 for s of-type veq:ff = (polyx polyfx i3 ,@rest)
;                 if (< #.*eps* (polyx polyfx i3 ,@rest) 1.0)
;                 do (return-from simd4/simple-raycast 0.0))
;           (when (and (< 0 ref) (zerop num)) (rec ref)))))

(veq:fvdef simd4/simple-raycast (bvh (:va 3 org ll))
  (declare #.*opt1* (bvh bvh) (veq:ff org ll))
  "raycast from org along ll. returns 0.0 if something is hit, 1.0 otherwise.
bvh must be constructed with mode :bvh4-simd."
  (veq:xlet ((nodes (bvh-simd-nodes bvh))
             (mima (bvh-simd-mima bvh))
             (polyfx (bvh-polyfx bvh))
             (f3!inv (-eps-div ll)))
    (declare (veq:fvec mima polyfx) (veq:pvec nodes))
    (labels ((rec (c) (declare (veq:pn c))
               (let ((nn (simd/bvh4/bbox-test mima
                           (ash c #.(simd4/bbox-leap)) inv org))
                     (c8 (ash c 3)))
                  (declare (veq:pn nn c8))
                  (simd/simple-subnode c8 1 org ll) (simd/subnode-rec c8 1)
                  (simd/simple-subnode c8 0 org ll) (simd/subnode-rec c8 0)
                  (simd/simple-subnode c8 3 org ll) (simd/subnode-rec c8 3)
                  (simd/simple-subnode c8 2 org ll) (simd/subnode-rec c8 2)
                  )))
      (rec 0)
      1.0)))

