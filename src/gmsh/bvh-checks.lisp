(in-package :gmsh/bvh)

(declaim (inline polyx bvh2/bbox-test int/bbox-test simd/bvh4/bbox-test))

(defmacro 3cross (ax ay az bx by bz) `(values (- (* ,ay ,bz) (* ,az ,by)) (- (* ,az ,bx) (* ,ax ,bz)) (- (* ,ax ,by) (* ,ay ,bx))))
(defmacro 3dot (ax ay az bx by bz) `(+ (* ,ax ,bx) (* ,ay ,by) (* ,az ,bz)))

; https://en.wikipedia.org/wiki/M%C3%B6ller%E2%80%93Trumbore_intersection_algorithm
(veq:fvdef polyx (fx i (:va 3 org ll))
  (declare #.*opt1* (veq:fvec fx) (veq:ff org ll) (veq:pn i))
  (macrolet ((i (leap) `(aref fx (the veq:pn (+ i ,leap)))))
    (veq:xlet (;(f3!i012 (values (i 0) (i 1) (i 2)))
               (f3!h (3cross ll (i 0) (i 1) (i 2)))  ;0
               (f!a (3dot (i 3) (i 4) (i 5) h))) ; 1
      ; e1,e2 are parallel, miss!
      (when (< (abs a) #.(the veq:ff #.*eps*)) (return-from polyx -1f0))
      (veq:xlet ((f!f (/ a))
                 (f3!s (f3!@- org (i 6) (i 7) (i 8))) ; 2
                 (f!u (the veq:ff (* f (3dot s h)))))
        (when (or (> u 1f0) (< u 0f0)) (return-from polyx -1f0)) ; miss!
        (veq:xlet ((f3!q (3cross s (i 3) (i 4) (i 5))) ; 1
                   (f!v (the veq:ff (* f (3dot ll q)))))

          (when (or (< v 0f0) (> (the veq:ff (+ u v)) 1f0))
               (return-from polyx -1f0))  ; miss!
          ; technically we should do this:
          ; (max (the veq:ff (* f (veq:3dot (veq:f3$ fx 2) q))) ; hit!
          ;      (the veq:ff #.(- *eps*))))
          ; but the final check is performed in the raycaster,
          ; so we do this instead:
          (the veq:ff (* f (the veq:ff (3dot (i 0) (i 1) (i 2) q))))))))) ; hit/miss! ; 2

; https://people.csail.mit.edu/amy/papers/box-jgt.pdf
; FOR BVH2 bvh-node-vec
(veq:fvdef bvh2/bbox-test (mima (:va 3 inv org)) ;MIMA MUST HAVE LENGTH 6, be part of NODE
  (declare #.*opt1* (veq:ff inv org) (veq:fvec mima))
  (macrolet ((i (leap) `(aref mima (the veq:pn ,leap)))
             (-bound ((ao invl mi ma tmin tmax) &body body)
                `(if (<= 0f0 ,invl)
                  (let ((,tmin #1=(the veq:ff (* ,invl (the veq:ff (- ,mi ,ao)))))
                        (,tmax #2=(the veq:ff (* ,invl (the veq:ff (- ,ma ,ao))))))
                    #3=(declare (veq:ff ,tmin ,tmax))
                    #4=(progn ,@body))
                  (let ((,tmin #2#) (,tmax #1#)) #3# #4#))))
    (-bound ((:vr org 0) (:vr inv 0) (i 0) (i 3) tmin tmax)
      (-bound ((:vr org 1) (:vr inv 1) (i 1) (i 4) tymin tymax)
        (unless (or (> tmin tymax) (> tymin tmax))
          (when (> tymin tmin) (setf tmin tymin))
          (when (< tymax tmax) (setf tmax tymax))
          (-bound ((:vr org 2) (:vr inv 2) (i 2) (i 5) tzmin tzmax)
            (and (<= tmin tzmax) (<= tmin 1f0)
                 (<= tzmin 1f0) (<= tzmin tmax)
                 (>= tmax 0f0) (>= tzmax 0f0))))))))
; old body, slightly slower:
; (unless (or (> tmin tzmax) (> tzmin tmax))
;   (when (> tzmin tmin) (setf tmin tzmin))
;   (when (< tzmax tmax) (setf tmax tzmax))
;   (and (< tmin 1f0) (> tmax 0f0)))

; mima layout xmi xma ymi xma ... pad
(veq:fvdef int/bbox-test (mima i (:va 3 inv org))
  (declare #.*opt1* (veq:ff inv org) (veq:pn i) (veq:fvec mima))
  (macrolet (($ (leap) `(aref mima (the veq:pn (+ i ,leap))))
             (-bound ((ao invl mi ma tmin tmax) &body body)
                `(let ((miao (- ,mi ,ao)) (maao (- ,ma ,ao)))
                   (declare (veq:ff miao maao))
                   (if (> ,invl 0f0)
                       (let ((,tmin #1=(the veq:ff (* ,invl miao)))
                             (,tmax #2=(the veq:ff (* ,invl maao))))
                         #3=(declare (veq:ff ,tmin ,tmax miao maao))
                         #4=(progn ,@body))
                       (let* ((,tmin #2#) (,tmax #1#)) #3# #4#)))))
    (let (($0 ($ 0)) ($1 ($ 1)) ($2 ($ 2)) ($3 ($ 3)))
      (-bound ((:vr org 0) (:vr inv 0) $0 $1 tmin tmax)
        (-bound ((:vr org 1) (:vr inv 1) $2 $3 tymin tymax)
          (unless (or (> tmin tymax) (> tymin tmax))
            (when (> tymin tmin) (setf tmin tymin))
            (when (< tymax tmax) (setf tmax tymax))
            ; (setf tmin (max tymin tmin) tmax (min tymax tmax))
            (-bound ((:vr org 2) (:vr inv 2) ($ 4) ($ 5) tzmin tzmax)
              (and (<= tmin tzmax) (<= tmin 1f0) (>= tmax 0f0)
                   (<= tzmin 1f0) (<= tzmin tmax) (>= tzmax 0f0)))))))))

; ----- SIMD ---------------------------------------------------------

; this is slower, maybe we can improve it?
; `(sb-simd-fma:f32.4-fnmadd ,iv ($ ,mx) ; 43.2s ; 35.6 sans bcast
;     (sb-simd-avx2:f32.4* ,o ,iv))

(macrolet (($ (k) `(sb-simd-avx2:f32.4-aref mima (the veq:pn (+ i ,k))))
           (.fmadd (mx o iv) `(sb-simd-avx2:f32.4* ,iv ;         35.2
                                (sb-simd-avx2:f32.4- ($ ,mx) ,o)))
           (.min (&rest rest) `(sb-simd-avx2:f32.4-min ,@rest))
           (.max (&rest rest) `(sb-simd-avx2:f32.4-max ,@rest))
           (.<= (&rest rest) `(sb-simd-avx2:f32.4<= ,@rest))
           (.and (&rest rest) `(sb-simd-avx2:u32.4-and ,@rest))
           (.or (&rest rest) `(sb-simd-avx2:u32.4-or ,@rest))
           (do-dim (8d iorg* inv*)
             `(let* ((iv (sb-simd-avx2:f32.4 ,inv*))
                     (o (sb-simd-avx2:f32.4 ,iorg*))
                     (a (.fmadd ,8d o iv))
                     (b (.fmadd ,(+ 8d 4) o iv)))
                (setf lo (.max (.min a b) lo)
                      hi (.min (.max a b) hi)))))
  (veq:fvdef simd/bvh4/bbox-test ( mima i (:va 3 inv iorg))
    (declare #.*opt1* (veq:pn i) (sb-simd-avx2:f32vec mima) (veq:ff inv iorg))
      (let ((lo (sb-simd-avx2:f32.4 -9000f0)) (hi (sb-simd-avx2:f32.4 9000f0)))
        (declare (sb-simd-avx2:f32.4 lo hi))
        (do-dim 0  (:vr iorg 0) (:vr inv 0))
        (do-dim 8  (:vr iorg 1) (:vr inv 1))
        (do-dim 16 (:vr iorg 2) (:vr inv 2))
        (veq:mvc #'logior
          (sb-simd-avx2:u32.4-values
            (.and (.and (.or (.<= 0f0 hi) (.<= lo 1f0)) (.<= lo hi))
                  (sb-simd-avx2:make-u32.4 1 2 4 8)))))))

