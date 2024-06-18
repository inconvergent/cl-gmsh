
(veq:fvdef* do-al-sample (bvh rc (:va 3 mrgb d hn hp sm))
  (declare (optimize speed) (function rc) (veq:ff mrgb d hn hp sm))
  (veq:xlet ((f!ao (veq:f 0f0))
             (f3!light (veq:f3rep 0f0)))
   (loop repeat 30
         do (let* ((hit (veq:mvc rc hp
                          (veq:f3+ sm (rnd:3on-sphere *dstlim*))))
                   (dst (veq:fclamp (gmsh::bvhres-s hit))))
              (declare (gmsh::bvhres hit) (veq:ff dst))
              (veq:mvb (flag (:va 3 rgb)) (hitmat bvh hit)
                (declare (symbol flag) (veq:ff rgb))
                ; glass contribute slightly?
                (when (eq flag :light)
                      (veq:f3vset (light) (veq:f3from light
                                            rgb (if (> (gmsh::bvhres-i hit) -1)
                                                    1f0 0f0))))
                (when (and (eq flag :al) (> (gmsh::bvhres-i hit) -1))
                  (incf ao 1f0)))))
   (veq:f3from (veq:f3scale light #.(/ 3f0 30f0))
               mrgb (* (expt (abs (- 1f0 (/ ao 30f0))) 1.1)
                       (- 1f0 (* (rnd:rndrng 0.25 0.35)
                                 (expt (abs (veq:f3. hn d)) 1.7)))))))

(do-re (depth (:va 3 rgb pt d hn))
  (declare (fixnum depth) (veq:ff rgb pt d hn))
  (veq:mvb ((:va 3 new-dir)) (reflect d hn) ;(refraf st d hn)
    (declare  (veq:ff new-dir))
    (let* ((ao (do-ao pt d hn))
          (c (* ao 0.2 (expt (abs (veq:f3. hn d)) 1.7))))
      (veq:f3* rgb (veq:f3from
                  (veq:f3 c c c) ; do-ao
                  ; (veq:f3scale (do-ao rc pt d hn) 0.2)
                  (render (1+ depth) (veq:f3from pt new-dir 0.01) new-dir)
                  0.85)))))

(do-al (rc (:va 3 rgb pt d hn))
  (declare (function rc) (veq:ff rgb pt d hn))
  (veq:f3let ((hp (veq:f3from pt hn 0.2)))
    (do-al-sample bvh rc rgb d hn hp (veq:f3from hp hn *dstlim*))))


(veq:fvdef make-raycaster (bvh)
  (declare  #.*opt1* (bvh bvh))
  "returns a a function, fx, for raycasting (fx from line). the call returns an
(values i s) i is the index (in the bvh) of the closest polygon, or -1,
and s is the lerp along line"
  ; NOTE: that bvh-node (internally) must be the 4 stride version see
  ; repack-nodes-stackless.
  (macrolet
    ((nodes- (slot) `(aref nodes (the veq:in (+ ,slot ni))))
     (for-leaves- ((hs hi i3) &body body)
       `(loop repeat (nodes- 0)
              for i of-type veq:in from (nodes- 1)
              for ,i3 of-type veq:in = (the veq:in (* 9 i))
              if (> i -1) do (let ((s ,@body)) (declare (veq:ff s))
                               (when (and (< #.*eps* s) (< s ,hs)) ; HIT!
                                     (setf ,hs s ,hi i))))))
    (let ((mima (bvh-mima bvh)) (polyfx (bvh-polyfx bvh))
          (nodes (bvh-nodes bvh)))
      (declare (veq:fvec mima polyfx) (veq:ivec nodes))
      (labels
        ((raycast ((:va 3 org ll))
          (declare #.*opt1* (veq:ff org ll))
          (veq:xlet ((f3!inv (-eps-div ll)) (i!hi -1) (f!hs 900000f0))
            (loop with curr of-type veq:in = 0
                  while (> curr -1)
                  for ni of-type veq:in = (* curr 4)
                  if (-bbox-test mima (the veq:in (* 6 curr)) inv org)
                  do (for-leaves- (hs hi i3) (-polyx polyfx i3 org ll))
                     (if (plusp (nodes- 0)) (setf curr (nodes- 3))
                                            (setf curr (nodes- 2)))
                  else do (setf curr (nodes- 3)))
            (values hi hs))))
        #'raycast))))

(veq:fvdef make-simple-raycaster (bvh)
  (declare #.*opt1* (bvh bvh))
  "returns a a function, fx, for raycasting (fx from line).
the call returns 0f0 if it hit something, and 1f0 if it did not"
  ; NOTE: that bvh-node (internally) must be the 4 stride version see
  ; repack-nodes-stackless.
  (macrolet
    ((nodes- (slot) `(aref nodes (the veq:in (+ ,slot ni))))
     (for-leaves- ((i3) &body body)
       `(loop repeat (nodes- 0)
              for ,i3 of-type veq:in from (* 9 (nodes- 1)) by 9
              if (> ,i3 -1)
              do (let ((s ,@body))
                   (declare (veq:ff s))
                   (when (< #.*eps* s #.(- 1f0 *eps*))
                         (return-from raycast-short 0f0)))))) ; HIT!
    (let ((mima (bvh-mima bvh)) (polyfx (bvh-polyfx bvh))
          (nodes (bvh-nodes bvh)))
      (declare (veq:fvec mima polyfx) (veq:ivec nodes))
      (labels
        ((raycast-short ((:va 3 org ll))
          (declare #.*opt1* (veq:ff org ll))
          (veq:f3let ((inv (-eps-div ll)))
            (loop with curr of-type veq:in = 0
                  while (> curr -1)
                  for ni of-type veq:pn = (* curr 4)
                  if (-bbox-test mima (the veq:pn (* 6 curr)) inv org)
                  do (for-leaves- (i3) (-polyx polyfx i3 org ll))
                     (if (plusp (nodes- 0)) (setf curr (nodes- 3))
                                            (setf curr (nodes- 2)))
                  else do (setf curr (nodes- 3)))
            1f0))) ; MISS!
        #'raycast-short))))
