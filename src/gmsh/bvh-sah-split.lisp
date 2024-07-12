(in-package :gmsh/bvh)

(declaim (inline -objs-list-bbox -axissort -longaxis merge-mima))

(veq:fvdef -objs-list-bbox (objs) (declare #.*opt* (list objs))
  (loop for o* of-type list in objs for o of-type veq:fvec = (second o*)
        minimizing (veq:f$ o 0) into xmi of-type veq:ff
        maximizing (veq:f$ o 1) into xma of-type veq:ff
        minimizing (veq:f$ o 2) into ymi of-type veq:ff
        maximizing (veq:f$ o 3) into yma of-type veq:ff
        minimizing (veq:f$ o 4) into zmi of-type veq:ff
        maximizing (veq:f$ o 5) into zma of-type veq:ff
        finally (return (values xmi ymi zmi xma yma zma))))

(veq:fvdef get-objs-ax-centroid-mima (objs ax) (declare #.*opt* (list objs) (veq:pn ax))
  (loop for (poly bbox . rest) in objs
        for c of-type veq:ff = (* 0.5 (f2_@+ (veq:f2$ bbox ax)))
        minimizing c into mi maximizing c into ma
        finally (return (values mi ma))))

(macrolet (($ (b f) `(,(gmsh::symb :sah-bucket- f) ,b))
           ($! (b f v) `(setf ($ ,b ,f) ,v))
           ($n (b) `(length ($ ,b :inds))))

(veq:fvdef sah-do-split-axis (objs buckets imin &aux (leftinds (hset:make)))
  (declare #.*opt* (list objs) (sah-buckets buckets) (veq:in imin) (hash-table leftinds))
  (loop for bi from 0 below (max 0 (- (length buckets) 1))
        for b of-type sah-bucket = (aref buckets bi)
        if (and (<= bi imin) (> ($n b) 0))
        do (loop for k of-type veq:pn across ($ b :inds) do (hset:add leftinds k)))
  (loop for k from 0 for o in objs
        if (hset:mem leftinds k) collect o into l else collect o into r
        finally (return (values l r))))

(veq:fvdef bb-area (b) (declare #.*opt* (veq:fvec b))
  (veq:xlet ((f!x (f2_@- (veq:$ b 1 0)))
             (f!y (f2_@- (veq:$ b 3 2)))
             (f!z (f2_@- (veq:$ b 5 4))))
    (max 0.01 (+ (abs (* x z)) (abs (* x y)) (abs (* y z))))))

(veq:fvdef sah-find-min-node (buckets &aux (imin 0) (mincost 9999999999f0))
  (declare #.*opt* (sah-buckets buckets) (veq:ff mincost) (veq:in imin))
  (labels ((costcnt (n) (max 2 n)))
    (loop repeat (- (length buckets) 1) for b across buckets
          for i of-type veq:in from 0
          for nodecost2 of-type veq:ff = (+ (* (costcnt ($ b :fwdcnt)) (bb-area ($ b :fwdbb)))
                                            (* (costcnt ($ b :bckcnt)) (bb-area ($ b :bckbb))))
          do (when (< nodecost2 mincost) (setf imin i mincost nodecost2))))
  (values buckets imin mincost))

(veq:fvdef merge-mima (curr new) (declare #.*opt* (veq:fvec curr new))
  (setf (veq:2$ curr 0) (veq:f2 (min (veq:$ curr 0) (veq:$ new 0))
                                (max (veq:$ curr 1) (veq:$ new 1)))
        (veq:2$ curr 1) (veq:f2 (min (veq:$ curr 2) (veq:$ new 2))
                                (max (veq:$ curr 3) (veq:$ new 3)))
        (veq:2$ curr 2) (veq:f2 (min (veq:$ curr 4) (veq:$ new 4))
                                (max (veq:$ curr 5) (veq:$ new 5)))))

; TODO: this is kinda messy
(veq:fvdef sah-estimate-axis (objs ax &rest rest &aux (nb 13))
  (declare #.*opt* (ignore rest) (list objs) (veq:pn ax nb))
  (veq:xlet ((buckets (init-sah-buckets nb))
             (f2!centmima (get-objs-ax-centroid-mima objs ax)))
    (declare (sah-buckets buckets))
    (labels ((update-surface-est (bi new-bb &aux (b (aref buckets bi)))
               (merge-mima ($ b :bb) new-bb))
             (push-obj (oi bi bb &aux (b (aref buckets bi)))
               (update-surface-est bi bb)
               (auxin:vextend oi ($ b :inds))))
      ; BUILD BUCKETS
      (loop with range of-type veq:ff = (abs (veq:ff (- (:vr centmima 1 0))))
            initially (if (< range *eps*) (error "bad range"))
            for (poly bbox . rest) in objs ; rest = normal , vv coordinates
            for oi from 0
            do (veq:xlet ((f2!bbmima (veq:f$ bbox (* 2 ax) (1+ (* 2 ax))))
                          (f!c (* 0.5 (f2_@+ bbmima)))
                          (p!bi (min (1- nb)
                                     (floor (* nb (/ (- c (:vr centmima 0)) range))))))
                  (push-obj oi bi bbox)))
      (loop with bb = (make-bb) with cnt of-type veq:pn = 0
            for i from 0 repeat nb for b = (aref buckets i)
            if (> ($n b) 0) do (incf cnt ($n b)) (merge-mima bb ($ b :bb))
            do ($! b :fwdcnt cnt) (merge-mima ($ b :fwdbb) bb))

      (loop with bb = (make-bb) with cnt of-type veq:pn = 0
            for i from (1- nb) downto 1 repeat (- nb 1)
            for b = (aref buckets i) for b- = (aref buckets (- i 1))
            if (> ($n b) 0) do (incf cnt ($n b)) (merge-mima bb ($ b :bb))
            do ($! b- :bckcnt cnt) (merge-mima ($ b- :bckbb) bb))
      (sah-find-min-node buckets)))))

; TODO: this is very messy and probably inefficent
; TODO: fix buckets init to nil
; TODO: fix very general handler-case
(veq:fvdef sah-split-by-best-axis (objs nb &aux (imin -1) (axis -1)
                                                (mincost 9999999999f0) buckets)
  (declare #.*opt* (veq:in imin axis) (veq:ff mincost))
  (loop for ax of-type veq:pn from 0 repeat 3
        do (handler-case
             (veq:mvb (buckets* imin* mincost*) (sah-estimate-axis objs ax :nb nb)
                (declare (sah-buckets buckets*)
                         (veq:in imin*) (veq:ff mincost*))
                (when (< mincost* mincost)
                      (setf mincost mincost* buckets buckets* imin imin* axis ax)))
             (simple-error (e) (declare (ignorable e))))) ; ignores bad range error

  (when (< imin 0) (error "tiny box"))
  ; TODO: make this an alternative?
  ; (let ((ax (-longaxis objs)))
  ;   (veq:mvb (buckets* imin*) (sah-estimate-axis objs ax :nb nb)
  ;            (setf imin imin* buckets buckets*)))
  (handler-case (veq:~ (sah-do-split-axis objs buckets imin) axis)
                (error (e) (error "split err: ~a" e))))

; TODO: fix mix of mi mi mi, ma ma ma, mi ma, mi ma mappings
(veq:fvdef -longaxis (objs) (declare #.*opt* (list objs))
  (veq:mvb (xmi ymi zmi xma yma zma)
    (-objs-list-bbox objs)
    (declare (veq:ff xmi xma ymi yma zmi zma))
    (veq:xlet ((f3!diff (f3.@abs (- xma xmi) (- yma ymi) (- zma zmi))))
      (apply #'values (cdr (first (sort (list (list (:vr diff 0) 0 xmi xma)
                                              (list (:vr diff 1) 1 ymi yma)
                                              (list (:vr diff 2) 2 zmi zma))
                                        #'> :key #'first)))))))

(defun -axissort (objs &key (ax (-longaxis objs)) &aux (ax (* 2 ax)))
  (declare #.*opt* (veq:pn ax) (list objs) )
  (labels ((ak (a ind) (declare (list a) (veq:pn ind))
                       (aref (the veq:fvec (second a)) ind))
           (bbsrt (a b &aux (ami (ak a ax)) (bmi (ak b ax))
                            (ama (ak a (1+ ax))) (bma (ak b (1+ ax))))
             (declare (list a b) (veq:ff ami ama bmi bma))
             (or (and (< (* 0.5 (+ ami ama)) (* 0.5 (+ bmi bma)))
                      (< ami bmi))
                 (and (< ami bma) (< ami bmi))
                 (< ama bmi))))
     (sort (the list (copy-tree objs)) #'bbsrt))) ; do we need to copy anymore?

