(in-package :gmsh/bvh)

(declaim (inline -objs-list-bbox -axissort -longaxis
                 get-objs-ax-centroid-mima sah-do-split-axis
                 sah-find-min-node
                 sah-reset-buckets! sah-estimate-axis))

(veq:fvdef -objs-list-bbox (objs) (declare #.*opt1* (list objs)) ; TODO: rewrite
  (loop for o* of-type list in objs for o of-type bbox = (second o*)
        minimizing (veq:f$ o 0) into xmi of-type veq:ff
        maximizing (veq:f$ o 1) into xma of-type veq:ff
        minimizing (veq:f$ o 2) into ymi of-type veq:ff
        maximizing (veq:f$ o 3) into yma of-type veq:ff
        minimizing (veq:f$ o 4) into zmi of-type veq:ff
        maximizing (veq:f$ o 5) into zma of-type veq:ff
        finally (return (values xmi ymi zmi xma yma zma))))

(veq:fvdef get-objs-ax-centroid-mima (objs ax) (declare #.*opt1* (list objs) (veq:pn ax))
  (veq:xlet ((f!mi *bvhhi*) (f!ma *bvhlo*))
    (loop for (poly bbox . rest) in objs
          for c of-type veq:ff = (* 0.5 (f2_@+ (veq:f2$ bbox ax)))
          do (when (< c mi) (setf mi c))
             (when (> c ma) (setf ma c)))
    (values mi ma)))

(veq:fvdef sah-do-split-axis (objs buckets nb ax imin
                             &aux (leftinds (hset:make :size 512)))
  (declare #.*opt1* (list objs) (sah-buckets buckets)
                    (veq:pn nb ax imin) (hash-table leftinds))
  (loop for bi of-type veq:pn from (* nb ax) repeat (- nb 1)
        for bk of-type veq:pn from 0
        while (<= bk imin)
        for b of-type sah-bucket = (aref buckets bi)
        ; if (> (bb$n b) 0)
        do (loop for k of-type veq:pn in (bb$ b :inds) do (hset:add leftinds k)))
  (loop for k from 0 for o in objs
        if (hset:mem leftinds k) collect o into l else collect o into r
        finally (return (values l r))))

(veq:fvdef sah-find-min-node (buckets nb ax &aux (imin 0) (mincost *bvhhi*))
  (declare #.*opt1* (sah-buckets buckets) (veq:ff mincost) (veq:pn imin) (veq:pn nb ax))
  (loop for i of-type veq:pn from 0 repeat (- nb 1)
        for bi from (* ax nb) for b = (aref buckets bi)
        for cost of-type veq:ff = (+ (*  (bb$ b :fwdcnt) (bb-area (bb$ b :fwdbb)))
                                     (*  (bb$ b :bckcnt) (bb-area (bb$ b :bckbb))))
        if (< cost mincost) do (setf imin i mincost cost))
  (values imin mincost))


; TODO: this is messy
(veq:fvdef sah-estimate-axis (buckets nb bb objs ax)
  (declare #.*opt1* (sah-buckets buckets) (list objs)
                    (veq:pn ax nb) (bbox bb))
  (macrolet ((bix (bi) `(aref buckets (+ axnb (the veq:pn ,bi)))))
    (veq:xlet ((p!axx (* 2 ax)) (axnb (* ax nb)) ; axnb is in macrolet!
               (f2!centmima (get-objs-ax-centroid-mima objs ax))
               (f!range (abs (- (:vr centmima 1 0)))))
      (declare (veq:pn axnb))
      (when (< range *eps*) (error "bad range"))
      (loop for o of-type list in objs for bbox of-type bbox = (second o) ; add to buckets
            for b of-type sah-bucket =
                (bix (min (floor (* nb (/ (- (* 0.5 (+ (aref bbox axx)
                                                       (aref bbox (1+ axx))))
                                             (:vr centmima 0))
                                          range)))
                          (1- nb)))
            for oi of-type veq:pn from 0
            do (push oi (bb$ b :inds))
               (incf (bb$ b :num-inds))
               (merge-bb! (bb$ b :bb) bbox))
      (reset-bb! bb)
      (loop with cnt of-type veq:pn = 0 ; sum left to right
            for i from 0 repeat nb for b = (bix i)
            for n of-type veq:pn = (bb$n b)
            if (> n 0) do (incf cnt n) (merge-bb! bb (bb$ b :bb))
            do (bb$! b :fwdcnt cnt) (set-bb! (bb$ b :fwdbb) bb))
      (reset-bb! bb)
      (loop with cnt of-type veq:pn = 0 ; sum right to left
            for i from (1- nb) downto 1 repeat (- nb 1)
            for b = (bix i) for b- = (bix (- i 1))
            for n of-type veq:pn = (bb$n b)
            if (> n 0) do (incf cnt n) (merge-bb! bb (bb$ b :bb))
            do (bb$! b- :bckcnt cnt) (set-bb! (bb$ b- :bckbb) bb))
      (sah-find-min-node buckets nb ax))))

(defun sah-reset-buckets! (buckets)
  (declare #.*opt1* (sah-buckets buckets))
  (loop for b of-type sah-bucket across buckets
        do (setf (sah-bucket-mi b) *bvhhi* (sah-bucket-ma b) *bvhlo*
                 (sah-bucket-fwdcnt b) 0    (sah-bucket-bckcnt b) 0
                 (sah-bucket-num-inds b) 0
                 (sah-bucket-inds b) (list))
            (reset-bb! (sah-bucket-fwdbb b))
            (reset-bb! (sah-bucket-bckbb b))
            (reset-bb! (sah-bucket-bb b))))
            ; (adjust-array (sah-bucket-inds b) (length (sah-bucket-inds b)) :fill-pointer 0)

; TODO: this is still messy. remove handler case?
; TODO: fix very general handler-case
(veq:fvdef sah-split-by-best-axis (buckets nb bb objs &aux (imin 0) (axis 10)
                                                        (mincost *bvhhi*))
  (declare #.*opt1* (sah-buckets buckets) (veq:pn imin axis nb)
                    (veq:ff mincost) (bbox bb))
  (sah-reset-buckets! buckets)
  (loop for ax of-type veq:pn from 0 repeat 3
        do (handler-case
             (veq:mvb (imin* mincost*) (sah-estimate-axis buckets nb bb objs ax)
                (declare (veq:pn imin*) (veq:ff mincost*))
                (when (< mincost* mincost)
                      (setf mincost mincost* imin imin* axis ax)))
             (simple-error (e) (declare (ignorable e))))) ; ignores bad range error

  (if (> axis 2) (progn (wrn :sah-narrow-box "narrow box with ~a objs" (length objs))
                        (values :narrow nil nil axis))
                 (veq:~ :sah (sah-do-split-axis objs buckets nb axis imin) axis)))

; TODO: fix mix of mi mi mi, ma ma ma, mi ma, mi ma mappings
(veq:fvdef -longaxis (objs) (declare #.*opt1* (list objs))
  (veq:mvb (xmi ymi zmi xma yma zma) (-objs-list-bbox objs)
    (declare (veq:ff xmi xma ymi yma zmi zma))
    (veq:xlet ((f3!diff (f3.@abs (- xma xmi) (- yma ymi) (- zma zmi))))
      (cond ((and (>= (:vr diff 0 1)) (>= (:vr diff 0 2))) (values 0 xmi xma))
            ((and (>= (:vr diff 1 0)) (>= (:vr diff 1 2))) (values 1 ymi yma))
            (t                                             (values 2 zmi zma))))))

; NOTE: this is one of the slowest parts, but it is not used frequently. try to remove?
(defun -axissort (objs ax &aux (ax (* 2 ax)))
  (declare #.*opt1* (veq:pn ax) (list objs))
  (labels ((ak (a ind) (declare (list a) (veq:pn ind))
                       (aref (the bbox (second a)) ind))
           (bbsrt (a b &aux (ami (ak a ax)) (bmi (ak b ax))
                            (ama (ak a (1+ ax))) (bma (ak b (1+ ax))))
             (declare (list a b) (veq:ff ami ama bmi bma))
             (or (and (< (* 0.5 (+ ami ama)) (* 0.5 (+ bmi bma)))
                      (< ami bmi))
                 (and (< ami bma) (< ami bmi))
                 (< ama bmi))))
     (sort objs #'bbsrt))) ; NOTE: there was a copy here

