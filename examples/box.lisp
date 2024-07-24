#!/usr/local/bin/sbcl --script

(load "/home/anders/quicklisp/setup.lisp") (require :sb-sprof)
(ql:quickload :auxin) (ql:quickload :gmsh)
(gmsh/xrend:init 60)
(defvar *window-size* 2000)

(veq:fvdef new-scene ()
  (declare (optimize speed (safety 1)))
  (let* ((sc (gmsh/scene:scene/make))
         (msh (gmsh/scene:@msh sc))
         (z 0f0) (w 70f0) (w* 70f0) (x 5f0)
         (ww (- w 25f0))
         (rr (- w (* 4 x)))
         (q (+ w x)))
    (macrolet ((box (&body body) `(gmsh/io:obj/load-model :cube :msh msh ,@body))
               (setmat (mat &body body) `(auxin:mvb (_ meta) ,@body
                                           (loop for p in (cdr (assoc :polys meta))
                                                 do (gmsh/scene:setmat sc p ',mat)))))
      (setmat (:cc :k) (box :xy (veq:f3$pt (- w)   z  z) :s (veq:f3$pt x q q)))    ; walls
      (setmat (:cc :k) (box :xy (veq:f3$pt    w    z  z) :s (veq:f3$pt x q q)))
      (setmat (:cc :k) (box :xy (veq:f3$pt    z    w  z) :s (veq:f3$pt q x q)))
      (setmat (:cc :k) (box :xy (veq:f3$pt    z (- w) z) :s (veq:f3$pt q x q)))

      (setmat (:cc :k) (box :xy (veq:f3$pt z z    w*)    :s (veq:f3$pt ww ww x)))  ; caps
      (setmat (:cc :k) (box :xy (veq:f3$pt z z (- w*))   :s (veq:f3$pt ww ww x)))

      (setmat (:ll :w) (box :s (veq:f3$pt rr rr rr)))

      (loop for lim in '(10f0) do (gmsh/scene:split-edges sc lim))
      (gmsh/scene:scene/new-canv sc :size *window-size*)
      (gmsh/cam:update! (gmsh/scene:@cam sc)
          :pos (veq:f3$pt 100f0 -100.0 625.0)
          :look (veq:f3$pt (rnd:3in-sphere 0.01)))
      ; (setf (gmsh/cam::@par (gmsh/scene:@cam sc) 1) (values 30.3 31.300 33.3))
      ; (veq:vp (gmsh/cam::@par (gmsh/scene:@cam sc) 1))
      (print sc))))

(veq:fvdef main ()
  (declare (optimize speed (safety 0) space))
  (let* ((sc (new-scene))
         (msh (gmsh/scene:@msh sc))
         (bvh (gmsh:make-bvh msh :num 16 :mode :bvh4-simd
                :matfx (lambda (p) (veq:from-lst (gmsh/scene:getmat sc p))))))
    (print bvh)
    ; export scene: _box.gmsh-scene
    (gmsh/scene:scene/save sc "_box")
    ; render
    (time (handler-case
      (gmsh/xrend:xrend sc bvh :size *window-size*
                               :vol t :par t :aa 50 :ao-rep 5
                               :raylen 2000.0
                               :vlim 50.0 :vdst 1000.0 :vmult 100.0
                               :miss :bgk :world :bgk)
      (sb-sys:interactive-interrupt () (warn "render aborted. exporting png ..."))))
    ; save _box.png
    (gmsh/scene:canv/save sc "_box" :gamma 1.1)))

(main)

