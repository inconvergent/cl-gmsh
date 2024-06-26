#!/usr/local/bin/sbcl --script

(load "/home/anders/quicklisp/setup.lisp") (require :sb-sprof)
(ql:quickload :auxin) (ql:quickload :gmsh)
(gmsh/xrend:init 70)

(veq:fvdef load-scene ()
  (declare (optimize speed (safety 1)))
  (let* ((sc (gmsh/scene:scene/make :s 0.01 :xy (veq:f2$zero)))
         (msh (gmsh/scene:scene-msh sc))
         (z 0f0) (w 70f0) (x 5f0)
         (ww (- w (* 5 x))) (rr (- w (* 4 x)))
         (q (+ w x)))
    (macrolet ((box (&body body) `(gmsh/io:obj/load-model :cube :msh msh ,@body)))
      (box :xy (veq:f3$point (- w)   z  z) :s (veq:f3$point x q q))    ; walls
      (box :xy (veq:f3$point    w    z  z) :s (veq:f3$point x q q))
      (box :xy (veq:f3$point    z    w  z) :s (veq:f3$point q x q))
      (box :xy (veq:f3$point    z (- w) z) :s (veq:f3$point q x q))
      (box :xy (veq:f3$point z z    w)     :s (veq:f3$point ww ww x))  ; caps
      (box :xy (veq:f3$point z z (- w))    :s (veq:f3$point ww ww x))
      (gmsh:itr-polys (msh p) (gmsh/scene:setmat sc p '(:cr :k)))
      (auxin:mvb (_ meta) (box :s (veq:f3$point rr rr rr))
        (loop for p in (cdr (assoc :polys meta))
              do (gmsh/scene:setmat sc p '(:ll :w))))

      (loop for lim in '(15f0 15f0) do (gmsh/scene::split-edges sc lim) (print msh))
      (gmsh/scene:scene/new-canv sc :size 2000)
      (ortho:update (gmsh/scene:scene-proj sc)
                    :s 3f0 :xy (veq:f2$val 1000.0)
                    :cam (veq:f3$point 200.0 200.0 190.0)
                    :look (veq:f3$zero))
      sc)))

; TODO: error in bs/blocks/size in render-utils
(veq:fvdef main (size)
  (declare (optimize speed (safety 0)))
  (let* ((sc (load-scene))
         (msh (gmsh/scene:scene-msh sc))
         (bvh (gmsh:make-bvh msh :num 8 :mode :bvh2-stackless
                :matfx (lambda (p) (veq:from-lst (gmsh/scene:getmat sc p))))))
    (print msh) (print bvh)
    (sb-sprof:with-profiling (:max-samples 200000
                            :mode :cpu
                            ; :mode :alloc
                            ; :mode :time
                            :report :graph)
    (time (gmsh/xrend:render sc bvh
                             :size 2000 :raylen 2000.0
                             :vol t :par t :aa 40))
    )
    (gmsh/scene:canv/save sc "tmp2" :gamma 1.1)))

(main 1000)

