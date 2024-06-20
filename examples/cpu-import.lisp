#!/usr/local/bin/sbcl --script

(load "/home/anders/quicklisp/setup.lisp")
(ql:quickload :auxin) (ql:quickload :gmsh)
; (rnd:set-rnd-state 107)
(gmsh/xrend:init 60)

(veq:fvdef load-scene ( &aux (sc (gmsh/scene:scene/load "_gpu-export.gmsh-scene")))
  (gmsh:center! (gmsh/scene:scene-msh sc) :max-side 3f0)
  ; (gmsh/scene:scene-proj sc)
  (ortho:update (gmsh/scene:scene-proj sc)
                :s 320f0
                :xy (veq:f2$point (veq:f2val 500f0))
                :cam (veq:f3$point 12.1f0 12.1f0 12.1f0)
                :look (veq:f3$point 0f0 0f0 0f0))
  ; (gmsh/scene:scene-proj sc)
  sc)

(veq:fvdef main (size)
  (declare (optimize speed (safety 0)))
  (let* ((sc (load-scene))
         (msh (gmsh/scene:scene-msh sc))
         (bvh (gmsh:make-bvh msh :num 10 :mode :bvh4-simd
                :matfx (lambda (p &aux (mc (gmsh/scene:getmat sc p)))
                        ; (veq:from-lst mc)
                        (case (second mc) (:c (values :ll :c))
                                          (:m (values :ll :m))
                                          (:y (values :ao :y))
                                          (:k (values :ao :k)))))))
    (print msh)
    (print bvh)
    (time (gmsh/xrend:render sc bvh :par t :aa 600
            :size 1000 :bs 1 :raylen 2000f0))
    (gmsh/scene:canv/save sc "tmp16" :gamma 1.1)
    (print sc)))

(main 1000)

