#!/usr/local/bin/sbcl --script

(load "/home/anders/quicklisp/setup.lisp")
(ql:quickload :auxin) (ql:quickload :gmsh)
; (rnd:set-rnd-state 107)
(gmsh/xrend:init 69)

(require :sb-sprof)

(veq:fvdef load-scene ( &aux (sc (gmsh/scene:scene/load "_gpu-export.gmsh-scene")))
  (print (gmsh/scene:scene-proj sc))
  ; (gmsh:center! (gmsh/scene:scene-msh sc) :max-side 200.0)
  (gmsh/scene:scene/new-canv sc :size 2000)
  (ortho:update (gmsh/scene:scene-proj sc)
                :s (* 1850.0 (ortho:@s (gmsh/scene:scene-proj sc))) ; WHYYYYY?? bad XREND scaling??
                :xy (veq:f2$point 1000.0 1000.0)
                ; :cam (veq:f3$point 200.0 200.0 200.0)
                ; :look (veq:f3$point 0.0 0.0 0.0)
                )
  (print (gmsh/scene:scene-msh sc))
  (loop for lim in '(25.0 25.0 15f0)
        do (gmsh/scene::split-edges sc lim) (print (gmsh/scene:scene-msh sc)))
  sc)

(veq:fvdef main (size)
  ; (declare (optimize speed (safety 0)))
  (let* ((sc (load-scene))
         (msh (gmsh/scene:scene-msh sc))
         (bvh (gmsh:make-bvh msh :num 8 :mode
                :bvh2-stackless
              ; :bvh4-simd
                ; :sort-lvl 32 :sort-num 8
                :matfx (lambda (p &aux (mc (gmsh/scene:getmat sc p)))
                        ; (veq:from-lst mc)
                        (case (second mc)
                           (:c (values :ll :c)) (:m (values :ll :m)) (:y (values :ll :y))
                           (:w (values :ll :w)) (:k (values :cr :k)))))))

    (print :-----) (print msh) (print bvh)


; (sb-sprof:with-profiling (:max-samples 200000
;                         :mode :cpu
;                         ; :mode :alloc
;                         ; :mode :time
;                         :report :graph)

    (time (gmsh/xrend:xrend sc bvh :size 2000
                                   :par t :vol t
                                              :aa 50
                                              :vmult 65.0
                                              :vrec 6 :vlim 0.1
                                              :vdepth 1
                                              :raylen 2000.0))
; )

    (gmsh/scene:canv/save sc (fn:fn) :gamma 1.1)
    ; (print sc)
    ))

(main 1000)

; ve = 1.2 -> less fog; ve = 0.3 -> more
