#!/usr/local/bin/sbcl --script

(require :sb-sprof)
(load "/home/anders/quicklisp/setup.lisp")
(ql:quickload :auxin) (ql:quickload :gmsh)
; (rnd:set-rnd-state 107)
(gmsh/xrend:init 64)

(defvar *size* 2000)

(veq:fvdef load-scene ( &aux (sc (gmsh/scene:scene/load "_gpu-export.gmsh-scene")))
  (print (gmsh/scene:scene-proj sc))
  ; (gmsh:center! (gmsh/scene:scene-msh sc) :max-side 200.0)
  (gmsh/scene:scene/new-canv sc :size *size*)
  (ortho:update (gmsh/scene:scene-proj sc)
                :s (* *size* (ortho:@s (gmsh/scene:scene-proj sc))) ; WHYYYYY?? bad XREND scaling??
                :xy (veq:f2$val (* 0.5 *size*))
                ; :cam (veq:f3$point 200.0 200.0 200.0)
                ; :look (veq:f3$point 0.0 0.0 0.0)
                )
  (print (gmsh/scene:scene-msh sc))
  (loop for lim in '(10.0 5.0)
        do (gmsh/scene::split-edges sc lim) (print (gmsh/scene:scene-msh sc)))
  sc)

; TODO: adjust bg/world color flag. probably with depth in hitmat?

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
                           (:w (values :ll :w)) (:k (values :ao :k)))))))

    (print :-----) (print msh) (print bvh)

; (sb-sprof:with-profiling (:max-samples 200000
;                         :mode :cpu
;                         ; :mode :alloc
;                         ; :mode :time
;                         :report :graph)

    (time (gmsh/xrend:xrend sc bvh :size *size*
                                   :par t :vol t :aa 55
                                  :vmult 100.0
                                  :ao-rep 6
                                  :miss :bgkk
                                  :vdst 600f0
                                  :world :bgkk
                                  :vrec 6
                                  :vdst 1000f0
                                  :raylen 1200.0
                                  :raylen2 50f0
                                  ))
    ; )

    (gmsh/scene:canv/save sc (fn:fn) :gamma 1.1)
    ))

(main 1000)

