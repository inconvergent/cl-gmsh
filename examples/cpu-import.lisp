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
  (loop for lim in '(25.0 25.0
                          20f0
                          )
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
                           (:c (values :ao :c)) (:m (values :ao :m)) (:y (values :ao :y))
                           (:w (values :ao :w)) (:k (values :ro :w)))))))

    (print :-----) (print msh) (print bvh)

; (sb-sprof:with-profiling (:max-samples 200000
;                         :mode :cpu
;                         ; :mode :alloc
;                         ; :mode :time
;                         :report :graph)

    (time (gmsh/xrend:xrend sc bvh :size *size*
                                   :par t :vol nil :aa 3
                                  :vmult 40.0
                                  :ao-rep 7
                                  :miss :bgr
                                  :world :bgg
                                  :vrec 6 :vlim 0.1
                                  :raylen 2000.0
                                  :raylen2 30f0
                                  ))
    ; )

    (gmsh/scene:canv/save sc (fn:fn) :gamma 1.1)
    ))

(main 1000)

