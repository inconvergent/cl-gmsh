#!/usr/local/bin/sbcl --script

(load "/home/anders/quicklisp/setup.lisp")
(ql:quickload :auxin) (ql:quickload :gmsh)
; (rnd:set-rnd-state 107)
(gmsh/xrend:init 60)

(veq:fvdef load-scene ( &aux (sc (gmsh/scene:scene/load "_gpu-export.gmsh-scene")))
  (print (gmsh/scene:scene-proj sc))
  ; (gmsh:center! (gmsh/scene:scene-msh sc) :max-side 200f0)
  (gmsh/scene:scene/new-canv sc :size 2000)
  (ortho:update (gmsh/scene:scene-proj sc)
                :s (* 2000.0 (ortho:@s (gmsh/scene:scene-proj sc))) ; WHYYYYY?? bad XREND scaling??
                :xy (veq:f2$point 1000f0 1000f0)
                ; :cam (veq:f3$point 200f0 200f0 200f0)
                ; :look (veq:f3$point 0f0 0f0 0f0)
                )

    (loop with msh = (gmsh/scene:scene-msh sc)
          for e across (lqn:keys? (gmsh::gmsh-edges->poly msh))
          if  (> (veq:f3dst (veq:f3$ (gmsh:get-verts msh e) 0 1)) 10f0)
          do (gmsh::split-edge! msh e :matfx (lambda (old new)
                                               (gmsh/scene:setmat sc new
                                                 (gmsh/scene:getmat sc old)))))
  sc)

; TODO: error in bs/blocks/size in render-utils
(veq:fvdef main (size)
  (declare (optimize speed (safety 0)))
  (let* ((sc (load-scene))
         (msh (gmsh/scene:scene-msh sc))
         (bvh (gmsh:make-bvh msh :num 8 :mode
                :bvh2-stackless
              ; :bvh4-simd
                :sort-lvl 32 :sort-num 8
                :matfx (lambda (p &aux (mc (gmsh/scene:getmat sc p)))
                        ; (veq:from-lst mc)
                        (case (second mc)
                           (:c (values :ll :c)) (:m (values :ll :m)) (:y (values :ll :y))
                           (:w (values :ll :w)) (:k (values :c :k)))))))

    (print :-----) (print msh) (print bvh)
    (time (gmsh/xrend:render sc bvh :par t :aa 1 :size 2000 :bs 2 :raylen 2000f0))
    (gmsh/scene:canv/save sc (fn:fn) :gamma 1.1)
    (print sc)))

(require :sb-sprof)
(sb-sprof:with-profiling (:max-samples 200000
                        ; :mode :cpu
                        ; :mode :alloc
                        :mode :time
                        :report :graph)

(main 1000)
)

; ve = 1.2 -> more fog; ve = 0.8 -> less
