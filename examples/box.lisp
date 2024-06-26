#!/usr/local/bin/sbcl --script

(load "/home/anders/quicklisp/setup.lisp") (require :sb-sprof)
(ql:quickload :auxin) (ql:quickload :gmsh)
(gmsh/xrend:init 64)

(veq:fvdef load-scene ()
  (declare (optimize speed (safety 1)))
  (let* ((sc (gmsh/scene:scene/make :s 0.01 :xy (veq:f2$zero)))
         (msh (gmsh/scene:scene-msh sc))
         (z 0f0) (w 70f0) (w* 70f0) (x 5f0)
         (ww (- w 25f0))
         (rr (- w (* 4 x)))
         (q (+ w x)))
    (macrolet ((box (&body body) `(gmsh/io:obj/load-model :cube :msh msh ,@body))
               (iso (&body body) `(gmsh/io:obj/load-model :iso :msh msh ,@body))
               (setmat (mat &body body) `(auxin:mvb (_ meta) ,@body
                                           (loop for p in (cdr (assoc :polys meta))
                                                 do (gmsh/scene:setmat sc p ',mat)))))
      (setmat (:cc :k) (box :xy (veq:f3$point (- w)   z  z) :s (veq:f3$point x q q)))    ; walls
      (setmat (:cc :k) (box :xy (veq:f3$point    w    z  z) :s (veq:f3$point x q q)))
      (setmat (:cc :k) (box :xy (veq:f3$point    z    w  z) :s (veq:f3$point q x q)))
      (setmat (:cc :k) (box :xy (veq:f3$point    z (- w) z) :s (veq:f3$point q x q)))

      (setmat (:cc :k) (box :xy (veq:f3$point z z    w*)     :s (veq:f3$point ww ww x)))  ; caps
      (setmat (:cc :k) (box :xy (veq:f3$point z z (- w*))    :s (veq:f3$point ww ww x)))

      (setmat (:ll :w) (box :s (veq:f3$point rr rr rr)))

      (loop for lim in '(10f0) do (gmsh/scene::split-edges sc lim) (print msh))
      (gmsh/scene:scene/new-canv sc :size 2000)
      (ortho:update (gmsh/scene:scene-proj sc)
                    :s 3.5f0 :xy (veq:f2$val 1000.0)
                    :cam (veq:f3$point (f3!@+ 200f0 200.0 175.0 (rnd:3in-sphere 0.01)))
                    :look (veq:f3$point (rnd:3in-sphere 0.01)))
      sc)))


; TODO: error in bs/blocks/size in render-utils
(veq:fvdef main (size)
  (declare (optimize speed (safety 0) space))
  (let* ((sc (load-scene))
         (msh (gmsh/scene:scene-msh sc))
         (bvh (gmsh:make-bvh msh :num 2
                             :mode :bvh2-stackless
                             ; :mode :bvh4-simd
                             ; :mode :bvh2
                :matfx (lambda (p) (veq:from-lst (gmsh/scene:getmat sc p))))))
    (print msh) (print bvh)
    ; (veq:4$print (gmsh/bvh::bvh-int-nodes bvh))
    ; (sb-sprof:with-profiling (:max-samples 200000 :report :graph
    ;                                        ; :mode :alloc
    ;                                        ; :mode :time
    ;                                        :mode :cpu
    ;                                        )
    (time (gmsh/xrend:xrend sc bvh :size 2000 :raylen 2000.0
                                   :vol t :par t :aa 60
                                   :vrec 10
                                   :vlim 50.0
                                   :vdst 1000.0
                                   :miss :bgk
                                   :world :bgk
                                   :ao-rep 5
                                   :vmult 100.0))
    ; )
    (gmsh/scene:canv/save sc "tmp" :gamma 1.1)))

(main 1000)

