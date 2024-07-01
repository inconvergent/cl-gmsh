#!/usr/local/bin/sbcl --script

(load "/home/anders/quicklisp/setup.lisp") (require :sb-sprof)
(ql:quickload :auxin) (ql:quickload :gmsh)
(gmsh/xrend:init 64)

(veq:fvdef load-scene (size)
  (declare (optimize speed (safety 1)))
  (let* ((sc (gmsh/scene:scene/make :s (/ 1200f0) :xy (veq:f2$zero)))
         (msh (gmsh/scene:scene-msh sc))
         (s- -1000.0) (ss (abs (* 0.5 s-)))
         (tt 30f0) (w -350f0))
    (macrolet ((box (&body body) `(gmsh/io:obj/load-model :cube :msh msh ,@body))
               (setmat (mat &body body)
                 `(auxin:mvb (_ meta) ,@body
                   (loop for p in (cdr (assoc :polys meta))
                         do (gmsh/scene:setmat sc p ',mat)))))

      (setmat (:ro :w) (gmsh/io:obj/load-model :teapot2 :msh msh
                                               :xy (veq:f3$point w w (- 450.0 ))
                                               :s (veq:f3$point tt tt tt)
                                               :center t))

      (setmat (:ro :w) (box :xy (veq:f3$point s- 0f0 0f0) :s (veq:f3$val ss)))
      (setmat (:ro :w) (box :xy (veq:f3$point 0f0 s- 0f0 ) :s (veq:f3$val ss)))
      (setmat (:ro :w) (box :xy (veq:f3$point 0f0 0f0 s-) :s (veq:f3$val ss)))
      (loop for lim in '(15f0 15f0) do (gmsh/scene::split-edges sc lim) (print msh))
      (gmsh/scene:scene/new-canv sc :size size)
      (ortho:update (gmsh/scene:scene-proj sc)
                    :s 10f0 :xy (veq:f2$val (* 0.5 size))
                    :cam (veq:f3$point 1000.0 1000.0 1000.0)
                    :up (veq:f3$point 0f0 0f0 -1f0)
                    :look (veq:f3$point 0f0 0f0 -45f0))
      sc)))

(veq:fvdef main (size)
  (declare (optimize speed (safety 0)))
  (let* ((sc (load-scene size))
         (msh (gmsh/scene:scene-msh sc))
         (bvh (gmsh:make-bvh msh :num 4 :mode :bvh2-stackless
                :matfx (lambda (p) (veq:from-lst (gmsh/scene:getmat sc p))))))
    (print msh) (print bvh)
    (time (gmsh/xrend:xrend sc bvh :size size :raylen 3000.0
                                   :vol nil :par t :aa 100
                                   :miss :bgw
                                   :world :bgw
                                   ; :vexpt 0.1
                                   :ao-rep 5
                                   ; :ao-mult
                                   :raylen2 200f0))
    (gmsh/scene:canv/save sc "tmp2" :gamma 1.1)))

(main 4000)

