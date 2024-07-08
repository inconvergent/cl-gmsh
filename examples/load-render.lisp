#!/usr/local/bin/sbcl --script

(require :sb-sprof)
(load "/home/anders/quicklisp/setup.lisp")
(ql:quickload :auxin) (ql:quickload :gmsh)
; (rnd:set-rnd-state 107)
(gmsh/xrend:init 64)

(defvar *size* 4000)

(veq:fvdef load-scene (fn &aux (sc (gmsh/scene:scene/load fn)))
  (print (gmsh/scene:scene-proj sc))
  ; (gmsh:center! (gmsh/scene:scene-msh sc) :max-side 200.0)
  (gmsh/scene:scene/new-canv sc :size *size*)
  (gmsh/cam:update (gmsh/scene:scene-proj sc)
                :s (* *size* (gmsh/cam:@s (gmsh/scene:scene-proj sc))) ; WHYYYYY?? bad XREND scaling??
                :xy (veq:f2$val (* 0.5 *size*))
                ; :cam (veq:f3$point 200.0 200.0 200.0)
                ; :look (veq:f3$point 0.0 0.0 0.0)
                )
  (print (gmsh/scene:scene-msh sc))
  (loop for lim in '(25.0 20.0)
        do (gmsh/scene::split-edges sc lim) (print (gmsh/scene:scene-msh sc)))
  sc)

(veq:fvdef main (size)
  (declare (optimize speed (safety 0)))
  (let* ((fn (or (second (auxin:cmd-args) ) "_example.gmsh-scene"))
         (sc (load-scene fn))
         (msh (gmsh/scene:scene-msh sc))
         (bvh (gmsh:make-bvh msh :num 16
                :matfx (lambda (p &aux (mc (gmsh/scene:getmat sc p)))
                        (case (second mc)
                           (:c (values :ao :c)) (:m (values :ao :m)) (:y (values :ao :y))
                           (:w (values :ro :w)) (:k (values :ro :w)))))))

    (print :-----) (print msh) (print bvh)
    (time (gmsh/xrend:xrend sc bvh
            :size *size* :par t :vol nil :aa 1
            :raylen 1200.0 :raylen2 50f0
            :ao-rep 6
            :vmult 13.0 :vlim 50f0 :vdst 500f0
            :miss :bgkk
            :world :bgkk))
    (gmsh/scene:canv/save sc "_example" :gamma 1.1)))

(main 1000)

