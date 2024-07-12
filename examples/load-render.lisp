#!/usr/local/bin/sbcl --script

(require :sb-sprof)
(load "/home/anders/quicklisp/setup.lisp")
(ql:quickload :auxin) (ql:quickload :gmsh)
; (rnd:set-rnd-state 107)
(gmsh/xrend:init 64)

(defvar *size* 4000)

(veq:fvdef load-scene (fn &aux (sc (gmsh/scene:scene/load fn)))
  ; (gmsh:center! (gmsh/scene:scene-msh sc) :max-side 200.0)
  (gmsh/scene:scene/new-canv sc :size *size*)
  (gmsh/cam:update (gmsh/scene:scene-proj sc)
                ; :s (* *size* (gmsh/cam:@s (gmsh/scene:scene-proj sc))) ; WHYYYYY?? bad XREND scaling??
                :s 20f0
                :xy (veq:f2$val (* 0.5 *size*))
                ; :cam (veq:f3$point 200.0 200.0 200.0)
                ; :look (veq:f3$point 0.0 0.0 0.0)
                )
  (print (gmsh/scene:scene-proj sc))
  (print (gmsh/scene:scene-msh sc))
  (loop for lim in '(20.0)
        do (gmsh/scene::split-edges sc lim) (print (gmsh/scene:scene-msh sc)))
  sc)

(veq:fvdef main (size)
  (declare (optimize speed (safety 0)))
  (let* ((fn (or (second (auxin:cmd-args) ) "_example.gmsh-scene"))
         (sc (load-scene fn))
         (msh (gmsh/scene:scene-msh sc))
         (bvh (gmsh:make-bvh msh :num 6
                :matfx (lambda (p &aux (mc (gmsh/scene:getmat sc p)))
                        (case (second mc)
                           (:c (values :ao :w)) (:m (values :ao :w)) (:y (values :ao :w))
                           (:w (values :ao :w)) (:k (values :ao :w)))
                        (values :ro :w)
                        ))))

    (print :-----) (print msh) (print bvh)
    (time (handler-case
     (gmsh/xrend:xrend sc bvh
            :size *size* :par t :vol nil :aa 150
            :raylen 5000.0 :ao-dst 2000f0
            :ao-rep 6
            :vmult 13.0 :vlim 50f0 :vdst 500f0
            :miss :bgk
            :world :bgww)
     (sb-sys:interactive-interrupt () (warn "render aborted. exporting png ..."))))
    (handler-case
      (gmsh/scene:canv/save sc "_example" :gamma 1.1)
      (sb-sys:interactive-interrupt () (warn "png export aborted."))
      )))

(main 1000)

