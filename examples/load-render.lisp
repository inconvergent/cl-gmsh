#!/usr/local/bin/sbcl --script

(load "/home/anders/quicklisp/setup.lisp")
 (ql:quickload :gmsh) (require :sb-sprof)
(gmsh/xrend:init 60)
(defvar *size* 1500)
; (defvar *fn* "_example")
(defvar *fn* "xx")

(veq:fvdef load-scene (fn &aux (sc (gmsh/scene:scene/load fn)))
  (gmsh/scene:scene/new-canv sc :size *size*)
  ; (gmsh/cam:update! (gmsh/scene:@cam sc)
  ;               ; :cam (veq:f3$point 200.0 200.0 200.0))
  ; (loop for lim in '(20.0 15f0 10f0)
  ;       do (gmsh/scene:split-edges sc lim) (print (gmsh/scene:@msh sc)))
  (lqn:out "~%loaded scene: ~%")
  (print sc))


(veq:fvdef main (size)
  (declare (optimize speed (safety 0)))
  (let* ((fn (or (second (auxin:cmd-args) ) (lqn:str! "" *fn* ".gmsh-scene")))
         (sc (load-scene fn))
         (msh (gmsh/scene:@msh sc))
         (bvh (gmsh:make-bvh msh :num 6
                :matfx (lambda (p &aux (mc (gmsh/scene:getmat sc p)))
                        ; (case (second mc)
                        ;    (:c (values :ao :w)) (:m (values :ao :w)) (:y (values :ao :w))
                        ;    (:w (values :ao :w)) (:k (values :ao :w)))
                        (veq:from-lst mc)))))
    (time (handler-case
     (gmsh/xrend:xrend sc bvh :par t :vol t :aa 50
                              :raylen 5000.0 :ao-dst 1500f0
                              :vmult 13.0 :vlim 50f0 :vdst 500f0
                              :max-depth 13 :ao-rep 7
                              :miss :bgk :world :bgw)
     (sb-sys:interactive-interrupt () (warn "render aborted. exporting png ..."))))
    (handler-case (gmsh/scene:canv/save sc *fn* :gamma 1.1)
                  (sb-sys:interactive-interrupt () (warn "png export aborted.")))))

(main 1000)

