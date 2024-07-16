#!/usr/local/bin/sbcl --script

(load "/home/anders/quicklisp/setup.lisp")
 (ql:quickload :gmsh) (require :sb-sprof)
; (rnd:set-rnd-state 107)
(gmsh/xrend:init 60)

(defvar *size* 5000)
; (defvar *fn* "sah-test")
; (defvar *fn* "20240713-214236-f77f802-b31ecf59")
; (defvar *fn* "20240715-190518-30b7476-a0f30535")
; (defvar *fn* "20240716-095857-19f769f-c73abd0e")

; (defvar *fn* "20240716-114004-7fbb3ed-1d4e5def")
(defvar *fn* "20240716-113944-7fbb3ed-47843858")



(veq:fvdef load-scene (fn &aux (sc (gmsh/scene:scene/load fn)))
  ; (gmsh:center! (gmsh/scene:scene-msh sc) :max-side 200.0)
  (gmsh/scene:scene/new-canv sc :size *size*)
  (gmsh/cam:update (gmsh/scene:scene-proj sc)
                ; :s (* *size* (gmsh/cam:@s (gmsh/scene:scene-proj sc))) ; WHYYYYY?? bad XREND scaling??
                :s 10f0
                :xy (veq:f2$point (- (* 0.5 *size*) 450f0) (- (* 0.5 *size*) 0f0))
                ; :cam (veq:f3$point 200.0 200.0 200.0)
                ; :look (veq:f3$point 0.0 0.0 0.0)
                )
  (print (gmsh/scene:scene-proj sc))
  (print (gmsh/scene:scene-msh sc))
  (loop for lim in '(20.0 15f0 10f0)
        do (gmsh/scene::split-edges sc lim) (print (gmsh/scene:scene-msh sc)))
  sc)

(veq:fvdef main (size)
  (declare (optimize speed (safety 0)))
  (let* ((fn (or (second (auxin:cmd-args) ) (lqn:str! "" *fn* ".gmsh-scene")))
         (sc (load-scene fn))
         (msh (gmsh/scene:scene-msh sc))
         (bvh

           (progn ;sb-sprof:with-profiling (:max-samples 200000 :report :graph :mode :cpu)

            (gmsh:make-bvh msh :num 16
                :matfx (lambda (p &aux (mc (gmsh/scene:getmat sc p)))
                        (case (second mc)
                           (:c (values :ao :w)) (:m (values :ao :w)) (:y (values :ao :w))
                           (:w (values :ao :w)) (:k (values :ao :w)))
                        (values :ro :w)
                        ))))
    )

    (print :-----) (print msh) (print bvh)

    (time (handler-case
     (gmsh/xrend:xrend sc bvh
            :size *size* :par t :vol nil :aa 50
            :raylen 5000.0 :ao-dst 1500f0
            :max-depth 13
            :ao-rep 7
            :vmult 13.0 :vlim 50f0 :vdst 500f0
            :miss :bgk
            :world :bgw)
     (sb-sys:interactive-interrupt () (warn "render aborted. exporting png ..."))))
    (handler-case
      (gmsh/scene:canv/save sc *fn* :gamma 1.1)
      (sb-sys:interactive-interrupt () (warn "png export aborted."))
      )))

(main 1000)

