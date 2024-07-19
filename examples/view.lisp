#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp") (require :sb-sprof)
(ql:quickload :gmsh)
; (gmsh/xrend:init 1)
(defvar *window-size* 1500) (defvar *pid* 0)

(defun fn () (fn:fn))

(veq:fvdef load-scene (fn &aux (sc (gmsh/scene:scene/load fn)))
  (gmsh/scene:scene/new-canv sc)
  (lqn:out "loaded scene: ~a" fn)
  (gmsh/scene:scale-to! sc *window-size* )
  (print sc))

(veq:fvdef main (&aux (srcfile cl-user::*load-truename*))
  (gmsh/gl:window-context (*window-size* *window-size*)
    (let (
          ; (sc (gmsh/scene:scene/make))
          (sc (load-scene "_box.gmsh-scene"))
          (itt 0))
      (veq:mvb (render-init render render-clean program) (gmsh/gl:make-render sc)
        (funcall render-init)
        (sdl2:with-event-loop (:method :poll)
          (:controllerdeviceadded (:type ty :which which)
            (lqn:out "~&██ ~a ~a~&" ty which)
            (gmsh/gl:init-controller controllers))
          (:controlleraxismotion (:value v :which w :axis ax)
                                 (gmsh/scene::update-axis sc ax v))
          (:controllerbuttondown (:state state :which which :button b :type ty)
            ; (lqn:out "~&██ btn: ~a~&" b)
            ;     L2              R2
            ;     L1 9            R1  10
            ;
            ;     ↑ 11            3
            ; ←13   14 →       2     1
            ;     ↓ 12            0
            ;
            ;     7               8
            (case b (3  (do-alter-mesh sc :sym)         (f@render-init)) ; triangle
                    (2  (do-alter-mesh sc :stretch-v)   (f@render-init)) ; square
                    (1  (do-alter-mesh sc :stretch-vpn) (f@render-init)) ; circle
                    (0  (setf *pid* (mod (1+ *pid*) (length gmsh:*programs*))) ; roll program
                        (funcall program (aref gmsh:*programs* *pid*))
                        (funcall render-init))
                    ; (12 (gmsh/scene::scene/save sc (fn))) ; down
                    ))
          (:keydown (:keysym keysym)
            ; (lqn:out "~&██ key: ~a~&" (sdl2:scancode keysym))
            (case (sdl2:scancode keysym)
                  (:scancode-e (gmsh/scene:scene/save sc (fn) :srcfile srcfile))
                  (:scancode-s (do-alter-mesh sc :sym) (f@render-init))
                  ; (:scancode-1 (swap-proj-mode sc :ortho))
                  ; (:scancode-2 (swap-proj-mode sc :persp))
                  ))
          (:idle () (let ((t0 (veq:ff (get-internal-real-time))))
                      (when run (gmsh/scene:update-view sc) (f@render))
                      (sdl2:gl-swap-window win)
                      (gmsh/gl:show-diag (incf itt) t0)))
          (:quit () (f@render-clean) t))))))
(main)

