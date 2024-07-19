#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp") (require :sb-sprof)
(ql:quickload :gmsh)
(setf lparallel:*kernel* (lparallel:make-kernel 10))
(defvar *window-size* 1000)
(defvar *pid* 0)
; (rnd:set-rnd-state 113)

(veq:fvdef reset-mesh (sc &aux (msh (gmsh/scene:@msh sc)))
  (labels ((reset-mat () (clrhash (gmsh/scene::scene-matmap sc))
                         (gmsh:itr/polys (msh p)
                           (gmsh/scene:setmat sc p
                             (list :c :w)
                             ; (rnd:rcond (0.1 '(:c :c)) (0.1 '(:c :m))
                             ;            (0.1 '(:c :y)) (0.5 '(:c :k)))
                             ))))
    (gmsh:clear! msh)
    (gmsh/io:obj/load-model :teapot :msh msh)
    (gmsh:center! msh :max-side 100.0)
    (reset-mat)
    (print sc)))

(veq:vdef do-alter-mesh (sc mode &aux (msh (gmsh/scene:@msh sc))
                                      (cam (gmsh/scene:@cam sc)))
  (declare (optimize speed (safety 1) (space 0)))
  (veq:xlet ((f3!nn  (gmsh/cam:@u   cam)) (f3!nn- (f3.@- nn))
             (f3!pos (gmsh/cam:@pos cam))
             (f3!vpn (gmsh/cam:@vpn cam)))
   (labels ((polymatfx (old new) (gmsh/scene:setmat sc new (gmsh/scene:getmat sc old)))
            (sym () (gmsh:plane-sym! msh pos nn :matfx #'polymatfx))
            (dot ((:va 3 x)) (> (veq:f3dot (f3!@- x pos) nn-) 0.001))
            (stretch-vpn (&aux (s (rnd:rndrng 5.0 40.0))
                               (msk (gmsh:msk/v msh x (dot x))))
              (gmsh:plane-slice! msh pos nn- :matfx #'polymatfx)
              (gmsh:msk/tx! msh x msk (veq:f3from x vpn s)))
            (stretch-v (&aux (s (rnd:rndrng 5.0 40.0))
                             (msk (gmsh:msk/v msh x (dot x))))
              (gmsh:plane-slice! msh pos nn- :matfx #'polymatfx)
              (gmsh:msk/tx! msh x msk (veq:f3from x nn- s))))
       (handler-case (ecase (print mode)
                            (:sym (sym))
                            (:stretch-vpn (stretch-vpn))
                            (:stretch-v (stretch-v)))
                     (error (e) (gmsh:wrn :alter-msh "unexpected err: ~a" e)))))
  (print msh))

(defun fn () (fn:fn))

(veq:fvdef main (&aux (srcfile cl-user::*load-truename* ))
  (gmsh/gl:window-context (*window-size* *window-size*)
    (let ((sc (gmsh/scene:scene/make))
          (itt 0))
      (reset-mesh sc)
      (veq:mvb (render-init render render-clean program) (make-render sc)
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
                    ; (0  (setf *pid* (mod (1+ *pid*) (length gmsh:*programs*))) ; roll program
                    ;     (funcall program (aref gmsh:*programs* *pid*))
                    ;     (funcall render-init))
                    ; (12 (gmsh/scene::scene/save sc (fn))) ; down
                    (6  (reset-mesh sc) (funcall render-init)) ; options
                    ; (9  (update-s sc     1.0)) ; L1
                    ; (10 (update-s sc    -1.0)) ; R1
                    ; (11 (update-near sc  1.0)) ; up
                    ; (12 (update-near sc -1.0)) ; dwn
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
                      (show-diag (incf itt) t0)))
          (:quit () (f@render-clean) t))))))
(main)

