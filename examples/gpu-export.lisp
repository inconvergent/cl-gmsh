#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")
(ql:quickload :auxin) (ql:quickload :gmsh)
(setf lparallel:*kernel* (lparallel:make-kernel 1))
(defvar *size* 1000) (defvar *pid* 0)
; (rnd:set-rnd-state 113)

; TODO: size export
; TODO: complete scene export/import -> cpu render pipeline

; TODO: fix failing tests
; TODO: volume gpu raytracer
; TODO: profile do-alter-mesh. performance.
; TODO: manual/auto extend max-verts
; TODO: itr (a b c ..) from list/vector

(veq:fvdef reset-mesh (sc &aux (msh (gmsh/scene:scene-msh sc)))
  (labels ((reset-mat () (clrhash (gmsh/scene::scene-matmap sc))
                         (gmsh:itr-polys (msh p)
                           (gmsh/scene:setmat sc p
                             (rnd:rcond (0.1 '(:c :c)) (0.1 '(:c :y))
                                        (0.1 '(:c :m)) (0.2 '(:c :k)))))))
    (gmsh:clear! msh)
    (gmsh/io:obj/load-model :ico :msh msh)
    (gmsh:center! msh :max-side 10f0)
    (reset-mat)
    (print sc)))

(veq:vdef do-alter-mesh (sc &aux (msh (gmsh/scene:scene-msh sc)))
  (declare (optimize speed (safety 1)))
  (handler-case
    (labels ((pwalker ((:va 3 s)) (rnd:3walker-acc s (rnd:3in-sphere 0.01f0)))
             (nwalker ((:va 3 s)) (rnd:3walker-acc s (rnd:3in-sphere 1.15f0)))
             (polymatfx (old new) (gmsh/scene:setmat sc new (gmsh/scene:getmat sc old)))
             (sym (wp wn)
               (veq:xlet ((f3!pt (f3!@+ (rnd:3in-sphere 10f0) (f@wp 2.3f0)))
                          (f3!n (veq:f3norm (f3!@+ (rnd:3in-sphere 1f0)
                                                   (f@wn (rnd:prob 0.3 2.1f0 0.5))))))
                 (gmsh:plane-sym msh pt n :matfx #'polymatfx)))
             (stretch (wp wn)
               (veq:xlet ((f3!pt (f@wp 3.3f0))
                          (f3!n (veq:f3norm (veq:fsel (:xyz) (f@wn (rnd:prob 0.5 2.1f0 0.1)))))
                          (inds (gmsh:plane-split msh pt n :matfx #'polymatfx))
                          (f!s (rnd:rndrng 20f0 30f0)))
                 (gmsh:tx! msh ; TODO p/tx
                   (set-difference (gmsh:p/classify-verts msh
                                     (lambda ((:va 3 pos)) (> (veq:f3dot (f3!@- pos pt) n) 0f0)))
                                   inds)
                   (lambda ((:va 3 pos)) (veq:f3from pos n s))))))
      (loop with wp = (m@pwalker (rnd:3in-sphere 0f0)) with wn = (m@nwalker (rnd:3on-sphere 1f0))
            repeat 1 if (rnd:prob 0.25 t nil) do (sym wp wn)
                     else do (stretch wp wn)))
    (error (e) (gmsh:wrn :alter-msh "unexpected err: ~a" e)))
  (print msh))

(veq:vdef make-render (sc)
  (declare (optimize speed (safety 1)))
  (let* ((pname (gmsh/scene:scene-program sc))
         (p (gmsh/gl:make-program pname))
         (proj (gmsh/scene:scene-proj sc))
         (plane (gmsh/gl:to-gl-array #.(veq:f_ '(1.0 1.0 -1.0 -1.0 1.0 -1.0 1.0 1.0 -1.0 1.0 -1.0 -1.0)) :float))
         (buf (gl:gen-buffers 10)) (tex (gl:gen-textures 10))
         (c2d (gl:get-attrib-location p "c2d")))
    (macrolet ((buftex (name i ty l)
                 `(gmsh/gl:make-buftex p ,(lqn:kw! name) (elt buf ,i) (elt tex ,i)
                    (gmsh/gl:to-gl-array ,(lqn:sym! name) ,ty) :slot ,i :layout ,l)))
      (labels ((init () (gl:use-program p)
                        (gl:bind-buffer :array-buffer (elt buf 0))
                        (gl:buffer-data :array-buffer :static-draw plane)
                        (veq:mvb (norms mima polyfx nodes mats matpar) (gmsh/scene:gpu/do-pack-bvh sc)
                          (buftex nodes 1 :int   :rgba32i) (buftex norms  2 :float :rgba32f)
                          (buftex mima  3 :float :rgba32f) (buftex polyfx 4 :float :rgba32f)
                          (buftex mats  5 :int   :rgba32i) (buftex matpar 6 :float :rgba32f))
                        (gl:enable-vertex-attrib-array c2d)
                        (gl:vertex-attrib-pointer c2d 2 :float nil 0 (cffi:null-pointer)))
               (program (pn) (setf pname pn p (gmsh/gl:make-program pn)) (gl:use-program p))
               (clean () (gl:disable-vertex-attrib-array c2d) (gl:free-gl-array plane)
                         (gl:delete-buffers buf) (gl:delete-textures tex))
               (uniforms () (gmsh/gl:set-uniform-f p :ts (gmsh/gl:tick))
                            (gmsh/gl:set-uniform-f p :resolution (veq:f2 1000f0 1000f0))
                            (gmsh/gl:set-uniform-f p :vpn (ortho:@vpn proj))
                            (gmsh/gl:set-uniform-f p :cam (ortho:@cam proj))
                            (gmsh/gl:set-uniform-mat-4f p :pm (gmsh/scene:get-pm sc))
                            (gmsh/gl:set-uniform-mat-4f p :vm (gmsh/scene:get-vm sc) )) ; nil
               (render () (uniforms) (gl:draw-arrays :triangles 0 6) (gl:flush)))
        (values #'init #'render #'clean #'program)))))

(defun show-diag (itt t0) (declare (veq:pn itt))
  (labels ((safe-inv (s) (or (and (> s 0f0) (/ s)) 0f0)))
    (when (zerop (mod itt 60))
      (let ((df (veq:ff (/ (- (veq:ff (get-internal-real-time)) t0)
                           #.(veq:ff internal-time-units-per-second)))))
        (lqn:out "~&██ frame: ~a, t: ~a, f: ~a~%" itt df (safe-inv df))))))

(defun fn () "_gpu-export") ; (fn:fn)

(veq:fvdef main ()
  (gmsh/gl:window-context (*size* *size*)
    (let ((sc (gmsh/scene:scene/make :s 0.01)) (itt 0))
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
            (lqn:out "~&██ btn: ~a~&" b)
            (case b (1 (do-alter-mesh sc) (f@render-init)) ; circle
                    (0 (setf *pid* (mod (1+ *pid*) (length gmsh:*programs*))) ; triangle
                       (funcall program (aref gmsh:*programs* *pid*))
                       (funcall render-init))
                    (2 (gmsh/scene::scene/save sc (fn))) ; square
                    (3 (reset-mesh sc) (funcall render-init)) ; x
                    (11 (gmsh/scene:set-s sc (min 100f0 (+ (gmsh/scene:get-s sc) 0.001))))
                    (12 (gmsh/scene:set-s sc (max 0.0005 (- (gmsh/scene:get-s sc) 0.001))))))
          (:keydown (:keysym keysym)
            ; (print (gmsh:get-num-polys (scene-msh sc)))
            ; (progn (gmsh:make-bvh (scene-msh sc) :num 10))
            (case (sdl2:scancode keysym)
                  (:scancode-e (gmsh/scene:scene/save sc (fn)))
                  (:scancode-r (do-alter-mesh sc) (f@render-init))))
          (:idle () (let ((t0 (veq:ff (get-internal-real-time))))
                      (when run (gmsh/scene:update-view sc) (f@render))
                      (sdl2:gl-swap-window win)
                      (show-diag (incf itt) t0)))
          (:quit () (f@render-clean) t))))))
(main)

