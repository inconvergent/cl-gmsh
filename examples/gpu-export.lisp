#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp") (require :sb-sprof)
(ql:quickload :gmsh)
(setf lparallel:*kernel* (lparallel:make-kernel 10))
(defvar *size* 1000) (defvar *pid* 0)
; (rnd:set-rnd-state 113)

; TODO: unified projectons
; TODO: volume gpu raytracer
; TODO: profile do-alter-mesh. performance.
; TODO: manual/auto extend max-verts
; TODO: itr (a b c ..) from list/vector

(veq:fvdef reset-mesh (sc &aux (msh (gmsh/scene:scene-msh sc)))
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


(veq:vdef do-alter-mesh (sc mode &aux (msh (gmsh/scene:scene-msh sc))
                                      (cam (gmsh/scene:scene-proj sc)))
  ; (declare (optimize speed (safety 1) (space 0)))
  (progn ; sb-sprof:with-profiling (:max-samples 200000 :report :graph :mode :cpu)
   (veq:xlet ((f3!nn (veq:3$ (gmsh/cam:cam-u cam))) (f3!nn- (f3.@- nn))
             (f3!look (veq:3$ (gmsh/scene:scene-look sc)))
             (f3!vpn (veq:3$ (gmsh/cam:cam-vpn cam))))
    (labels ((polymatfx (old new) (gmsh/scene:setmat sc new (gmsh/scene:getmat sc old)))
             (sym () (gmsh:plane-sym! msh look nn :matfx #'polymatfx))
             (dot ((:va 3 x)) (> (veq:f3dot (f3!@- x look) nn-) 0.001))
             (stretch-vpn (&aux (s (rnd:rndrng 5.0 40.0))
                                (msk (gmsh:msk/v msh x (dot x))))
               (gmsh:plane-slice! msh look nn- :matfx #'polymatfx)
               (gmsh:msk/tx! msh x msk (veq:f3from x vpn s)))
             (stretch-v (&aux (s (rnd:rndrng 5.0 40.0))
                              (msk (gmsh:msk/v msh x (dot x))))
               (gmsh:plane-slice! msh look nn- :matfx #'polymatfx)
               (gmsh:msk/tx! msh x msk (veq:f3from x nn- s))))
        (handler-case
         (ecase (print mode)
                (:sym (sym))
                (:stretch-vpn (stretch-vpn))
                (:stretch-v (stretch-v)))
         ; (error (e) (gmsh:wrn :alter-msh "unexpected err: ~a" e))
         ))))
  (print msh))

(veq:vdef make-render (sc)
  (declare (optimize speed (safety 1) (space 0)))
  (let* ((pname (gmsh/scene:scene-program sc))
         (p (gmsh/gl:make-program pname))
         (proj (gmsh/scene:scene-proj sc))
         (plane (gmsh/gl:to-gl-array
                  #.(veq:f_ '(1.0  1.0 -1.0 -1.0  1.0 -1.0
                              1.0  1.0 -1.0  1.0 -1.0 -1.0)) :float))
         (buf (gl:gen-buffers 10)) (tex (gl:gen-textures 10))
         (c2d (gl:get-attrib-location p "c2d")))
    (macrolet ((buftex (name i ty l)
                 `(gmsh/gl:make-buftex p ,(lqn:kw! name) (elt buf ,i) (elt tex ,i)
                    (gmsh/gl:to-gl-array ,(lqn:sym! name) ,ty) :slot ,i :layout ,l)))
      (labels ((init () (gl:use-program p)
                        (gl:bind-buffer :array-buffer (elt buf 0))
                        (gl:buffer-data :array-buffer :static-draw plane)
                        (progn ;sb-sprof:with-profiling (:max-samples 200000 :report :graph :mode :cpu)
                         (veq:mvb (norms mima polyfx nodes mats matpar)
                                 (gmsh/scene:gpu/do-pack-bvh sc)
                          (buftex nodes  1 :int   :rgba32i)
                          (buftex norms  2 :float :rgba32f)
                          (buftex mima   3 :float :rgba32f)
                          (buftex polyfx 4 :float :rgba32f)
                          (buftex mats   5 :int   :rgba32i)
                          (buftex matpar 6 :float :rgba32f)))
                        (gl:enable-vertex-attrib-array c2d)
                        (gl:vertex-attrib-pointer c2d 2 :float nil 0 (cffi:null-pointer)))
               (program (pn) (setf pname pn p (gmsh/gl:make-program pn)) (gl:use-program p))
               (clean () (gl:disable-vertex-attrib-array c2d) (gl:free-gl-array plane)
                         (gl:delete-buffers buf) (gl:delete-textures tex))
               (uniforms () (gmsh/gl:set-uniform-f p :ts (gmsh/gl:tick))
                            (gmsh/gl:set-uniform-f p :resolution (veq:f2 1000.0 1000.0))
                            (gmsh/gl:set-uniform-f p :vpn (gmsh/cam:@vpn proj))
                            (gmsh/gl:set-uniform-f p :cam (gmsh/cam:@pos proj))
                            (gmsh/gl:set-uniform-mat-4f p :pm (gmsh/scene:@pm sc))
                            (gmsh/gl:set-uniform-mat-4f p :vm (gmsh/scene:@vm sc) )) ; nil
               (render () (uniforms) (gl:draw-arrays :triangles 0 6) (gl:flush)))
        (values #'init #'render #'clean #'program)))))

(defun show-diag (itt t0) (declare (veq:pn itt))
  (labels ((safe-inv (s) (or (and (> s 0.0) (/ s)) 0.0)))
    (when (zerop (mod itt 60))
      (let ((df (veq:ff (/ (- (veq:ff (get-internal-real-time)) t0)
                           #.(veq:ff internal-time-units-per-second)))))
        (lqn:out "~&██ frame: ~a, t: ~a, f: ~a~%" itt df (safe-inv df))))))

(defun fn () (fn:fn))

(veq:fvdef main ()
  (gmsh/gl:window-context (*size* *size*)
    (let ((sc (gmsh/scene:scene/make :s 1000f0
                :cam (veq:f3$point 1200.0 1200.0 500.0)))
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
            (lqn:out "~&██ btn: ~a~&" b)

            ;     L2              R2
            ;     L1 9            R1  10
            ;
            ;     ↑ 11            3
            ; ←13   14 →       2     1
            ;     ↓ 12            0
            ;
            ;     7               8
            (case b
                    (3  (do-alter-mesh sc :sym)     (f@render-init)) ; L1
                    (2  (do-alter-mesh sc :stretch-v) (f@render-init)) ; L2
                    (1  (do-alter-mesh sc :stretch-vpn) (f@render-init)) ; L2
                    (0  (setf *pid* (mod (1+ *pid*) (length gmsh:*programs*))) ; triangle
                        (funcall program (aref gmsh:*programs* *pid*))
                        (funcall render-init))
                    (12  (gmsh/scene::scene/save sc (fn))) ; square
                    (6  (reset-mesh sc) (funcall render-init)) ; options
                    (9 (gmsh/scene:set-s sc (min 1000.0 (+ (gmsh/scene:@s sc) 25.0))))
                    (10 (gmsh/scene:set-s sc (max 0.0 (- (gmsh/scene:@s sc) 25.0))))))
          (:keydown (:keysym keysym)
            ; (print (gmsh:@pnum (scene-msh sc)))
            ; (progn (gmsh:make-bvh (scene-msh sc) :num 10))
            (case (sdl2:scancode keysym)
                  (:scancode-e (gmsh/scene:scene/save sc (fn)))
                  (:scancode-s (do-alter-mesh sc :sym) (f@render-init))))
          (:idle () (let ((t0 (veq:ff (get-internal-real-time))))
                      (when run (gmsh/scene:update-view sc) (f@render))
                      (sdl2:gl-swap-window win)
                      (show-diag (incf itt) t0)))
          (:quit () (f@render-clean) t))))))
(main)

