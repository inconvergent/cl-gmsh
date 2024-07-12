(in-package :gmsh/gl)

(defun sdl-info () "print sdl info."
  (lqn:out "~&██ SDL library version: ~d.~d.~d~&"
    sdl2-ffi:+sdl-major-version+ sdl2-ffi:+sdl-minor-version+ sdl2-ffi:+sdl-patchlevel+))

(defun init-viewport (&key (w 1000) (h 1000) (v 0.0))
  (declare (veq:pn w h) (veq:ff v)) "initialize gl viewport."
  (gl:viewport 0 0 w h) ; (gl:enable :depth-test) (gl:depth-func :less)
  (gl:clear-color v v v 1.0))

(defmacro init-controller (controllers) ; TODO: WITH CONTROLLERS CONTEXT
  "initialize connected controllers. incomplete."
  `(progn
     (setf ,controllers (list))
     (loop :for i :upto (- (sdl2:joystick-count) 1)
           :do (when (sdl2:game-controller-p i)
                 (format t "~&██ found CONTROLLER: ~a~&" (sdl2:game-controller-name-for-index i))
                 (let* ((gc (sdl2:game-controller-open i))
                        (joy (sdl2:game-controller-get-joystick gc)))
                   (setf ,controllers (acons i gc ,controllers)))))))

(defmacro window-context ((w h &key (win 'cl-user::win) (ctxt 'cl-user::ctxt)
                                    (controllers 'cl-user::controllers)
                                    (run 'cl-user::run))
                               &body body)
  (declare (symbol w h win ctxt controllers run))
  "create gl window context. see basic.lisp for example use. incomplete."
  `(sdl2:with-init (:everything)
    (sdl-info)
    (sdl2:with-window (,win :w ,w :h ,h :flags '(:shown :opengl ))
      (sdl2:with-gl-context (,ctxt ,win)
        (lqn:out "~&██ GL WINDOW. wh: ~a ~a.~%" ,w ,h)
        (sdl2:gl-make-current ,win ,ctxt)
        (init-viewport :w ,w :h ,h)
        (let* ((,run t) (,controllers (list)))
          (init-controller ,controllers)
          (progn ,@body))))))

(defun compile-shader (shader-type src &aux (s (gl:create-shader shader-type)))
  (declare #.*opt* (keyword shader-type) (string src))
  "build shader of type :vertex-shader, :fragment-shader at src."
  (gl:shader-source s src) (gl:compile-shader s)
  (let ((l (gl:get-shader-info-log s)))
    (when (lqn:some? l)
      (lqn:out "~&██ COMPILE ~(~a~) message:~%~d~&" shader-type l) (finish-output)))
  s)

(defun make-program* (vs fs &aux (p (gl:create-program))) (declare #.*opt* (string vs fs))
  "make shader program from these two source codes."
  (gl:attach-shader p (compile-shader :vertex-shader vs))
  (gl:attach-shader p (compile-shader :fragment-shader fs))
  (gl:link-program p)
  (let ((l (gl:get-program-info-log p)))
    (when (lqn:some? l) (lqn:out "~&██ LINK program message:~%~d~&" l) (finish-output)))
  p)

(defun make-program (name &optional (loc (gmsh::internal-path-string "src/shaders/")))
  (declare (keyword name) (string loc))
  "make program from shader files (name.frag, name.vert) located at path loc."
  (lqn:out "~&██ MAKE PROGRAM: ~(~a~a~)(.vert|frag)~&" loc name)
  (let ((p (make-program* (uiop:read-file-string (lqn:fmt "~a~(~a~).vert" loc name))
                          (uiop:read-file-string (lqn:fmt "~a~(~a~).frag" loc name)))))
    (finish-output)
    p))

(defun tick (&optional (s 1000f0)) (declare (veq:ff s) (ignorable s)) "get current tick."
  (/ (veq:ff (sdl2-ffi.functions:sdl-get-ticks)) s))

(defun to-gl-array (seq type &aux (l (length seq)) (arr (gl:alloc-gl-array type l)))
  (declare #.*opt* (sequence seq) (keyword type)) "create gl array from seq."
  (dotimes (i l) (setf (gl:glaref arr i) (aref seq i)))
  arr)

(defmacro set-uniform-f (p name val) "set float uniform."
  `(veq:mvc #'gl:uniformf (gl:get-uniform-location ,p ,(lqn:sdwn name)) ,val))
(defmacro set-uniform-i (p name val) "set integer iniform."
  `(veq:mvc #'gl:uniformi (gl:get-uniform-location ,p ,(lqn:sdwn name)) ,val))
(defmacro set-uniform-mat-4f (p name val &optional (transpose t)) "set 4f uniform."
  `(gl:uniform-matrix (gl:get-uniform-location ,p ,(lqn:sdwn name)) 4 ,val ,transpose))

; let (arrloc (gl:get-uniform-block-index p "AllTriangles"))
; (cl-opengl-bindings:uniform-block-binding p arrloc 0)
; TODO: use shader binding by uniform name
(defun bind-buffer-object (buf ty o &key (slot 0) &aux (arr (to-gl-array o ty)))
  (declare #.*opt*)
  (gl:bind-buffer :uniform-buffer buf)
  (gl:buffer-data :uniform-buffer :static-draw arr)
  (cl-opengl-bindings:bind-buffer-base :uniform-buffer slot buf))

(defun make-buftex (p name buf buf-tex arr &key (slot 0) (layout :RGBA32F))
  (declare #.*opt*)
  (gl:active-texture (lqn:kw! :texture slot))
  (gl:bind-buffer :texture-buffer buf)
  (gl:buffer-data :texture-buffer :static-draw arr)
  (gl:bind-texture :texture-buffer buf-tex)
  (cl-opengl-bindings:tex-buffer :texture-buffer layout buf)
  (cl-opengl-bindings:uniform-1i (gl:get-uniform-location p (lqn:sdwn name)) slot))

