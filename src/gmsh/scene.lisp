(in-package :gmsh/scene)

(defun print-scene (o s)
  (format s "<@scene ~a ~a | ~a~%--~%~a>"
    (scene-program o) (scene-size o) (scene-msh o) (scene-proj o)))
(defstruct (scene (:constructor -make-scene) (:print-object print-scene))
  "unified scene object for gpu/cpu rendering. with file export/import."
  (msh nil :type gmsh:gmsh :read-only t)
  (program :std :type (or keyword string) :read-only nil)
  (proj (ortho:make) :type ortho::ortho :read-only t)
  (look (veq:f3$point 0f0 0f0 0f0) :type veq:fvec :read-only nil)
  (axis-state #.(veq:f_ '(0f0 0f0 0f0 0f0 0f0 0f0 0f0)) :type veq:fvec :read-only t)
  (size 1000 :type fixnum :read-only t)
  ; (canv (canvas:make :size 2000) :type canvas::canvas :read-only t)
  (canv nil :read-only nil)
  (lim 0.00001f0 :type veq:ff :read-only t)
  (matfx #'identity :type function :read-only nil)
  (matmap (make-hash-table :test #'equal) :type hash-table :read-only t)
  (pm #() :type vector :read-only nil)
  (vm #() :type vector :read-only nil))
  ; (attr nil :read-only nil) (n 0 :read-only nil)

(declaim (inline make-pm make-vm get-pm get-vm get-s set-s))
(defun make-vm (sc) (declare (scene sc)) "new view matrix."
  (vector (ortho:vm (scene-proj sc) (veq:f3$ (scene-look sc)))))
(defun make-pm (sc) (declare (scene sc)) "new projection matrix."
  (vector (ortho:pm (scene-proj sc) (get-s sc) 0.01 50f0)))
(defun get-pm (sc)  (declare (scene sc)) "current projection matrix." (scene-pm sc))
(defun get-vm (sc)  (declare (scene sc)) "current view matrix."       (scene-vm sc))
(defun get-s (sc)   (declare (scene sc)) "current scale."             (ortho:@s (scene-proj sc)))
(defun set-s (sc s) (declare (scene sc)) "set new scale."             (ortho:update (scene-proj sc) :s s))

(veq:fvdef update-view (sc &aux (look (scene-look sc)))
  (declare (optimize speed (safety 1))) "update scene view."
  (labels ((trans (proj ax v)
             (veq:xlet ((f3!dx (f3!@*. (veq:f3$
                                         (if (eq ax :u) (ortho::ortho-u proj)
                                                        (ortho::ortho-v proj))) v)))
               (setf (veq:3$ look) (f3!@+ (veq:f3$ look) dx))
               (ortho:update proj
                 :cam (veq:f3$point (f3!@+ (veq:f3$ (ortho::ortho-cam proj)) dx))
                 :look (scene-look sc))))
           (around (proj ax v) (ortho::around proj ax v :look (scene-look sc))))

    (loop for (fx ax s) in `((,#'around :yaw 0.07) (,#'around :pitch 0.07)
                             (,#'trans :u -2.0) (,#'trans :v -2.0)
                             (,#'around :roll 0.07) (,#'around :roll -0.07))
          for v across (scene-axis-state sc)
          if fx do (funcall fx (scene-proj sc) ax (* s v))))
  (setf (scene-vm sc) (make-vm sc) (scene-pm sc) (make-pm sc)))

(defun canv/save (sc fn &key (gamma 1f0))
  (declare (scene sc) (string fn) (veq:ff gamma)) "save scene canvas to fn as png."
  (canvas:save (scene-canv sc) fn :gamma gamma))

(veq:fvdef update-axis (sc ax v &aux (v (* (signum v) (max 0 (- (abs v) 3500)))))
  (labels ((analogue (val &aux (val* (expt (veq:fclamp (/ (veq:ff (abs val))
                                                         #.(- 32767 3500))) 1.3)))
                     (* (signum val) val*)))
     (setf (aref (scene-axis-state sc) ax) (analogue v))))

(declaim (inline setmat getmat setmats))
(defun setmat (sc p m &aux (matmap (scene-matmap sc)))
  (declare (scene sc) (list p m)) "set poly mat"
  (if m (setf (gethash p matmap) m)
        (wrn :setmat "missing material when assigning: ~a" p)))

(defun setmats (sc pp m)
  (declare (scene sc) (list pp m)) "set poly mats"
  (loop for p in pp do (setmat sc (the list p) m)))

(defun getmat (sc p &optional default
                    &aux (matmap (scene-matmap sc)) (res (gethash p matmap)))
  (declare (scene sc) (list p)) "get poly material "
  (unless res (wrn :getmat "missing material for: ~a; default: ~a" p default))
  (or res default))

(defun gpu/do-pack-bvh (sc) (declare (scene sc))
  (gmsh/bvh:gpu/pack-bvh (gmsh:make-bvh (scene-msh sc) :num 7
                           :matfx (scene-matfx sc) :mode :bvh2-stackless)))

(defun scene/save (sc &key (fn (fn:fn)) matfx (colors gmsh:*matpar*)
                      &aux (msh (scene-msh sc)))
  (declare (scene sc) (string fn))
  (lqn:out "~&████ exporting: ~a.~%████ fn: ~a~&" msh fn)
  (labels ((mfx (p) (getmat sc p '(:c :x)))
           (mname (m) (lqn:sdwn m 1))
           (expfx (m &aux (m (second m))) `(,(mname m) :c ,m))
           (propfx (mc) `(("mtllib" . ,(lqn:fmt "~a.mtl" fn))
                          ("usemtl" . ,(mname (second mc)))))) ; TODO: usemtl is incorrect
    (let ((exp-mats (gmsh/io:obj/save msh fn :matfx (the function (or matfx #'mfx))
                                             :propfx #'propfx)))
      (gmsh/io:obj/mat-save msh fn (mapcar #'expfx exp-mats) :colors colors)
      (lqn:dat-export fn
        `((:fn . ,fn) (:size . ,(scene-size sc))
          (:programs . ,gmsh:*programs*) (:matpar . ,gmsh:*matpar*)
          (:proj . ,(ortho:export-data (scene-proj sc)))
          (:msh . ,(gmsh/io:mexport msh
                      :matfx (the function (or matfx #'mfx)))))))))

; TODO: use fvec for keys?
(veq:fvdef scene/make (&key (max-verts 2000000) (program :std)
                            (msh (gmsh:gmsh :max-verts max-verts))
                            (cam '(401f0 400f0 101f0)) (look '(0f0 0f0 0f0))
                            proj canv
                            (s 1f0) (xy '(500f0 500f0))
                            (matmap (make-hash-table :test #'equal)) matfx)
  (declare (list look cam xy) (keyword program) (hash-table matmap))
  (labels ((make-ortho () (ortho:make :cam (veq:f3$point (veq:from-lst cam))
                                      :look (veq:f3$point (veq:from-lst look))
                                      :xy (veq:f2$point (veq:from-lst xy)) :s s))
           (matfx (p) (veq:mvb (m exists) (gethash p matmap '(:c :x))
                        (unless exists (wrn :scene-matfx "poly missing matmap: ~a" p))
                        (values-list m))))
    (-make-scene :program program :msh msh :proj (the ortho::ortho (or proj (make-ortho)))
                 :canv canv
                 :look (veq:f3$point (veq:from-lst look))
                 :matmap matmap :matfx (the function (or matfx #'matfx)))))
(defun scene/load (fn &aux (o (lqn:dat-read-one fn))) ; TODO: incomplete
  (declare (string fn))
  (gmsh/io:mimport (cdr (assoc :msh o))))

