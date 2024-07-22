(in-package :gmsh/scene)

(declaim (inline @pm @vm setmat getmat setmats))

(defun -prt (o s)
  (format s "<@scene ~@(~a ~a~%~a~%~a>~)"
            (scene-size o) (scene-program o) (scene-msh o) (scene-cam o)))
(defstruct (scene (:constructor -make-scene)
                  (:print-object -prt))
  "unified scene object for gpu/cpu rendering. with file export/import."
  (program :std :type (or keyword string) :read-only nil)
  (msh nil :type gmsh:gmsh :read-only t)
  (cam nil :type gmsh/cam:cam :read-only t)
  (axis-state #.(veq:f$~ (6) 0f0 0f0 0f0 0f0 0f0 0f0 0f0) :type veq:fvec :read-only t)
  (size 1000 :type fixnum :read-only nil)
  (canv nil :read-only nil)
  (matfx #'identity :type function :read-only nil)
  (matmap (make-hash-table :test #'equal) :type hash-table :read-only t)
  (pm #(nil) :type vector :read-only t)
  (vm #(nil) :type vector :read-only t))

(defun @program (sc) (declare (scene sc)) "get program." (scene-program sc))
(defun @size    (sc) (declare (scene sc)) "get program." (scene-size sc))
(defun @msh     (sc) (declare (scene sc)) "get msh."     (scene-msh sc))
(defun @cam     (sc) (declare (scene sc)) "get cam."     (scene-cam sc))
(defun @pm      (sc) (declare (scene sc)) "get pm."      (scene-pm sc))
(defun @vm      (sc) (declare (scene sc)) "get vm."      (scene-vm sc))
(defun @canv    (sc) (declare (scene sc)) "get canv"     (scene-canv sc))

(defun scale-to! (sc to &optional (from (@size sc)))
  (gmsh/cam:scale-from-to! (gmsh/scene:@cam sc) from to)
  (setf (scene-size sc) to)
  sc)

(defun canv/save (sc fn &key (gamma 1f0))
  (declare (scene sc) (string fn) (veq:ff gamma)) "save scene canvas to fn as png."
  (canvas:save (scene-canv sc) fn :gamma gamma))

(doc PS4 CONTROLLER BUTTONS

      L2              R2
      L1 9            R1  10

      ↑ 11            3
  ←13   14 →       2     1
      ↓ 12            0

      7     sticks    8       )

(veq:fvdef update-view (sc &aux (c (@cam sc)))
  (declare #.*opt* (scene sc)) "update scene view and proj matrices."
  (gmsh/cam:nav/axis! c (scene-axis-state sc))
  (setf (veq:$ (scene-vm sc)) (gmsh/cam:vm c)
        (veq:$ (scene-pm sc)) (gmsh/cam:pm c)))

(veq:fvdef update-axis (sc ax val &aux (val (* (signum val) (max 0 (- (abs val) 3500)))))
  (labels ((analogue (val &aux (val* (expt (veq:fclamp (/ (veq:ff (abs val))
                                                          #.(veq:ff (- 32767 3500))))
                                           3.0)))
                     (* (signum val) val*)))
     (setf (aref (scene-axis-state sc) ax) (analogue val))))

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

; TODO: pass all bvh params
(defun gpu/do-pack-bvh (sc &optional (num 32) (num-buckets 31))
  (declare (scene sc) (veq:pn num num-buckets))
  (gmsh/bvh:gpu/pack-bvh
    (print (gmsh:make-bvh (@msh sc) :mode :bvh2-stackless
             :num num :num-buckets num-buckets
             :matfx (scene-matfx sc)))))

(defun scene/new-canv (sc &key size) (declare (scene sc))
  (when size (setf (scene-size sc) size))
  (setf (scene-canv sc) (canvas:make :size (scene-size sc)))
  sc)

(veq:fvdef split-edges (sc lim) (declare (scene sc) (veq:ff lim))
  "split edges longer than lim. sometimes this makes faster raycasting (with bvh)."
  (loop with msh = (@msh sc)
        for e across (lqn:keys? (gmsh::gmsh-edges->poly msh))
        if  (> (veq:f3dst (veq:f3$ (gmsh:@verts msh e) 0 1)) lim)
        do (gmsh::split-edge! msh e :matfx (lambda (old new)
                                             (gmsh/scene:setmat sc new
                                               (gmsh/scene:getmat sc old))))))

(veq:fvdef scene/make (&key (size 1000) (max-verts 2000000)
                            (program :std)
                            (msh (gmsh:gmsh :max-verts max-verts))
                            (cam (gmsh/cam:make))
                            matfx (matmap (make-hash-table :test #'equal)) )
  (declare (veq:pn max-verts size)
           (keyword program ) (hash-table matmap) (gmsh/cam:cam cam))
  (labels ((matfx (p) (veq:mvb (m exists) (gethash p matmap '(:c :x))
                        (unless exists (wrn :scene-matfx "poly missing matmap: ~a" p))
                        (values-list m))))
    (-make-scene :size size :program program :msh msh
                 :cam cam
                 :matmap matmap :matfx (the function (or matfx #'matfx)))))

(defun scene/save (sc fn &key matfx obj (colors gmsh:*matpar*) srcfile
                         &aux (msh (@msh sc)))
  (declare (scene sc) (string fn) (boolean obj) (list colors))
  (lqn:out "~&████ exporting: ~a.~%████ fn: ~a~&" msh fn)
  (labels ((matfx (p) (getmat sc p '(:c :x)))
           (expfx (m &aux (m (second m))) `(,(mname m) :c ,m))
           (mname (m) (lqn:sdwn m 1))
           (propfx (mc) `(("mtllib" . ,(lqn:fmt "~a.mtl" fn))
                          ("usemtl" . ,(mname (second mc)))))) ; TODO: usemtl could be better
    (when obj (gmsh/io:obj/save-mtl msh fn
                (mapcar #'expfx (gmsh/io:obj/save msh fn :propfx #'propfx
                                  :matfx (the function (or matfx #'matfx))))
                :colors colors))
    (lqn:dat-export fn `((:ctx . ((:fn . ,fn) (:now . ,(lqn:now)) (:pkg . :gmsh/scene)
                                  (:sys . :gmsh) (:sysver . ,(gmsh:v?))
                                  (:srcfile . ,(lqn:str! srcfile)) (:ver . :v2)))
                         (:size . ,(scene-size sc))
                         (:programs . ,gmsh:*programs*) (:matpar . ,gmsh:*matpar*)
                         (:cam . ,(gmsh/cam:export-data (@cam sc)))
                         (:msh . ,(gmsh/io:export-data msh
                                     :matfx (the function (or matfx #'matfx)))))
                    ".gmsh-scene")))

; TODO: material definitions/ colors/textures/
(defun scene/load (fn &aux (o (lqn:dat-read-one fn))
                           (matmap (make-hash-table :test #'equal)))
  (declare (string fn) (hash-table matmap))
  (labels ((gk (k) (cdr (assoc k o)))
           (as-float (k) (veq:ff (gk k)))
           (as-arr (k) (veq:f_ (coerce (gk k) 'list)))
           (matfx (p m c) (setf (gethash p matmap) `(,m ,c))))
   (let ((msh (gmsh/io:import-data (gk :msh) :matfx #'matfx)))
    (scene/make :size (gk :size)
                :msh msh :max-verts (gmsh::gmsh-max-verts msh)
                :cam (gmsh/cam:import-data (gk :cam))
                :matmap matmap))))

