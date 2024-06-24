(in-package :gmsh/io)

(veq:fvdef obj/load (fn &key (max-verts 100000) (msh (gmsh:gmsh :max-verts max-verts))
                             ign silent
                        &aux (fn (auxin::ensure-filename fn ".obj" t)))
  (declare (string fn) (veq:pn max-verts) (list ign))
  ; (v1 vt1 vn1 v2 vt2 vn2 v3 vt3 vn3)
  "load this obj file. see: obj/load-model. see obj/load-model, obj/save"
  (let* ((verts (gmsh::gmsh-verts msh))
         ; (norms (gmsh-norms msh))
         (uv (gmsh::gmsh-uv msh)) (vt (gmsh::gmsh-vt msh)) (vn (gmsh::gmsh-vn msh))
         (num-verts (gmsh::gmsh-num-verts msh)) (num-uvs (gmsh::gmsh-num-uvs msh))
         (props (gmsh::gmsh-obj-props msh))
         (new-verts (list)) (new-uvs (list)) (new-polys (list)))
    (labels
      ((rfs (x) (read-from-string x nil nil))
       (/splt (x) (mapcar #'rfs (auxin:split (auxin:mkstr x) #\/)))
       (m1- (l) (mapcar #'1- l))
       (do-face (f) (mapcar #'/splt f))
       (l+ (l a) (mapcar (lambda (x) (+ x a)) l))
       (rn (in &optional (n 1)) (loop repeat n for x = (read in nil nil)
                                      while x collect x)))
      (dat::do-lines-as-buffer fn
        (lambda (in &aux (c (lqn:kw! (car (rn in 1)))))
          (case c
            (:vn nil)
            (:v (progn (setf (veq:3$ verts num-verts) (veq:from-lst (rn in 3)))
                       (push num-verts new-verts)
                       (incf num-verts)))
            (:vt (progn (setf (veq:2$ uv num-uvs) (veq:from-lst (rn in 2)))
                        (push num-uvs new-uvs)
                        (incf num-uvs)))
            (:f (let* ((triplet (do-face (rn in 3)))
                       (p* (veq:lpos triplet 0))
                       (pvt (veq:lpos triplet 1))
                       (pvn (veq:lpos triplet 2)))
                  (auxin:mvb (p i) (gmsh::add-poly! msh (l+ (m1- p*) (gmsh::gmsh-num-verts msh)))
                    (if p (progn (when (car pvt) (setf (gethash p vt) (gmsh::roll-ind (m1- pvt) i)))
                                 (when (car pvn) (setf (gethash p vn) (gmsh::roll-ind (m1- pvn) i)))
                                 (push p new-polys))
                          (wrn :obj-load "duplicate poly: ~a" p)))))
            (otherwise (push `(,c . ,(car (rn in))) props)))))
      (setf (gmsh::gmsh-num-verts msh) num-verts (gmsh::gmsh-num-uvs msh) num-uvs)
      ; TODO: reverse props to maintain mtl order?
      ; TODO: ignore props, set props, silent
      (unless (find :props ign) (setf (gmsh::gmsh-obj-props msh) props))
      (unless silent
        (lqn:out "~&loaded obj: ~a~%  polys: ~a, verts: ~a, uvs: ~a~%  props: ~a~%"
                 fn (hash-table-count (gmsh::gmsh-polys msh)) num-verts num-uvs props))
      (values msh `((:verts . ,new-verts) (:uvs . ,new-uvs) (:polys . ,new-polys))))))

(defun obj/fn (s)
  (etypecase s (keyword (gmsh::internal-path-string (format nil "src/models/~(~a~)" s)))
               (string s)))

(veq:fvdef obj/load-model (fn
    &key (max-verts 100000) (msh (gmsh:gmsh :max-verts max-verts))
         (center t) (s '(1f0 1f0 1f0)) (xy '(0f0 0f0 0f0))
    &aux (fn (obj/fn fn)))
  (declare ((or string keyword) fn) (gmsh:gmsh msh) (veq:pn max-verts) (list s xy))
  "load this obj file. optional shift, scale, center. see obj/load, obj/save."
  (auxin:mvb (msh ores) (obj/load fn :max-verts max-verts :msh msh)
    (when center (gmsh:center! msh))
    (veq:xlet ((f3!pxy (veq:from-lst xy)) (f3!ps (veq:from-lst s)))
      (gmsh:tx! msh (cdr (assoc :verts ores))
                    (lambda ((:va 3 lxy)) (f3!@+ pxy (f3!@* lxy ps)))))
    (values msh ores)))

; MTL -------------- ; need this in obj file to enable mat files:
; mtllib tmp.mtl ; usemtl tmp
(veq:fvdef obj/save-mtl (msh fn mats &key (colors gmsh:*matpar*)
                                     &aux (fn (auxin::ensure-filename fn ".mtl" t)))
  (declare (ignorable msh) (string fn))
  "export mtl file. see: obj/save."
  (with-open-file (fs fn :direction :output :if-exists :supersede)
    (labels ((wn (k rest) (apply #'format fs "~&~a~@{ ~a~}~%" k rest))
             (ws (k s) (format fs "~&~a ~(~a~)~%" k s)))
      (loop for (name m c) in mats for kd = (subseq (cdr (assoc c colors)) 0 3)
            if (eq m :g) do (ws "newmtl" name)       ; glass
                            (wn "Ns" '(360.0))       (wn "Ka" '(1.0 1.0 1.0))
                            (wn "Kd" kd)             (wn "Ks" '(0.5 0.5 0.5))
                            (wn "Ke" '(0.0 0.0 0.0)) (wn "Ni" '(1.0))
                            (wn "d" '(1.0))          (wn "illum" '(2)) ; map_Kd tmp.jpg
                            (format fs "~%")
            else do         (ws "newmtl" name)       ; diffuse
                            (wn "Ns" '(250.0))       (wn "Ka" '(1.0 1.0 1.0))
                            (wn "Kd" kd)             (wn "Ks" '(0.5 0.5 0.5))
                            (wn "Ke" '(0.0 0.0 0.0)) (wn "Ni" '(1.45))
                            (wn "d" '(1.0))          (wn "illum" '(2))
                            (format fs "~%")))))

; OBJ SAVE --------------
(defun polys->verts (polys) (declare (list polys))
  (loop with ht = (make-hash-table :test #'equal)
        for p in polys
        do (loop for v in p do (unless (gethash v ht) (setf (gethash v ht) t)))
        finally (return (loop for v being the hash-keys of ht collect v))))

(veq:fvdef obj/save (msh fn &key (matfx (lambda (p) (declare (ignorable p)) '(:c :x)))
                                 (propfx (lambda (mc) (declare (ignorable mc)) nil))
                            &aux (fn (auxin::ensure-filename fn ".obj" t))
                                 (vert-offset 0))
  ; remember that there is a global vertex index offset to the file.
  ; can we export vertices only once?
  ; assume the same for normals and uv.
  (declare (gmsh:gmsh msh) (string fn) (function matfx propfx))
  "export obj file. see: obj/load, obj/load-model, obj/save-mtl"
  (labels
    ((ns (s) (if s s "")) (1up (l) (mapcar #'1+ l))
     (remap-poly (inds)
       (loop with ht = (make-hash-table :test #'eql)
             for i in inds for k from 0 do (setf (gethash i ht) k)
             finally (return ht)))
     (re (ht &rest rest) (loop for k in rest collect (+ vert-offset (gethash k ht))))
     (do-export (fs o polys props)
       (let* ((inds (sort (polys->verts polys) #'<))
              (old->new (remap-poly inds))
              (verts (gmsh:get-verts msh inds)))
         (unless polys (format t "~&  o: ~a; ()~%" o) (return-from do-export))
         (format fs "~&o ~a~%" o)
         (loop for (k . v) in props do (format fs "~&~a ~a~%" k v))
         (f3x@$vfx verts (((:va 3 x)) (format fs "~&v ~f ~f ~f~%" x)))
         ; TODO: remap uvs, remap normals, vt
         ; (f2x@vfx (?@ uv 0 num-uvs) (((:va 2 x)) (format fs "vt ~f ~f~%" x)))
         ; (f3x@vfx (?@ verts 0 num-verts) (((:va 3 x)) (format fs "v ~f ~f ~f~%" x)))
         (loop for (v1 v2 v3) in polys
               for (w1 w2 w3) = (re old->new v1 v2 v3)
               for (vt1 vt2 vt3) = (math:nrep 3 "");(1up (gethash p vt))
               ; for (vn1 vn2 vn3) = (1up (gethash p vn))
               do (format fs "~&f ~a/~a/~a ~a/~a/~a ~a/~a/~a~%"
                          (1+ w1) (ns vt1) ""
                          (1+ w2) (ns vt2) ""
                          (1+ w3) (ns vt3) ""))
         (let ((num-verts (veq:3$num verts)))
           (format t "~&  o: ~a; (v: ~a p: ~a)~%" o num-verts (length polys))
           (incf vert-offset num-verts)))))

    (let ((ht (make-hash-table :test #'equal)))
      (with-open-file (fs fn :direction :output :if-exists :supersede)
        (format t "~&writing obj to: ~a~%" fn)
        (labels ((update (p m) (auxin:mvb (m* exists) (gethash m ht)
                                 (declare (ignorable m*) (boolean exists))
                                 (unless exists (setf (gethash m ht) (list)))
                                 (push p (gethash m ht)))))
          (gmsh:itr-polys (msh p) (update p (f@matfx p)))
          (loop for mc being the hash-keys of ht using (hash-value polys)
                for o = (auxin:mkstr (apply #'auxin:mkstr mc) 1)
                do (do-export fs o polys (f@propfx mc)) collect mc
                finally (format t "~&done.~%")))))))

; TODO: :uv, vn, vt normals?
; TODO: this is all rather inefficient
(defun export-data (msh &key meta (matfx (lambda (p) (declare (ignore p)) '(:c :x))))
  (declare (gmsh:gmsh msh) (list meta) (function matfx))
  "serialize gmsh. see gmsh/io:import-data"
  (labels ((do-poly (&aux (res (list))) (gmsh:itr-polys (msh p)
                                          (veq:dsb (m c) (funcall matfx p)
                                            (setf res `(,@p ,m ,c ,@res))))
                                        res)
           (do-vert () (gmsh:get-verts msh (math:range 0 (gmsh:get-num-verts msh)))))
     `((:file . ((:ver . :v1) (:pkg . :gmsh/io))) (:meta . (,@meta))
       (:num-verts . ,(gmsh:get-num-verts msh))
       (:max-verts . ,(gmsh:get-max-verts msh))
       (:num-polys . ,(gmsh:get-num-polys msh))
       (:poly . ,(do-poly)) (:verts . ,(do-vert)))))

(defun import-data (o &key matfx max-verts) ; TODO: count from existing verts, polys
  (declare (list o)) "deserialize gmsh. matfx should accept two arguments, polygon
and a list of (color material). see gmsh/io:export-data"
  (labels ((gk (k) (declare (keyword k)) (cdr (assoc k o))))
    (let ((verts (gk :verts)) (nv (gk :num-verts)) (np (gk :num-polys))
          (msh (gmsh:gmsh :max-verts (the veq:pn (or max-verts (gk :max-verts))))))
      (declare (vector verts) (veq:pn nv np))
      (loop for poly in (veq:group (gk :poly) 5) for p = (subseq poly 0 3)
        do (gmsh:add-poly! msh p)
           (when matfx (apply (the function matfx) p (subseq poly 3))))
      (loop for i from 0 below (veq:3$num verts)
        collect (gmsh:add-vert! msh (veq:3$ verts i)))
      (unless (= (gmsh:get-num-verts msh) nv)
        (wrn :import-data "expected ~a verts, got: ~a." nv (gmsh:get-num-verts msh)))
      (unless (= (gmsh:get-num-polys msh) np)
        (wrn :import-data "expected ~a polys, got: ~a." np (gmsh:get-num-polys msh)))
      (values msh (the list (gk :meta))))))

