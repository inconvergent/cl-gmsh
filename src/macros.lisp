
(in-package :gmsh)

(defmacro reorder (a &rest rest) ; NOTE: not very efficient
  (declare (symbol a)) "new vector with these elements/ranges."
  `(concatenate 'vector
    ,@(loop for ind in rest collect
        (etypecase ind
          (number `(list (aref ,a ,ind)))
          (symbol `(list (aref ,a ,ind)))
          (cons (ecase (length ind) (1 `(list (aref ,a ,@ind)))
                                    (2 `(subseq ,a ,@ind))))))))

(defmacro dsbmapc ((data vars) &body body &aux (g (gensym "DSBMAPC")))
  (etypecase vars
    (symbol `(mapc (lambda (,vars) (progn ,@body)) ,data))
    (cons `(mapc (lambda (,g) (auxin:dsb ,vars ,g ,@body)) ,data))))

(defmacro itr/polys ((msh p &key collect) &body body)
  (declare (symbol msh p) (boolean collect) (ignorable p))
  "iterate all polygons as p. optionally collect body."
  `(loop for ,p of-type list being the hash-keys of (gmsh-polys ,msh)
         ,(if collect 'collect 'do)
           (let ((,p ,p)) (declare (list ,p)) (progn ,@body))))

(defmacro itr/verts ((msh i x y z &key collect) &body body)
  (declare (symbol msh x y z) (boolean collect) (ignorable i x y z))
  "iterate all vertices as index and pos. optionally collect body."
  (awg (verts) `(loop with ,verts = (gmsh-verts ,msh)
                      for ,i from 0 below (gmsh-num-verts ,msh)
                      ,(if collect 'collect 'do)
                      (veq:mvb (,x ,y ,z) (veq:f3$ ,verts ,i)
                        (declare (veq:ff ,x ,y ,z))
                        (progn ,@body)))))

(defmacro $verts (msh &rest rest) ; TODO: this should go somewhere else.
  "get coordinates of vert i, j, ... as (values i1 i2 .. j1 j2 ..)."
  `(veq:fvprogn (veq:f3$ (gmsh-verts ,msh) ,@rest)))

; (defun msk/set! (msk inds &optional (v 0) &aux (n (length msk)))
;   (declare (veq:pvec msk) (sequence inds) (veq:pn v n))
;   (etypecase inds
;      (list (loop for i in inds if (< i n) do (setf (aref msk i) v)))
;      (vector (loop for i across inds if (< i n) do (setf (aref msk i) v))))
;   msk)

; TODO: protect mask m/i or make them configurable?
; TODO: out of bounds protections
(defmacro msk/tx! (msh pos msk &body body) (declare (symbol msh pos))
  "apply this transform to verts where msk is not 0"
  (auxin:awg (verts)
    `(loop with ,verts of-type veq:fvec = (gmsh-verts ,msh)
           for gmsh::m across ,msk for gmsh::i from 0 if (not (zerop gmsh::m))
           do (veq:fvprogn (veq:mvb ((:va 3 ,pos)) ($verts ,msh gmsh::i)
                (declare (veq:ff ,pos))
                (setf (veq:3$ ,verts gmsh::i) (progn ,@body)))))))

(defmacro msk/v (msh pos &body body) (declare (symbol msh pos))
  "return veq:pvec mask"
  (auxin:awg (verts inds)
    `(let ((,verts (gmsh-verts ,msh)) (,inds (vrange (@vnum ,msh))))
       (declare (veq:fvec ,verts) (veq:pvec ,inds))
       (lparallel:pmap-into ,inds
         (lambda (gmsh::i) (declare (veq:pn gmsh::i))
           (veq:fvprogn (veq:mvb ((:va 3 ,pos)) (veq:3$ ,verts gmsh::i)
                         (declare (veq:ff ,pos))
                         (if (progn ,@body) 1 0))))
         ,inds))))

