
(in-package :gmsh)

(defmacro itr-polys ((msh p &key collect) &body body)
  (declare (symbol msh p) (boolean collect))
  "iterate all polygons as p. optionally collect body."
  `(loop for ,p of-type list being the hash-keys of (gmsh-polys ,msh)
         ,(if collect 'collect 'do)
           (let ((,p ,p)) (declare (list ,p)) (progn ,@body))))

(defmacro itr-verts ((msh i x y z &key collect) &body body)
  (declare (symbol msh x y z) (boolean collect))
  "iterate all vertices as index and pos. optionally collect body."
  (awg (verts) `(loop with ,verts = (gmsh-verts ,msh)
                      for ,i from 0 below (gmsh-num-verts ,msh)
                      ,(if collect 'collect 'do)
                      (veq:mvb (,x ,y ,z) (veq:f3$ ,verts ,i)
                        (declare (veq:ff ,x ,y ,z))
                        (progn ,@body)))))

