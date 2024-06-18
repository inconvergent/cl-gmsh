
(in-package :gvox)

(declaim (inline -set-imap))
(defun -set-imap (imap ix iy iz)
  (declare #.*opt* (veq:pvec imap) (veq:pn ix iy iz))
  (loop for i of-type veq:pn from 0 below 24 by 3
        do (setf (aref imap i) (+ ix (aref *offsets* i))
                 (aref imap (1+ i)) (+ iy (aref *offsets* (1+ i)))
                 (aref imap (+ i 2)) (+ iz (aref *offsets* (+ i 2))))))

; old test: (when (and  (>= hi va lo)) (incf ind (expt (abs 2) b)))
(declaim (inline -get-cubeindex))
(defun -get-cubeindex (a imap fx)
  (declare #.*opt* (veq:fvec a) (veq:pvec imap) (function fx))
  (loop with ind of-type veq:pn = 0
        for i of-type veq:pn from 0 below 24 by 3
        for b of-type (integer 0 24) from 0
        if (funcall fx (aref a (aref imap i) (aref imap (1+ i)) (aref imap (+ i 2))))
        do (incf ind (the veq:pn (expt (abs 2) b)))
        finally (return ind)))

(declaim (inline -set-voxel-list))
(defun -set-voxel-list (voxellist ec)
  (declare #.*opt* (veq:pvec voxellist) (veq:pn ec))
  (loop for i of-type veq:pn from 0 below 12
        for k of-type veq:pn from 0 by 2
        for pow of-type veq:pn = (expt (abs 2) i)
        if (= (logand ec pow) pow)
        do (setf (aref voxellist k) (aref *cubeind* k)
                 (aref voxellist (1+ k)) (aref *cubeind* (1+ k)))))

(declaim (inline -do-global-edge))
(defun -do-global-edge (imap v &aux (3v (* 3 v)))
  (declare #.*opt* (veq:pvec imap) (veq:pn v 3v))
  (list (aref imap 3v) (aref imap (1+ 3v)) (aref imap (+ 3v 2))))

(declaim (inline -get-pos-mid))
(veq:fvdef -get-pos-mid (a e)
  (declare #.*opt* (ignore a) (list e))
  (veq:dsb (ev1 ev2) e
    (declare (list ev1 ev2))
    (f3!@+ (veq:f3mid (veq:ffl ev1) (veq:ffl ev2))
           (veq:f3rep *shift*))))


(veq:fvdef* -intersect ((:va 3 p1 p2) av1 av2 &aux (av1 (abs av1)) (av2 (abs av2)))
  (declare #.*opt* (veq:ff p1 p2 av1 av2))
  (if (< (abs (- av2 av1)) *eps*)
    (veq:f3mid p1 p2)
    (veq:f3iscale (f3!@+ (veq:f3scale p1 av1) (veq:f3scale p2 av2))
                  (+ av1 av2))))


; (declaim (inline -get-pos-weighted))
(veq:fvdef -get-pos-weighted (a e)
  (declare #.*opt* (veq:fvec a) (list e))
  (veq:dsb (ev1 ev2) e
    (declare (list ev1 ev2))
    (f3!@+ (-intersect (veq:ffl ev1) (veq:ffl ev2)
                         (apply #'aref a ev1) (apply #'aref a ev2))
             (veq:f3rep *shift*))))

(defun set-voxels (dims fx)
  (declare #.*opt* (list dims) (function fx))
  " dims = (list nx ny nz). (funcall fx x y z) "
  (veq:dsb (nx ny nz) dims
    (declare (veq:pn nx ny nz))
    (loop with voxs = (make dims)
          for x of-type veq:pn from 0 below nx
          do (loop for y of-type veq:pn from 0 below ny
                   do (loop for z of-type veq:pn from 0 below nz
                            do (setvoxel voxs x y z (funcall fx x y z))))
          finally (return voxs))))

(declaim (inline -single-ind))
(defun -single-ind (v)
  (declare #.*opt* (list v))
  (veq:dsb (x y z) v
    (declare (veq:pn x y z))
    (+ x (the veq:pn (* *max-voxels*
                        (the veq:pn (+ y (the veq:pn (* *max-voxels* z)))))))))

(declaim (inline -hash-edge))
(defun -hash-edge (edge)
  (declare #.*opt* (list edge))
  (veq:dsb (a b) edge
    (declare (list a b))
    (let ((a (-single-ind a)) (b (-single-ind b)))
      (declare (veq:pn a b))
      (if (< a b) (list a b) (list b a)))))

(declaim (inline -add-poly))
(defun -add-poly (a edge->vert msh tri posfx)
  (declare #.*opt* (veq:fvec a) (hash-table edge->vert) (list tri))
  (gmsh:add-poly! msh
    (loop for e of-type list in tri
          for h of-type list = (-hash-edge e)
          collect (veq:mvb (v exists) (gethash h edge->vert)
                    (declare (boolean exists))
                    (if exists v (setf (gethash h edge->vert)
                                       (gmsh:add-vert! msh
                                         (veq:mvc (the function posfx) a e)))))
            of-type veq:pn)))

(declaim (inline -make-poly))
(defun -make-poly (imap voxellist cubeindex i)
  (declare #.*opt* (veq:pvec imap voxellist) (veq:pn cubeindex i))
  (loop for k of-type veq:pn from 0 below 3
        for i2 of-type veq:pn = (* 2 (aref *triangles* cubeindex (+ i k)))
        collect (list (-do-global-edge imap (aref voxellist i2))
                      (-do-global-edge imap (aref voxellist (1+ i2))))
          of-type list))

(declaim (inline -add-polys))
(defun -add-polys (a edge->vert imap voxellist cubeindex msh posfx)
  (declare #.*opt* (veq:fvec a) (veq:pvec imap voxellist)
                   (veq:pn cubeindex) (hash-table edge->vert))
  (loop for i of-type veq:pn from 0 by 3
        until (= (aref *triangles* cubeindex i) 99)
        do (-add-poly a edge->vert msh (-make-poly imap voxellist cubeindex i)
                      posfx)))


; TODO: pass function or hash table to set material or set new triangles/edges
(defun get-mesh (msh voxs &key w (fx (lambda (v) (declare (veq:ff v)) (>= 0.0 v ))))
  (declare #.*opt* (voxels voxs) (boolean w) (function fx))
  "reconstruct mesh surounding (fx ...) == t."
  (let ((imap (veq:p$zero 24))
        (voxellist (veq:p$zero 24))
        (a (voxels-a voxs))
        (edge->vert (make-hash-table :test #'equal :size 2048 :rehash-size 2f0))
        (polys (list))
        (posfx (if w #'-get-pos-weighted #'-get-pos-mid)))
    (declare (veq:pvec voxellist imap) (veq:fvec a) (hash-table edge->vert)
             (function posfx))
    (loop for ix of-type veq:pn from 0 to (voxels-nx voxs)
          do (loop for iy of-type veq:pn from 0 to (voxels-ny voxs)
                   do (loop for iz of-type veq:pn from 0 to (voxels-nz voxs)
                            do (-set-imap imap ix iy iz)
                               (let* ((cubeindex (-get-cubeindex a imap fx))
                                      (ec (aref *edges* cubeindex)))
                                 (declare (veq:pn ec cubeindex))
                                 (unless (or (= ec 0) (= ec 255))
                                   (-set-voxel-list voxellist ec)
                                   (-add-polys a edge->vert imap voxellist
                                               cubeindex msh posfx))))))
    polys))

