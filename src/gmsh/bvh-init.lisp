(in-package :gmsh/bvh)
; TODO: name this so i know what it is,
; TODO: make it actually configurable
(declaim (veq:pn +int-leap+ +polyleap+ +int-mima-leap+))
(defconstant +int-leap+ 3)      (defconstant +polyleap+ #.(* 3 3))
(defconstant +int-mima-leap+ 8) (defconstant +simd-mima-leap+ 32)

(declaim (veq:ff *bvhlo* *bvhhi*))
(defvar *bvhhi* 9f19)
(defvar *bvhlo* (- *bvhhi*))

(defmacro make-bb () `(veq:f3$line *bvhhi* *bvhlo* *bvhhi* *bvhlo* *bvhhi* *bvhlo*))
(defmacro reset-bb! (b*)
  `(let ((b ,b*)) (declare (veq:fvec b))
     (setf (aref b 0) *bvhhi* (aref b 1) *bvhlo* (aref b 2) *bvhhi*
           (aref b 3) *bvhlo* (aref b 4) *bvhhi* (aref b 5) *bvhlo*)))

