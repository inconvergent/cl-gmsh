(in-package :gmsh/bvh)
; TODO: name this so i know what it is,
; TODO: make it actually configurable
(declaim (veq:pn +int-leap+ +polyleap+ +int-mima-leap+))
(defconstant +int-leap+ 3)      (defconstant +polyleap+ #.(* 3 3))
(defconstant +int-mima-leap+ 8) (defconstant +simd-mima-leap+ 32)

(declaim (veq:ff *bvhlo* *bvhhi*))
(defvar *bvhhi* 9f19)
(defvar *bvhlo* (- *bvhhi*))

