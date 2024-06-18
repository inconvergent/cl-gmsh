(in-package :gmsh/bvh)
; TODO: name this so i know what it is,
; TODO: make it actually configurable
(declaim (veq:pn +int-leap+ +polyleap+ +int-mima-leap+))
(defconstant +int-leap+ 3)      (defconstant +polyleap+ #.(* 3 3))
(defconstant +int-mima-leap+ 8) (defconstant +simd-int-leap+ 32)
(defvar *stck* (gmsh::make-fast-stack :n 2048))
(defmacro make-bb () `(veq:f3$line 90000f0 -90000f0 90000f0 -90000f0 90000f0 -900000f0))

