(in-package :gmsh)

(declaim (single-float *eps*)
         (veq:pn *wrnlimit*) (hash-table *wrncnt*) (boolean *dev*)
         (vector *programs*) (list *mats* *matpar* +nilpol+))
(defparameter *eps* #.(* 1f0 single-float-epsilon))
(defparameter +nilpol+ '(0 0 0))

(defvar *wrnlimit* 10) ; TODO: clear-wrn , all key?
(defvar *wrncnt* (make-hash-table :test #'eql))
(defmacro wrn (k s &rest rest) (declare (keyword k) (string s))
  "show limited number of warnings. counts by k. shows gmsh::*wrnlimit* warnings for each k."
  `(veq:xlet ((p!cnt (gethash ,k gmsh:*wrncnt* 0)))
     (when (< cnt gmsh::*wrnlimit*) (warn ,(auxin:mkstr "~(~a (~a): ~)" s) ,k cnt ,@rest))
     (setf (gethash ,k gmsh:*wrncnt*) (1+ cnt))))

(init-config
  ((defvar *opt*  '(optimize (safety 3) (speed 3) (debug 3) (space 0))) ; dev
   (defvar *opt1* '(optimize (safety 3) (speed 3) (debug 3) (space 0)))
   (defvar *dev* t))

  ((defvar *opt*  '(optimize (safety 1) (speed 3) (debug 1) (space 2))) ; prod
   (defvar *opt1* '(optimize (safety 0) (speed 3) (debug 0) (space 2)))
   (defvar *dev* nil)))

(defvar *programs* #(:std :flat))
(defvar *mats* '((:c . 0) (:l . 1) (:ao . 2)))
(defvar *matpar*
  '((:x   0.7 0.4 0.3  1.0)
    (:c   0.0 1.0 1.0  1.0) (:m  1.0 0.0 1.0  1.0) (:y  1.0 1.0 0.0  1.0)
    (:r   1.0 0.0 0.0  1.0) (:g  0.0 1.0 0.0  1.0) (:b  0.0 0.0 1.0  1.0)
    (:k   0.0 0.0 0.0  1.0) (:w   1.0 1.0 1.0  1.0)))

; Can we make this work?
; (defun inline? (&rest rest) (print (if *dev* nil `(declaim (inline ,@rest)))))
; (print (list :dev *dev*))
