
(defpackage #:gmsh-tests (:use #:cl #:prove) (:export #:run-tests))

(setf prove:*enable-colors* nil)

(in-package #:gmsh-tests)

(setf lparallel:*kernel* (lparallel:make-kernel 4))

(defun -run-tests (files)
  (loop with fails = 0
        for f in files
        do (format t "~&~%starting tests in: ~a~%" (gmsh::mkstr f))
           (unless (prove:run f :reporter :fiveam)
                   (incf fails))
           (format t "~&done: ~a~%" (gmsh::mkstr f))
        finally (return (unless (< fails 1)
                          (sb-ext:quit :unix-status 7)))))

(defun run-tests ()
  (-run-tests '(
                #P"test/gmsh.lisp"
                #P"test/gmsh-rc.lisp"
                #P"test/gmsh-isect.lisp"
                )))

(defmacro is-arr (&rest rest) `(is ,@rest :test #'equalp))

(defmacro model (m)
  `(gmsh::internal-path-string
     ,(format nil "src/models/~a" (string-downcase (gmsh::mkstr m)))))

