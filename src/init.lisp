(in-package #:gmsh)

; from: http://cl-cookbook.sourceforge.net/os.html
(defun vgetenv (name &optional default)
  #+CMU (let ((x (assoc name ext:*environment-list* :test #'string=)))
          (if x (cdr x) default))
  #-CMU (or #+Allegro (sys:getenv name)
            #+CLISP (ext:getenv name)
            #+ECL (si:getenv name)
            #+SBCL (sb-unix::posix-getenv name)
            #+LISPWORKS (lispworks:environment-variable name)
            default))

(defmacro init-config (dev-forms forms)
  (if
      (or (> (length (string-downcase (vgetenv "DEV" ""))) 0)
          #+:gmsh-dev t)
    `(progn ,@dev-forms
            (format t "~&---------!!!!! GMSH COMPILED IN DEVMODE !!!!!---------
--------- ~a~%" ',dev-forms))
    `(progn ,@forms)))

