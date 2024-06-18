#!/bin/bash

set -e

touch ./gmsh.asd
DEV=1 sbcl --quit \
     --eval '(load "~/quicklisp/setup.lisp")'\
     --eval '(ql:quickload :auxin)'\
     --eval '(ql:quickload :lparallel)'\
     --eval '(ql:quickload :fset)'\
     --eval '(ql:quickload :sdl2)'\
     --eval '(ql:quickload :cl-opengl)'\
     --eval '(ql:quickload :prove)'\
     --eval '(setf lparallel:*kernel* (lparallel:make-kernel 6))'\
     --eval '(handler-case (asdf:test-system :gmsh)
                           (error (c) (print c) (sb-ext:quit :unix-status 5)))'

touch ./gmsh.asd

