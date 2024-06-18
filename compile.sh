#!/bin/bash

set -e
touch ./gmsh.asd
time sbcl --quit \
           --eval '
  (handler-case (time (ql:quickload :gmsh :verbose t))
  (error (c) (print c) (sb-ext:quit :unix-status 2)))'\
  >compile.sh.log 2>&1
