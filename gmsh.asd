; (setf *features* `(:veq-simd ,@*features*))
; (declaim (optimize compilation-speed))

(asdf:defsystem #:gmsh
  :description "mesh thing"
  :version "1.2.0"
  :author "anders hoff / @inconvergent / inconvergent@gmail.com"
  :licence "MIT"
  :in-order-to ((asdf:test-op (asdf:test-op #:gmsh/tests)))
  :pathname "src/" :serial nil
  :depends-on (#:uiop #:sb-simd
               #:auxin
               #:cl-opengl #:cl-glu #:sdl2)
  :components ((:file "packages")
               (:file "init" :depends-on ("packages"))
               (:file "utils" :depends-on ("init"))
               (:file "docs" :depends-on ("utils"))
               (:file "config" :depends-on ("utils"))
               (:file "macros" :depends-on ("config"))

               (:file "gmsh/gmsh" :depends-on ("macros"))
               (:file "gmsh/isect" :depends-on ("gmsh/gmsh"))
               (:file "gmsh/io" :depends-on ("gmsh/gmsh"))

               (:file "gmsh/bvh-init" :depends-on ("gmsh/gmsh")) ; ???
               (:file "gmsh/bvh-types" :depends-on ("gmsh/bvh-init"))
               (:file "gmsh/bvh-sah-split" :depends-on ("gmsh/bvh-types"))
               (:file "gmsh/bvh-stack" :depends-on ("gmsh/bvh-sah-split")) ; dep?/
               (:file "gmsh/bvh-stack-simd" :depends-on ("gmsh/bvh-sah-split"))
               (:file "gmsh/bvh-checks" :depends-on ("gmsh/bvh-stack"
                                                     "gmsh/bvh-stack-simd"))
               (:file "gmsh/bvh" :depends-on ("gmsh/bvh-checks"))
               (:file "gmsh/bvh-raycast" :depends-on ("gmsh/bvh"))
               (:file "gmsh/bvh-raycast-simd" :depends-on ("gmsh/bvh"))
               (:file "gmsh/scene" :depends-on ("gmsh/bvh"))
               (:file "gmsh/cpu/render-utils"
                :depends-on ("gmsh/scene"
                             "gmsh/bvh-raycast" "gmsh/bvh-raycast-simd"
                             "gmsh/bvh" "gmsh/gmsh"))
               (:file "gmsh/cpu/render" :depends-on ("gmsh/cpu/render-utils"))
               (:file "gmsh/gpu/gl" :depends-on ("gmsh/bvh" "gmsh/gmsh"))

               (:file "voxel/init" :depends-on ("gmsh/gmsh"))
               (:file "voxel/voxel" :depends-on ("voxel/init"))))

(asdf:defsystem #:gmsh/tests
  :depends-on (#:gmsh #:prove)
  :version "1.2.0"
  :perform (asdf:test-op (o s) (uiop:symbol-call ':gmsh-tests '#:run-tests))
  :pathname "test/" :serial t
  :components ((:file "run")))

