
(asdf:defsystem #:gmsh
  :description "triangular mesh and raytracer utilities."
  :version "2.0.0"
  :author "anders hoff / @inconvergent / inconvergent@gmail.com"
  :licence "MIT"
  :in-order-to ((asdf:test-op (asdf:test-op #:gmsh/tests)))
  :pathname "src/" :serial nil
  :depends-on (#:uiop #:sb-simd #:auxin #:lparallel #:fset #:cl-opengl #:cl-glu #:sdl2)
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
               (:file "gmsh/cam" :depends-on ("macros"))
               (:file "gmsh/scene" :depends-on ("gmsh/cam" "gmsh/bvh"))
               (:file "render/utils"
                :depends-on ("gmsh/scene"
                             "gmsh/bvh-raycast" "gmsh/bvh-raycast-simd"
                             "gmsh/bvh" "gmsh/gmsh"))
               (:file "render/render" :depends-on ("render/utils"))
               (:file "gl/gl" :depends-on ("gmsh/bvh" "gmsh/gmsh"))

               (:file "voxel/init" :depends-on ("gmsh/gmsh"))
               (:file "voxel/voxel" :depends-on ("voxel/init"))))

(asdf:defsystem #:gmsh/tests
  :depends-on (#:uiop #:sb-simd #:auxin #:lparallel #:fset #:cl-opengl #:cl-glu #:sdl2 #:gmsh)
  :version "2.0.0"
  :perform (asdf:test-op (o s) (uiop:symbol-call ':gmsh-tests '#:run-tests))
  :pathname "test/" :serial t
  :components ((:file "run")))

