(in-package #:gmsh-tests)

(plan 1)

(veq:fvdef compare-bvh-simple-rt (num msh &aux (ok12 0) (hit1 0) (hit2 0)
                                               (hit3 0) (hit4 0) (hit5 0))
  (loop with bvh2 = (gmsh:make-bvh msh :num num :mode :bvh2-stackless)
        with bvh4-simd = (gmsh:make-bvh msh :num num :mode :bvh4-simd)
        with rs = (srnd:srnd 2)
        for i from 0 repeat 5000
        do (veq:xlet ((f3!a (srnd:3in-sphere rs 4f0))
                      (f3!b (srnd:3in-sphere rs 4f0))
                      (f3!ab (f3!@- b a))
                      (h1 (gmsh/bvh::m@int/simple-raycast bvh2 a ab))
                      (h2 (gmsh/bvh::m@simd4/simple-raycast bvh4-simd a ab))
                      (h3 (gmsh/bvh::m@int/raycast bvh2 a ab))
                      (h4 (gmsh/bvh::m@simd4/raycast bvh4-simd a ab))
                      ; int/*raycast should work with :bvh4-simd version too:
                      (h5 (gmsh/bvh::m@int/raycast bvh4-simd a ab)))
              (when (and (= h1 h2) (= h3 h4 h5)) (incf ok12))
              (when (< h1 0.1) (incf hit1))
              (when (< h2 0.1) (incf hit2))

              (when (> h3 -1) (incf hit3))
              (when (> h4 -1) (incf hit4))
              (when (> h5 -1) (incf hit5))))
  (list ok12 hit1 hit2 hit3 hit4 hit5))

(subtest "raycast 1"
  (let* ((msh (gmsh/io:obj/load-model :teapot)))
    (is (compare-bvh-simple-rt 2 msh) '(5000 2226 2226 2226 2226 2226))
    (is (compare-bvh-simple-rt 3 msh) '(5000 2226 2226 2226 2226 2226))
    (is (compare-bvh-simple-rt 4 msh) '(5000 2226 2226 2226 2226 2226))
    (is (compare-bvh-simple-rt 5 msh) '(5000 2226 2226 2226 2226 2226))
    (is (compare-bvh-simple-rt 8 msh) '(5000 2226 2226 2226 2226 2226))
    (is (compare-bvh-simple-rt 9 msh) '(5000 2226 2226 2226 2226 2226))
    (is (compare-bvh-simple-rt 16 msh) '(5000 2226 2226 2226 2226 2226))))

(unless (finalize) (error "error in raycast, gmsh-rc"))

