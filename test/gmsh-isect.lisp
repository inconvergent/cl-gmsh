(in-package #:gmsh-tests)

(plan 1)

(subtest "gmsh obj/bvh"
(veq:fvprogn

  (let* ((msh (gmsh:obj-load (model :cube) :silent t)))
    (is (gmsh:plane-split msh 0.1f0 0.3f0 0.33738 (veq:f3norm 1f0 1.2f0 -0.3333))
        '(16 15 14 13 12 11 10 9 8))
    (is (gmsh:get-all-polys msh)
        '((1 2 3) (5 7 9) (4 5 15) (2 13 6) (2 6 9) (0 3 11) (0 1 3) (4 7 5)
          (0 16 1) (1 15 2) (2 8 3) (4 16 7) (7 8 9) (2 9 8) (7 10 11) (0 11 10)
          (6 12 9) (5 9 12) (5 12 13) (6 13 12) (1 14 15) (4 15 14) (0 10 16)
          (7 16 10) (7 11 8) (3 8 11) (5 13 15) (2 15 13) (4 14 16) (1 16 14))))

 (let ((msh (gmsh:obj-load (model :cube) :silent t)))
    (is (gmsh:get-all-polys msh)
        '((1 2 3) (5 7 6) (1 4 5) (2 5 6) (2 6 7) (0 3 7) (0 1 3) (4 7 5) (0 4 1)
          (1 5 2) (2 7 3) (0 7 4)))
    (is (gmsh:plane-sym msh 0.103f0 0.31f0 0.33738 (veq:f3norm 1f0 -1.2f0 -0.3333))
        '((1 2 3) (1 4 14) (0 3 11) (0 1 3) (0 4 1) (1 15 2) (2 9 3) (0 10 4)
          (2 8 9) (0 11 10) (2 12 8) (4 13 14) (1 14 15) (4 10 13) (3 9 11)
          (2 15 12)))

    (let ((bvh1 (gmsh:make-bvh msh :num 2 :mode :bvh2-stackless)))

      (is (gmsh/bvh::int/simple-raycast bvh1 ; hit
            (veq:f3 10.0 7f0 4f0) (veq:f3 -10.2 -7f0 -4.1)) 0f0)
      (is (gmsh/bvh::int/simple-raycast bvh1 ; hit
            (veq:f3 10.1 7.3f0 4.0f0) (veq:f3 -10.2 -7f0 -4.1)) 0f0)

      (is (veq:lst (gmsh/bvh::int/raycast bvh1 ; hit
                     (veq:f3 10.0 7f0 4f0) (veq:f3 -10.2 -7f0 -4.1)))
          '(12 0.88235295))
      (is (veq:lst (gmsh/bvh::int/raycast bvh1 ; hit
                     (veq:f3 10.0 7f0 4f0) (veq:f3 -10.9 -7.1f0 -4.3)))
          '(31 0.8359933))
    )
    )))

(unless (finalize) (error "error in gmsh-isect"))

