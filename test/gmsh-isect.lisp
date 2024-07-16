(in-package #:gmsh-tests)

(plan 1)

(subtest "gmsh-isect"
(veq:fvprogn

  (let* ((msh (gmsh/io:obj/load-model :cube)))
    (is (gmsh:plane-slice! msh 0.1f0 0.3f0 0.33738 (veq:f3norm 1f0 1.2f0 -0.3333))
        '(16 15 14 13 12 11 10 9 8))
    (is (gmsh:@all-polys msh)
        '((1 2 3) (5 7 9) (4 5 12) (2 11 6) (2 6 9) (0 3 13) (0 1 3) (4 7 5)
       (0 15 1) (1 12 2) (2 8 3) (4 15 7) (7 8 9) (2 9 8) (5 10 11) (6 11 10)
       (5 11 12) (2 12 11) (6 10 9) (5 9 10) (7 13 8) (3 8 13) (0 14 15)
       (7 15 14) (1 16 12) (4 12 16) (4 16 15) (1 15 16) (7 14 13) (0 13 14))))

 (let ((msh (gmsh/io:obj/load-model :cube)))
    (is (gmsh:@all-polys msh)
        '((1 2 3) (5 7 6) (1 4 5) (2 5 6) (2 6 7) (0 3 7) (0 1 3) (4 7 5) (0 4 1)
          (1 5 2) (2 7 3) (0 7 4)))
    (is (gmsh:plane-sym! msh 0.103f0 0.31f0 0.33738 (veq:f3norm 1f0 -1.2f0 -0.3333))
        '(15 14 13 12 11 10 9 8))

    (let ((bvh1 (gmsh:make-bvh msh :num 2 :mode :bvh2-stackless)))

      (is (gmsh/bvh::m@int/simple-raycast bvh1 ; hit
            (veq:f3 10.0 7f0 4f0) (veq:f3 -10.2 -7f0 -4.1)) 0f0)
      (is (gmsh/bvh::m@int/simple-raycast bvh1 ; hit
            (veq:f3 10.1 7.3f0 4.0f0) (veq:f3 -10.2 -7f0 -4.1)) 0f0)

      (is (veq:lst (gmsh/bvh::m@int/raycast bvh1 ; hit
                     (veq:f3 10.0 7f0 4f0) (veq:f3 -10.2 -7f0 -4.1)))
          '(15 0.88235295))
      (is (veq:lst (gmsh/bvh::m@int/raycast bvh1 ; hit
                     (veq:f3 10.0 7f0 4f0) (veq:f3 -10.9 -7.1f0 -4.3)))
          '(29 0.8359933))))))

(unless (finalize) (error "error in gmsh-isect"))

