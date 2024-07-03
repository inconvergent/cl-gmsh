#!/usr/local/bin/sbcl --script

(load "/home/anders/quicklisp/setup.lisp") (require :sb-sprof)
(ql:quickload :auxin) (ql:quickload :gmsh)
(gmsh/xrend:init 1)

(in-package :gmsh/bvh)

(defvar *rays* 10000)

(veq:fvdef compare-bvh-simple-rt (num msh &aux (ok12 0) (hit1 0) (hit2 0)
                                               (hit3 0) (hit4 0) (hit5 0))
  (loop
        with bvh2 = (gmsh:make-bvh msh :num num :mode :bvh2-stackless)
        with bvh4-simd = (gmsh:make-bvh msh :num (/ num 2) :mode :bvh4-simd
                                        :sort-lvl 100
                                        :sort-num num
                                        )
        with rs = (srnd:srnd 2)
        initially
                  (print bvh2) (print bvh4-simd)
                  ; (print (subseq (bvh-nodes bvh4-simd) 0 15))
                  ; (veq:4$print (bvh-int-nodes bvh4-simd) :n 32)
                  (veq:$print (bvh-simd-nodes bvh4-simd) :dim 8 :n 22)
        for i from 0 repeat *rays*

        do (list 11)
           (veq:xlet ((f3!a (srnd:3in-sphere rs 4f0))
                      (f3!b (srnd:3in-sphere rs 10f0))
                      (f3!ab (f3!@- b a))
                      ; (h1 (gmsh/bvh:int/simple-raycast bvh2 a ab))
                      ; (h2 (gmsh/bvh:simd4/simple-raycast bvh4-simd a ab))
                      (h3 (gmsh/bvh:int/raycast bvh2 a ab))
                      (h4 (gmsh/bvh:simd4/raycast bvh4-simd a ab))
                      )


              ; (when (and (= h1 h2) (= h3 h4 h5)) (incf ok12))
              ; (when (< h1 0.1) (incf hit1))
              ; (when (< h2 0.1) (incf hit2))

              (when (> h3 -1) (incf hit3))
              (when (> h4 -1) (incf hit4))
              (when (= h3 h4) (incf ok12))
              ; (when (> h5 -1) (incf hit5))
              )
           )
  (list ok12 hit1 hit2 hit3 hit4 hit5))

  (let* ((msh (gmsh/io:obj/load-model :ico)))

    ; (veq:vpr (compare-bvh-simple-rt 2 msh))
    ; '(5000 2226 2226 2226 2226 2226)
    ; (veq:vpr (compare-bvh-simple-rt 3 msh))
    (veq:vpr (compare-bvh-simple-rt 4 msh))
    ; (veq:vpr (compare-bvh-simple-rt 5 msh))
    ; (veq:vpr (compare-bvh-simple-rt 8 msh))
    )

; (handler-case
;   (progn
; (lqn:out "   POLY")

; (veq:vp :-----poly *poly* (veq:ff (/ *poly* *rays*)))
; (veq:vp :simd-poly *simd-poly* (veq:ff (/ *simd-poly* *rays*)))
; (veq:vp :--------- (veq:ff (/ *poly* *simd-poly* )))

; (lqn:out "   BVH")

; (veq:vp :------bvh *bvh* (veq:ff (/ *bvh* *rays*)))
; (veq:vp :-simd-bvh *simd-bvh* (veq:ff (/ *simd-bvh* *rays*)))
; (veq:vp :--------- (veq:ff (/ *bvh* *simd-bvh* )))
; (lqn:out "done")

; ; (veq:vpr *xxx* *yyy*)

;     )
;   (error (e) (warn "some err: ~a" e))

;   )



