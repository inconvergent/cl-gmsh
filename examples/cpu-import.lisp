#!/usr/local/bin/sbcl --script

(require :sb-sprof)
(load "/home/anders/quicklisp/setup.lisp")
(ql:quickload :auxin) (ql:quickload :gmsh)
; (rnd:set-rnd-state 107)
(gmsh/xrend:init 64)

(defvar *size* 1000)

(veq:fvdef load-scene ( &aux (sc (gmsh/scene:scene/load "_gpu-export.gmsh-scene")))
  (print (gmsh/scene:scene-proj sc))
  ; (gmsh:center! (gmsh/scene:scene-msh sc) :max-side 200.0)
  (gmsh/scene:scene/new-canv sc :size *size*)
  (ortho:update (gmsh/scene:scene-proj sc)
                :s (* *size* (ortho:@s (gmsh/scene:scene-proj sc))) ; WHYYYYY?? bad XREND scaling??
                :xy (veq:f2$val (* 0.5 *size*))
                ; :cam (veq:f3$point 200.0 200.0 200.0)
                ; :look (veq:f3$point 0.0 0.0 0.0)
                )
  (print (gmsh/scene:scene-msh sc))
  (loop for lim in '(35.0 35.0)
        do (gmsh/scene::split-edges sc lim) (print (gmsh/scene:scene-msh sc)))
  sc)

; TODO: adjust bg/world color flag. probably with depth in hitmat?

; SIMD:
; >> POLY | 63088901 | 8
; >> BVH | 8923504 | 7
; >> ALL | 35694016 | 8
; file: fuck.png

(veq:fvdef main (size)
  ; (declare (optimize speed (safety 0)))
  (let* ((sc (load-scene))
         (msh (gmsh/scene:scene-msh sc))
         (bvh (gmsh:make-bvh msh :num 8 :mode
                ; :bvh2-stackless
              :bvh4-simd
                :matfx (lambda (p &aux (mc (gmsh/scene:getmat sc p)))
                        ; (veq:from-lst mc)
                        (case (second mc)
                           (:c (values :cc :c)) (:m (values :cc :m)) (:y (values :cc :y))
                           (:w (values :cc :w)) (:k (values :cc :k)))))))

    (print :-----) (print msh) (print bvh)

(sb-sprof:with-profiling (:max-samples 200000
                        ; :mode :cpu
                        ; :mode :alloc
                        :mode :time
                        :report :graph)

    (time (gmsh/xrend:xrend sc bvh :size *size*
                                   :par t :vol t :aa 10
                                  :vmult 13.0
                                  :ao-rep 6
                                  :miss :bgkk
                                  :vdst 500f0
                                  :vlim 50f0
                                  :world :bgkk
                                  :raylen 1200.0
                                  :raylen2 50f0
                                  ))
    )

    ; (veq:vp :poly #1=gmsh/bvh::*poly*  (lqn:size? (lqn:str! #1#)))
    ; (veq:vp :bvh #2=gmsh/bvh::*bvh* (lqn:size? (lqn:str! #2#)))
    ; (veq:vp :all #3=gmsh/bvh::*all* (lqn:size? (lqn:str! #3#)))

    (gmsh/scene:canv/save sc "fuck" :gamma 1.1)
    ))

(main 1000)

; bvh2-stackless:

; :-----
; <@gmsh · 101461 ∇ 193616 ◩ 0>
; <@bvh2-stackless ❦8 # 75035 ↳25 ∇ 193616 s 2.7120202>
; ██ rendering scene at 2000 pix (2000 * 1)
; ██   aa: 10;  ao-rep: 6; vlim: 50.0
; ██   raylen: 1200.0;  max-depth: 12
; ██   0.10 № 200  Δ  0:03.46 ∇  0:31.18 𝛿 3.46  ☰ λ  1.15
; ██ ◦ 0.20 № 400  Δ  0:06.58 ∇  0:26.32 𝛿 3.12  ☰ λ  1.22
; ██ ○ 0.30 № 600  Δ  0:10.76 ∇  0:25.10 𝛿 4.18  ☰ λ  1.12
; ██ ◔ 0.40 № 800  Δ  0:15.84 ∇  0:23.75 𝛿 5.08  ☰ λ  1.01
; ██ ◑ 0.50 № 1000 Δ  0:21.12 ∇  0:21.12 𝛿 5.28  ☰ λ  0.95
; ██ ◕ 0.60 № 1200 Δ  0:25.94 ∇  0:17.29 𝛿 4.82  ☰ λ  0.93
; ██ ❉ 0.70 № 1400 Δ  0:30.92 ∇  0:13.25 𝛿 4.98  ☰ λ  0.91
; ██ ✱ 0.80 № 1600 Δ  0:35.19 ∇  0:08.80 𝛿 4.28  ☰ λ  0.91
; ██ ◉ 0.90 № 1800 Δ  0:38.95 ∇  0:04.33 𝛿 3.76  ☰ λ  0.92
; ██ ● 1.00 № 2000 Δ  0:42.30 ∇  0:00.00 𝛿 3.35  ☰ λ  0.95
; ██
; Evaluation took:
;   42.300 seconds of real time
;   2662.342382 seconds of total run time (2661.646823 user, 0.695559 system)
;   6293.95% CPU
;   156,520,035,732 processor cycles
;   751,312 bytes consed

; bvh4-simd:

; :-----
; <@gmsh · 101461 ∇ 193616 ◩ 0>
; <@bvh4-simd ❦8 # 75035 ↳25 ∇ 193616 s 2.66002>
; ██ rendering scene at 2000 pix (2000 * 1)
; ██   aa: 10;  ao-rep: 6; vlim: 50.0
; ██   raylen: 1200.0;  max-depth: 12
; ██   0.10 № 200  Δ  0:08.90 ∇  1:20.06 𝛿 8.90  ☰ λ  0.45
; ██ ◦ 0.20 № 400  Δ  0:18.64 ∇  1:14.54 𝛿 9.74  ☰ λ  0.43
; ██ ○ 0.30 № 600  Δ  0:27.90 ∇  1:05.10 𝛿 9.26  ☰ λ  0.43
; ██ ◔ 0.40 № 800  Δ  0:36.06 ∇  0:54.08 𝛿 8.16  ☰ λ  0.44
; ██ ◑ 0.50 № 1000 Δ  0:44.94 ∇  0:44.94 𝛿 8.88  ☰ λ  0.45
; ██ ◕ 0.60 № 1200 Δ  0:54.62 ∇  0:36.42 𝛿 9.69  ☰ λ  0.44
; ██ ❉ 0.70 № 1400 Δ  1:03.93 ∇  0:27.40 𝛿 9.31  ☰ λ  0.44
; ██ ✱ 0.80 № 1600 Δ  1:11.25 ∇  0:17.81 𝛿 7.32  ☰ λ  0.45
; ██ ◉ 0.90 № 1800 Δ  1:19.84 ∇  0:08.87 𝛿 8.59  ☰ λ  0.45
; ██ ● 1.00 № 2000 Δ  1:27.82 ∇  0:00.00 𝛿 7.98  ☰ λ  0.46
; ██
; Evaluation took:
;   87.824 seconds of real time
;   5518.548968 seconds of total run time (5517.027934 user, 1.521034 system)
;   6283.65% CPU
;   324,955,673,453 processor cycles
;   751,312 bytes consed
