(in-package :gmsh/xrend)

(defmacro falloff (b e) `(exp (- (expt (abs ,b) ,e))))
; (defmacro rc-simple (&rest rest) "simple raycast using current raycaster."
;   `(gmsh/bvh:simd4/simple-raycast ,@rest))
; (defmacro rc (&rest rest) "raycast using current raycaster."
;   `(gmsh/bvh:simd4/raycast ,@rest))

(defmacro rc-simple (&rest rest) `(gmsh/bvh:int/simple-raycast ,@rest))
(defmacro rc (&rest rest) `(gmsh/bvh:int/raycast ,@rest))

; TODO: config object?
(veq:vdef xrend (sc bvh &key (par t) (size 1000) (bs 1) (aa 1) (vol nil)
                             (raylen 2000.0) (raylen2 (* 0.5 raylen))
                             (max-depth 12)
                             (ao-rep 4)
                             (vmult 64.0)
                             (vexpt 1.45)
                             (vdst 1000f0)
                             (vlim 0.01)
                             (vrec 6)
                             (miss :bgk) (world miss)
                             (ao-mult (veq:ff (/ ao-rep)))
                             (aa-mult (veq:ff (/ aa)))     ; used in wrapper
                             (vdepth (if vol 1 0)))        ; used in wrapper
  (declare #.*opt1* (gmsh/scene:scene sc) (gmsh/bvh:bvh bvh)
                    (veq:pn bs aa size max-depth ao-rep vrec vdepth)
                    (keyword world miss)
                    (veq:ff raylen raylen2 ao-mult
                            vmult vlim vexpt vdst)
                    (boolean vol par))
  "render scene from this scene/bvh.

use (gmsh/xrend:init 4) before calling render to initialize parallel cores:

keywords:
 - aa     : rendering fidelity. higher is slower.
 - raylen : ray length for sampling. not volume sampling.
 - vdst   : ray length for sampling volume light. [:ll mat]
 - vmult  : volume light brightness. [:ll mat]
 - vexpt  : volume light distance falloff. higher is brighter.
 - vdepth : disable volume sampling after vdepth ray bounces.
 - vrec   : max recursive depth in volume sampling. higher is more detailed.
 - vlim   : volume sampling min recursive sampling step size.
            lower is more detailed. [0.0, 1.0]
 - vol    : enable disable volumetric light. [:ll mat]
 - par    : use parallelism.

  TODO: ; ve = 1.2 -> less fog; ve = 0.3 -> more
" (render-wrap ; NOTE: requires scene: sc ; TODO: rewrite?
    ((do-ll (hi (:va 3 origin pt dir))
       (declare (veq:in hi) (veq:ff origin pt dir))
       (veq:xlet ((f3!far (if (< -1 hi) (veq:f3 pt)
                                        (veq:f3from origin dir vdst))))
         (labels ((ll-rec (lvl a b (:va 3 fa fb))
                       (declare (veq:pn lvl) (veq:ff a b fa fb))
                   (veq:xlet ((f!sd (* 0.5 (- b a))))
                     (if (or (> lvl vrec) (< sd vlim))
                         (f3!@*. (f3!@+ fa fb) sd)
                         (veq:xlet ((p!num (1+ lvl))
                                    (f!mid (rndrng a b))
                                    (f3!fm (ll-sample mid)))
                          (f3!@+ (ll-rec num a mid fa fm)
                                 (ll-rec num mid b fm fb))))))
                  (ll-sample (q) (declare (veq:ff q))
                    (veq:xlet ((f3!php (veq:f3lerp origin far q)))
                      (veq:mvb (hi hs) (rc bvh php (f3!@+ php (3on-sphere vdst)))
                        (declare (fixnum hi) (veq:ff hs))
                        (veq:mvb (flag (:va 3 rgb)) (hitmat-simple bvh hi :bgkk)
                          (declare (symbol flag) (veq:ff rgb))
                          (if (and (eq flag :ll) (< #.veq:*eps* hs))
                              (f3!@*. rgb (abs (exp (- (expt (abs hs) vexpt)))))
                              (veq:f3val 0.0)))))))
           (m@ll-rec 0 #1=0.01 #2=0.999
                          (ll-sample #1#) (ll-sample #2#)))))

     (do-ao (hi (:va 3 pt dir)) ; ambient occlusion
       (declare (veq:in hi) (veq:ff pt dir))
       (veq:xlet ((f!rl (* raylen2 (expt (srnd:rnd* *rs* 1f0) 2.0)))
                  (f3!hn (get-normal bvh hi dir))
                  (f3!hp* (veq:f3from pt hn 0.001))
                  (f3!hnrl (f3!@*. hn rl)))
         (min 1f0 (* (loop repeat ao-rep
                           summing (abs (the veq:ff
                                          (rc-simple bvh hp*
                                            (f3!@+ (3on-sphere rl) hnrl)))))
                     ao-mult))))

     (do-rr (depth hi (:va 3 rgb pt dir)) ; reflection
       (declare (veq:pn depth) (veq:in hi) (veq:ff rgb pt dir))
       (veq:xlet ((f3!hn (get-normal bvh hi dir))
                  (f3!hp* (veq:f3from pt hn 0.001)))
         (f3!@+ (f3!@*. rgb 0.75) ; TODO: config factor
         (f3!@*. (m@do-render (1+ depth) hp*
                   (m@reflect dir
                     (veq:f3norm (f3!@+ (3in-sphere 0.05) hn))))
                 0.25))))

     (do-ro (depth hi (:va 3 rgb pt dir)) ; ao * rr
       (declare (veq:pn depth) (veq:in hi) (veq:ff rgb pt dir))
       (f3!@*. (do-rr depth hi rgb pt dir)
               (do-ao hi pt dir))))))

