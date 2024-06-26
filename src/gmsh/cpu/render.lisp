(in-package :gmsh/xrend)
; (declaim (inline ao-sample-light do-ao do-cr ll-sample-volume))

(veq:fvdef ao-sample-light (bvh (:va 3 hp d hn))
  (declare #.*opt* (gmsh/bvh:bvh bvh) (veq:ff hp d hn))
  (veq:xlet ((f3!v 0)
             (f!dt (* #.(/ 3.0) (abs (veq:f3dot (veq:f3norm d) hn)))))
     (loop repeat 2
          do  (f3!@+.! v (* 0.5 dt (+ (rc-simple bvh hp (f3!@- (srnd:2on-circ *rs* 1.0) 1.4 hp))
                                      (rc-simple bvh hp (f3!@- (srnd:2on-circ *rs* 3.0) 1.0 hp))))))
     (f3!@*. v 0.1)))

(defmacro falloff (b e) `(exp (- (expt (abs ,b) (abs ,e)))))

; larger lim is faster
(veq:fvdef ll-sample-volume (bvh maxdepth lim vexpt vdst hi (:va 3 origin pt d)) ;pt = hit
   (declare #.*opt1* (fixnum hi maxdepth) (veq:ff origin pt d lim vexpt vdst))
               ; ve = 1.2 -> less fog; ve = 0.3 -> more
   (veq:xlet ((f3!far (if (< -1 hi) (veq:f3 pt) (veq:f3from origin d vdst))))
   (labels ((rec (lvl a b (:va 3 fa fb) &aux (num (1+ lvl)))
               (declare (veq:ff a b fa fb) (veq:pn lvl num))
               (veq:xlet ((f!sd (- b a)))
                 (if (or (> num maxdepth) (< sd lim) )
                     (f3!@*. (f3!@+ fa fb) (* 0.5 sd))
                     (veq:xlet ((f!mid (srnd:rndrng *rs* a b))
                                (f3!fm (sample mid)))
                      (f3!@+ (rec num a mid fa fm) (rec num mid b fm fb))))))

            (sample (q) (declare (veq:ff q))
              (veq:xlet ((f3!php (veq:f3lerp origin far q)))
                (veq:mvb (hi hs)
                         (rc bvh php (f3!@+ php (srnd:3on-sphere *rs* vdst)))
                         (declare (fixnum hi) (veq:ff hs))
                  (veq:mvb (flag (:va 3 rgb)) (hitmat bvh hi)
                           (declare (symbol flag) (veq:ff rgb))
                           ; (expt (- 1.0 (abs (veq:f3dot lray d))) 0.5)
                           (if (and (eq flag :ll) (< #.veq:*eps* hs))
                               (f3!@*. rgb (falloff hs vexpt))
                               ; (veq:f3 rgb)
                               (veq:f3rep 0.0)))))))
     (m@rec 0 0.01 0.999 (sample 0.01) (sample 0.999)))))

(veq:fvdef do-ao (bvh do-render depth raylen (:va 3 rgb pt dir hn))
  (declare #.*opt* (gmsh/bvh:bvh bvh) (function do-render)
                   (veq:pn depth) (veq:ff pt dir hn raylen))
  (veq:xlet ((f3!hp* (veq:f3from pt hn 0.001))
             (f3!ldir (f3!@+ (f3!@*. hn raylen)
                             (srnd:3on-sphere *rs* raylen)))
             (f!ao (abs (the veq:ff (rc-simple bvh hp* ldir)))))
    (f3!@+ (f3!@.* 0.7 (f3!@+ (f3!@*. rgb (* 0.8 ao))
                              (ao-sample-light bvh hp* dir hn ))) ; scale, s was here
           (f3!@*. (f3!@* rgb (f@do-render (1+ depth) hp*
                                (m@reflect dir
                                  (veq:f3norm (f3!@+ (srnd:3in-sphere *rs* 0.1) hn)))))
                   0.4))))

(veq:fvdef do-cr (bvh do-render depth (:va 3 rgb pt dir hn)) ; reflection
  (declare #.*opt1* (ignorable bvh) (function do-render) (gmsh/bvh:bvh bvh)
                    (veq:pn depth) (veq:ff pt dir hn))
  (veq:xlet ((f3!hp* (veq:f3from pt hn 0.001)))
    (f3!@+ (f3!@*. rgb 0.5) ; reflectance factor
           (f3!@*. (f@do-render (1+ depth) hp*
                     (m@reflect dir
                       (veq:f3norm (f3!@+ (srnd:3in-sphere *rs* 0.05) hn))))
                   0.5))))

; TODO: configurable background
; TODO: make config object?
(veq:vdef xrend (sc bvh &key (aa 1) (par t) (size 1000) (bs 1) (raylen 2000.0)
                             vol (vmult 64.0)  (vexpt 0.3)
                             (vdst 600f0)
                             (vlim 0.01) (vrec 7) (vdepth (if vol 1 0)))
  (declare #.*opt1* (gmsh/scene:scene sc) (gmsh/bvh:bvh bvh)
                    (veq:pn bs aa size vrec vdepth)
                    (veq:ff raylen vmult vlim vexpt vdst)
                    (boolean vol par))
  "render scene from this scene/bvh.

use (gmsh/xrend:init 4) before calling render to initialize parallel cores:

keywords:
 - aa     : rendering fidelity. higher is slower.
 - raylen : ray length for sampling. not volume sampling.
 - vdst   : ray length for sampling volume light. [:ll mat]
 - vmult  : volume light brightness. [:ll mat]
 - vexpt  : volume light distance falloff. lower is brighter.
 - vdepth : disable volume sampling after vdepth ray bounces.
 - vrec   : max recursive depth in volume sampling. higher is more detailed.
 - vlim   : volume sampling min recursive sampling step size.
            lower is more detailed. [0.0, 1.0]
 - vol    : enable disable volumetric light. [:ll mat]
 - par    : use parallelism.
" (render-wrap ; NOTE: requires scene: sc ; TODO: rewrite/make this explict
    (labels
      ((do-render (depth (:va 3 p d))
         (declare (veq:ff p d) (veq:pn depth))
         (when (> depth 13) (return-from do-render (veq:f3val 0.0))) ; TODO: configure
         (veq:xlet ((f3!ll (veq:f3scale d raylen)))
           (veq:mvb (hi hs) (rc bvh p ll)
             (declare (veq:ff hs) (veq:in hi))
             (veq:mvb (flag (:va 3 rgb)) (hitmat bvh hi)
               (declare (symbol flag) (veq:ff rgb))
               (veq:xlet
                 ((f3!pt (veq:f3from p ll hs))
                  (f3!res (case flag
                            (:ao (m@do-ao bvh #'do-render depth raylen rgb pt d (get-normal bvh hi d)))
                            (:cr (m@do-cr bvh #'do-render depth rgb pt d (get-normal bvh hi d)))
                            ((:ll :cc) (veq:f3 rgb))
                            ; (otherwise (veq:f3val (srnd:rndrng *rs* 0.95 1.0)))
                            (otherwise (veq:f3val (srnd:rndrng *rs* 0.0 0.05)))))) ; miss
                 (if (and vol (< depth vdepth))
                   (f3!@+ res (f3!@.* vmult
                                (m@ll-sample-volume bvh vrec vlim vexpt vdst hi p
                                  (f3!@+ p (f3!@*. ll hs)) ;
                                  (veq:f3norm (f3!@+ d (srnd:3in-sphere *rs* 0.05))))))
                   (veq:f3 res)))))))
       (do-row (yy &optional (xx 0) (repx size) (repy 1))
         (p/init-srnd)
         (loop for j of-type veq:pn from yy repeat repy
               do (veq:xlet ((f3!uv (veq:f3from (veq:f3$ (ortho::ortho-cam proj)) v
                                                (+ (:vr xy 1) (veq:ff j)))))
                    (loop for i of-type veq:pn from xx repeat repx
                          do (veq:xlet ((f3!p (veq:f3from uv u (+ (:vr xy 0) (veq:ff i))))
                                        (f3!res (veq:f3val 0.0)))
                               (loop repeat aa
                                     do (f3!@+! res (m@do-render 0
                                                      (pixel-shift *rs* u v p 0.75) vpn*)))
                               (canvas::m@set-pix canv i j (f3!@*. res ascale))))))))
      (format t "~&██ rendering pix: ~d; blocks: ~d; bs: ~d; aa: ~d~&"
            size blocks bs aa)
      (veq:xlet ((p!interval (max 1 (floor (/ blocks 10))))
                 (timer (auxin:iter-timer blocks
                          :int interval :infofx (get-info-fx size aa)
                          :prefx (lambda (&rest rest) (declare (ignore rest))"██ "))))
        (if par (let ((chan (lparallel:make-channel)))
                  (loop for k from 0 repeat blocks
                        do (lparallel:submit-task chan
                             (veq:xlet ((p!k* k)) (lambda () (do-row k*)))))
                  (loop for k from 0 repeat blocks
                        do (lparallel:receive-result chan) (f@timer)))
                (loop for k of-type veq:pn from 0 repeat blocks
                      do (do-row k) (f@timer)))
        (f@timer) (format t "~&██~&")))))

