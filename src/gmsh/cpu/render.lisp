(in-package :gmsh/xrend)


(defmacro falloff (b e) `(exp (- (expt (abs ,b) (abs ,e)))))

; larger lim is faster
; (veq:fvdef do-ll (bvh maxdepth lim vexpt vdst hi (:va 3 origin pt d)) ;pt = hit
;    (declare #.*opt1* (fixnum hi maxdepth) (veq:ff origin pt d lim vexpt vdst))
;                ; ve = 1.2 -> less fog; ve = 0.3 -> more
;    (veq:xlet ((f3!far (if (< -1 hi) (veq:f3 pt) (veq:f3from origin d vdst))))
;    (labels ((rec (lvl a b (:va 3 fa fb) &aux (num (1+ lvl)))
;                (declare (veq:ff a b fa fb) (veq:pn lvl num))
;                (veq:xlet ((f!sd (- b a)))
;                  (if (or (> num maxdepth) (< sd lim) )
;                      (f3!@*. (f3!@+ fa fb) (* 0.5 sd))
;                      (veq:xlet ((f!mid (srnd:rndrng *rs* a b))
;                                 (f3!fm (sample mid)))
;                       (f3!@+ (rec num a mid fa fm) (rec num mid b fm fb))))))

;             (sample (q) (declare (veq:ff q))
;               (veq:xlet ((f3!php (veq:f3lerp origin far q)))
;                 (veq:mvb (hi hs)
;                          (rc bvh php (f3!@+ php (srnd:3on-sphere *rs* vdst)))
;                          (declare (fixnum hi) (veq:ff hs))
;                   (veq:mvb (flag (:va 3 rgb)) (hitmat bvh hi :bgk) ; TODO: hitmat
;                            (declare (symbol flag) (veq:ff rgb))
;                            ; (expt (- 1.0 (abs (veq:f3dot lray d))) 0.5)
;                            (if (and (eq flag :ll) (< #.veq:*eps* hs))
;                                (f3!@*. rgb (falloff hs vexpt))
;                                ; (veq:f3 rgb)
;                                (veq:f3rep 0.0)))))))
;      (m@rec 0 0.01 0.999 (sample 0.01) (sample 0.999)))))

; TODO: configurable background
; TODO: make config object?
(veq:vdef xrend (sc bvh &key (par t) (size 1000) (bs 1) (aa 1)
                             (raylen 2000.0)
                             (max-depth 13)
                             (ao-rep 4)
                             (vol nil)
                             (vmult 64.0)
                             (vexpt 0.3)
                             (vdst 600f0)
                             (vlim 0.01)
                             (vrec 7)
                             (vdepth (if vol 1 0))
                             (miss :bgk)
                             (world miss)
                             (raylen2 (* 0.5 raylen))
                             (ao-mult (veq:ff (/ ao-rep))))
  (declare #.*opt1* (gmsh/scene:scene sc) (gmsh/bvh:bvh bvh)
                    (veq:pn bs aa size max-depth ao-rep vrec vdepth )
                    (keyword world miss)
                    (veq:ff raylen raylen2
                            ao-mult
                            vmult vlim vexpt vdst)
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
" (render-wrap (labels ; NOTE: requires scene: sc ; TODO: rewrite/make this explict
    ((do-ao (hi (:va 3 pt dir)) ; ambient occlusion
       (declare (veq:in hi) (veq:ff pt dir))
       (veq:xlet ((f3!hn (get-normal bvh hi dir))
                  (f3!hp* (veq:f3from pt hn 0.001))
                  (f3!hnrl (f3!@*. hn raylen2)))
         (* (loop repeat ao-rep summing
                (abs (the veq:ff
                       (rc-simple bvh hp*
                         (f3!@+ (3on-sphere raylen2) hnrl)))))
              ao-mult)))

     (do-rr (depth hi (:va 3 rgb pt dir)) ; reflection
       (declare (veq:pn depth) (veq:in hi) (veq:ff rgb pt dir))
       (veq:xlet ((f3!hn (get-normal bvh hi dir))
                  (f3!hp* (veq:f3from pt hn 0.001)))
         (f3!@+ (f3!@*. rgb 0.5) ; TODO: config factor
         (f3!@*. (m@do-render (1+ depth) hp*
                   (m@reflect dir
                     (veq:f3norm (f3!@+ (3in-sphere 0.05) hn))))
                 0.5))))

     ; TODO: conf/simplify with rr
     (do-ro (depth hi (:va 3 rgb pt dir)) ; ao * rr
       (declare (veq:pn depth) (veq:in hi) (veq:ff rgb pt dir))
       (veq:xlet ((f3!hn (get-normal bvh hi dir))
                  (f3!hp* (veq:f3from pt hn 0.001)))
         (f3!@*. (f3!@+ (f3!@*. rgb 0.2)
                        (f3!@*. (m@do-render (1+ depth) hp*
                                  (m@reflect dir
                                    (veq:f3norm (f3!@+ (3in-sphere 0.03) hn))))
                                0.8))
                 (do-ao hi pt dir))))

      (hitmat (i depth) (declare (veq:in i) (veq:pn depth))
        (when (and (< i 0) (< depth 1)) (return-from hitmat (values miss 0f0 0f0 0f0)))
        (when (< i 0)                   (return-from hitmat (values world 0f0 0f0 0f0)))
        (veq:mvb (flag rgbflag) (gmsh/bvh::get-mat bvh i)
          (declare (keyword flag rgbflag))
          (veq:~ flag (symbol-rgb rgbflag))))

     (do-render (depth (:va 3 p dir)) (declare (veq:pn depth) (veq:ff p dir))
       (when (> depth max-depth) (return-from do-render (veq:f3val 0.0)))
       (veq:xlet ((f3!ll (veq:f3scale dir raylen)))
         (veq:mvb (hi hs) (rc bvh p ll)
           (declare (veq:ff hs) (veq:in hi))
           ; hitmat returns eg: (~ (if (> hi 0) :ao :miss) 1f0 1f0 1f0)
           (veq:mvb (flag (:va 3 rgb)) (hitmat hi depth)
             (declare (keyword flag) (veq:ff rgb))
             (when (null flag) (error "oh no ~a ~a" flag hi))
             (veq:xlet ((f3!pt (veq:f3from p ll hs))
                        (f3!res (ecase flag
                                   (:ao  (f3!@*. rgb (m@do-ao hi pt dir))) ; hits
                                   (:rr  (m@do-rr depth hi rgb pt dir))
                                   (:ro  (m@do-ro depth hi rgb pt dir))
                                   ; ((:ll :cc :ao :cr) (veq:f3 rgb))
                                   ((:ll :cc) (veq:f3 rgb))
                                   (:bgw (veq:f3val (rndrng 0.95  1.0)))
                                   (:bgk (veq:f3val (rndrng 0.0   0.05)))
                                   (:bgr (veq:f3 1f0 0f0 0f0)) ; TODO ..
                                   (:bgg (veq:f3 0f0 1f0 0f0))
                                   ; (nil (veq:f3 0.7 0.7 0.0))
                                   )))

              (veq:f3 res)
              ; (if (and vol (< depth vdepth)) ; [:ll-mat] volume sampling
              ;     (f3!@+ res
              ;       (f3!@*. (m@do-ll bvh vrec vlim vexpt vdst hi p
              ;                 (f3!@+ p (f3!@*. ll hs))
              ;                 (veq:f3norm (f3!@+ (3in-sphere 0.05) dir)))
              ;               vmult))
              ;     (veq:f3 res))
              )))))

     (do-row (yy &optional (xx 0) (repx size) (repy 1))
       (p/init-srnd) ; create srnd state *rs* if thread does not have it
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

    (format t "~&██ rendering pix: ~d; blocks: ~d; bs: ~d; aa: ~d~&" size blocks bs aa)
    (veq:xlet ((p!interval (max 1 200))
               (timer (auxin:iter-timer blocks
                        :int interval :infofx (get-info-fx size aa)
                        :prefx (lambda (&rest rest) (declare (ignore rest))"██ "))))
      (if par (let ((chan (lparallel:make-channel)))
                (loop for k from 0 repeat blocks
                      do (veq:xlet ((p!k* k))
                           (labels ((row () (do-row k*)))
                             (lparallel:submit-task chan #'row))))
                (loop for k from 0 repeat blocks
                      do (lparallel:receive-result chan) (f@timer)))
              (loop for k of-type veq:pn from 0 repeat blocks
                    do (do-row k) (f@timer)))
      (f@timer) (format t "~&██~&")))))

