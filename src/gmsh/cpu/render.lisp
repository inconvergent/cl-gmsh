(in-package :gmsh/xrend)

(declaim (veq:ff *dstlim*)) ; TODO: this should be configurable
(defvar *dstlim* 2000f0) (defvar *vdst* 600f0)
(defvar *rs* (srnd:srnd 1))

(declaim (inline sample-light do-ao do-cr %sample-volume
                 %do-subsec-sample %subseq-sample))

(defun xrend-worker-context (worker-loop) ; TODO: why bind rs here?
  (let ((*rs*)) (funcall worker-loop)))

(veq:fvdef sample-light (bvh (:va 3 hp d hn))
  (declare #.*opt* (gmsh/bvh:bvh bvh) (veq:ff hp d hn))
  (veq:xlet ((f3!v 0)
             (f!dt (* #.(/ 3f0) (abs (veq:f3dot (veq:f3norm d) hn)))))
     (loop repeat 2
          do  (f3!@+.! v (* 0.5 dt (+ (rc-simple bvh hp (f3!@- (srnd:2on-circ *rs* 1f0) 1.4f0 hp))
                                      (rc-simple bvh hp (f3!@- (srnd:2on-circ *rs* 3f0) 1f0 hp))))))
     (f3!@*. v 0.1)))


(veq:fvdef* subseq-sample (a* b* fx &optional (lim 0.1))
  (declare #.*opt1* (function fx) (veq:ff a* b* lim))
  (labels
    ((rec (a b (:va 3 fa fb))
       (declare (veq:ff a b fa fb))
       (veq:xlet ((f!d (- b a)))
         (when (< d lim) (return-from rec (f3!@*. (f3!@+ fa fb) (* 0.5 d)))))
       (veq:xlet ((f!mid (srnd:rndrng *rs* a b)) (f3!fm (f@fx mid)))
         (f3!@+ (rec a mid fa fm) (rec mid b fm fb)))))
    (m@rec a* b* (f@fx a*) (f@fx b*))))

(defmacro falloff (b e) `(exp (- (expt (abs ,b) ,e))))

(veq:fvdef* do-subsec-sample (bvh dst (:va 3 p delta) &optional (va 0.0009f0) (ve 1.74f0))
  (declare #.*opt1* (gmsh/bvh::bvh bvh) (veq:ff dst p delta) (ignorable va ve))

   (f3!@.* 34f0 (subseq-sample 0.001 0.991
    (lambda (q)
      (declare (veq:ff q))
      (handler-case
        (veq:xlet ((f3!php (veq:f3from p delta q))
                   (f3!lray (srnd:3on-sphere *rs* 1f0))
                   )
          (veq:mvb (hi hs) (rc bvh php (f3!@+ php (f3!@*. lray *vdst*)))
            (declare (fixnum hi) (veq:ff hs))
            (veq:mvb (flag (:va 3 rgb)) (hitmat bvh hi)
              (declare (symbol flag) (veq:ff rgb))
              ; (print flag)
              ; (veq:vpr lray d)
              (if (and (eq flag :ll) (< veq:*eps* hs))
                  (f3!@*. rgb
                         ; (* 1000 (print hs))
                          ; (falloff (* (* *vdst* hs) va) ve)
                          1f0
                          ; (expt (- 1f0 (abs (veq:f3dot lray d)))
                          ;       0.5
                          ;       )
                          )
                  (veq:f3rep 0f0)))))
        ; (division-by-zero (e) (declare (ignorable e)) (veq:f3rep 0f0))
        ))
    (/ 6.0 dst))))

(veq:fvdef*  sample-volume (bvh hi (:va 3 origin pt d)) ;pt = hit
   (declare #.*opt1* (fixnum hi) (veq:ff origin pt d))
   (veq:xlet ((f3!far (if (< -1 hi) (veq:f3 pt) (veq:f3from origin d *vdst*))))
     (do-subsec-sample bvh (veq:f3dst origin far) origin (f3!@- far origin))))

(veq:fvdef do-ao (bvh do-render depth (:va 3 rgb pt dir hn))
  (declare #.*opt* (gmsh/bvh:bvh bvh)
                    (veq:ff pt dir hn) (veq:pn depth))
  (veq:xlet ((f3!hp* (veq:f3from pt hn 0.001))
             (f3!ldir (f3!@+ (f3!@*. hn *dstlim*)
                             (srnd:3on-sphere *rs* *dstlim*)))
             (f!ao (abs (the veq:ff (rc-simple bvh hp* ldir)))))
    ; (f3!@+ (f3!@*. rgb (* 0.3 ao)) (sample-light bvh rs hp* dir hn 0.4f0))
    (f3!@+ (f3!@.* 0.7 (f3!@+ (f3!@*. rgb (* 0.80 ao))
                              (sample-light bvh hp* dir hn ))) ; scale, s was here
           (f3!@*. (f3!@* rgb (f@do-render (1+ depth) hp*
                                (m@reflect dir
                                  (veq:f3norm (f3!@+ (srnd:3in-sphere *rs* 0.1) hn)))))
                   0.4))))

(veq:fvdef do-cr (bvh do-render depth (:va 3 rgb pt dir hn)) ; reflection
  (declare #.*opt1* (gmsh/bvh:bvh bvh) (veq:ff pt dir hn)
                   (veq:pn depth) (ignorable bvh))
  (veq:xlet ((f3!hp* (veq:f3from pt hn 0.001)))
    (f3!@+ (f3!@*. rgb 0.9)
           (f3!@*. (f@do-render (1+ depth) hp*
                       (m@reflect dir (veq:f3norm
                                        (f3!@+ (srnd:3in-sphere *rs* 0.05) hn))))
                   0.1))))


; TODO: pass in ambient light sampler
(veq:vdef render (sc bvh &key (aa 1) (par t) (size 1000) (bs 1) (raylen 2000f0))
  (declare #.*opt1* (gmsh/scene:scene sc) (gmsh/bvh:bvh bvh)
                    (veq:pn bs aa size bs) (boolean par) (veq:ff raylen))
  "render scene from this scene/bvh."
  (render-wrap ; NOTE: requires scene: sc
    (labels
      ((do-render (depth (:va 3 p d))
         (declare (veq:ff p d) (veq:pn depth))
         (when (> depth 13) (return-from do-render (veq:f3val 0f0)))
         (veq:xlet ((f3!ll (veq:f3scale d raylen)))
           (veq:mvb (hi hs) (rc bvh p ll)
             (declare (veq:ff hs) (veq:in hi))
             ; (print hs)
             (veq:mvb (flag (:va 3 rgb)) (hitmat bvh hi)
               (declare (symbol flag) (veq:ff rgb))
               (veq:xlet ((f3!pt (veq:f3from p ll hs))
                          (f3!res
                            (case flag
                                    (:ao (m@do-ao bvh #'do-render depth rgb pt d (get-normal bvh hi d)))
                                    (:cr (m@do-cr bvh #'do-render depth rgb pt d (get-normal bvh hi d)))
                                    ((:ll :c) (veq:f3 rgb))
                                    ; (otherwise (veq:f3val (srnd:rndrng *rs* 0.95 1.0)))
                                    (otherwise (veq:f3val (srnd:rndrng *rs* 0.0 0.05))) ; miss
                                    )))
                         (progn ;if t ; (< depth 1)
                             (f3!@+ res (sample-volume bvh hi p
                                                        (f3!@+ p (f3!@*. ll hs)) ;
                                           (veq:f3norm (f3!@+ (srnd:3in-sphere *rs* 0.1) d))
                                          ; d
                                          ))
                             ; (veq:f3 res)
                             ))))))
       (do-row (yy &optional (xx 0) (repx size) (repy 1))
         (init-srnd)
         (loop for j of-type veq:pn from yy repeat repy
               do (veq:xlet ((f3!uv (veq:f3from (veq:f3$ (ortho::ortho-cam proj)) v
                                                (+ (:vr xy 1) (veq:ff j)))))
                    (loop for i of-type veq:pn from xx repeat repx
                          do (veq:xlet ((f3!p (veq:f3from uv u (+ (:vr xy 0) (veq:ff i))))
                                        (f3!res (veq:f3val 0f0)))
                               (loop repeat aa
                                     do (f3!@+! res (m@do-render 0
                                                      (pixel-shift *rs* u v p 0.5) vpn*)))
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

