(in-package :gmsh/xrend)

(declaim (inline reflect pixel-shift symbol-rgb get-normal hitmat-simple screen->world))

(defvar *rs*)
(defmacro rndrng        (&rest rest) `(srnd:rndrng *rs* ,@rest))
(defmacro rnd3in-sphere (&rest rest) `(srnd:3in-sphere *rs* ,@rest))
(defmacro rnd2in-circ   (&rest rest) `(srnd:2in-circ *rs* ,@rest))
(defmacro rnd3on-sphere (&rest rest) `(srnd:3on-sphere *rs* ,@rest))
(defun xrend-worker-context (worker-loop) (let ((*rs*)) (funcall worker-loop)))

(defmacro p/init-srnd ()
  `(unless *rs*
     (setf *rs* (srnd:make (+ (get-internal-real-time)
                              (the fixnum (* (the (unsigned-byte 6)
                                                  (lparallel:kernel-worker-index))
                                             19997)))))))

; NOTE: make-kernel can accept thread local bindings. eg fast stack
(defmacro init (k &key (context #'gmsh/xrend::xrend-worker-context))
  `(progn (format t "~&██ starting ~a threads~&" ,k)
          (setf lparallel:*kernel* (lparallel:make-kernel ,k :context ,context))))

; (defmacro falloff (b e) `(exp (- (expt (abs ,b) ,e))))
(veq:fvdef reflect ((:va 3 d n)) (declare #.*opt1* (veq:ff d n))
  (f3!@- d (f3!@*. n (* 2.0 (veq:f3dot d n)))))

(veq:vdef pixel-shift (rs (veq:varg 3 u v p) s)
  (declare #.*opt1* (srnd:srnd rs) (veq:ff u v p s))
  (veq:f3from (veq:f3from p u (srnd:rnd* rs s)) v (srnd:rnd* rs s)))

(veq:fvdef symbol-rgb (s) ; TODO: adapt to *color* and *rs*
  (declare #.*opt1* (keyword s))
  (case s (:w (veq:f3rep 1.0))
          (:r (veq:f3 1.0 0.0 0.0)) (:g (veq:f3 0.0 1.0 0.0)) (:b (veq:f3 0.0 0.0 1.0))
          (:gray (veq:f3val 0.4))
          (:c (veq:f3 0.0 1.0 1.0)) (:m (veq:f3 1.0 0.0 1.0)) (:y (veq:f3 1.0 1.0 0.0))
          (:k (veq:f3rep 0.0))
          (otherwise (veq:f3rep 1.0))))

(veq:fvdef align-normal (bvh i (:va 3 d))
  (declare #.*opt1* (gmsh/bvh:bvh bvh) (veq:in i) (veq:ff d))
  (veq:xlet ((f3!n (gmsh/bvh:@norm bvh i)))
    (if (< (veq:f3dot n d) 0.0) (veq:f3 n) (f3.@- n))))

(veq:fvdef hitmat-simple (bvh i default) ; embedded in xrend for the time being
  (declare #.*opt1* (gmsh/bvh:bvh bvh) (veq:in i) (keyword default))
  (if (> i -1) (veq:mvb (flag rgbflag) (gmsh/bvh:@mat bvh i)
                 (declare (symbol flag rgbflag))
                 (veq:~ flag (symbol-rgb rgbflag)))
               (veq:~ default (veq:f3rep 0.0))))

(defun get-info-fx (size aa interval) (declare (veq:pn size aa interval))
  (lambda (i secs df_secs) (declare (veq:pn i) (veq:ff secs))
    (lqn:fmt "λ ~06,2f / ~05,2f" (auxin:me (/ (* interval aa size) df_secs))
                                   (auxin:me (/ (* i aa size) secs)))))

(veq:fvdef screen->world (txmat (:va 3 xyz))
  (declare #.*opt1* (veq:fvec txmat) (veq:ff xyz))
  "expects coordinates in ndc box"
  (f3!@/. (veq:f4mv txmat xyz 1.0)))

(defmacro render-wrap (labels*) ; TODO: make this cleaner ;
 (let ((default-labels
   `((do-row (yy xx repx repy)
       (declare (veq:pn yy xx repx repy))
       (p/init-srnd) ; create srnd state *rs* if thread does not have it
       (loop for j of-type veq:pn from yy repeat repy
         do (loop for i of-type veq:pn from xx repeat repx
              do (veq:xlet ((f3!res (veq:f3val 0.0)))
                   (loop repeat aa
                     do (veq:xlet
                          ((f2!xy (f2!@+ (rnd2in-circ isize)
                                    (f2!@.* 2.0
                                      (f2!@-. (f2!@*. (veq:ff i) (veq:ff (- size j))
                                                      isize)
                                              0.5))))
                           (f3!p (screen->world txmat xy 0f0)))
                          (f3!@+! res (m@do-render 0 p
                                        (veq:f3norm (f3!@- (screen->world txmat xy 1.0) p))))))
                   (canvas::m@set-pix canv i j
                     (f3!@*. res aa-mult))))))
     (hitmat (i depth)
       (declare (veq:in i) (veq:pn depth))
       (if (and (< i 0) (< depth 1)) (values miss 0.0 0.0 0.0) ; CHANGE?
                                     (hitmat-simple bvh i world)))
      (do-render (depth (:va 3 p dir)) (declare (veq:pn depth) (veq:ff p dir))
        (when (> depth max-depth) (return-from do-render (veq:f3 0.0 0.0 0.0))) ; CHANGE?
        (veq:xlet ((f3!ll (veq:f3scale dir raylen)))
          (veq:mvb (hi hs) (rc bvh p ll)
            (declare (veq:ff hs) (veq:in hi))
            ; hitmat returns eg: (~ (if (> hi 0) :ao :miss) 1f0 1f0 1f0)
            (veq:mvb (flag (:va 3 rgb)) (hitmat hi depth)
              (declare (keyword flag) (veq:ff rgb))
              (veq:xlet ((f3!pt (veq:f3from p ll hs))
                         (f3!res (ecase flag ; TODO: process input shaders
                                   (:ao  (f3!@*. rgb (m@do-ao hi pt dir))) ; hits
                                   (:rr  (m@do-rr depth hi rgb pt dir))
                                   (:ro  (m@do-ro depth hi rgb pt dir))
                                   ((:ll :cc) (veq:f3 rgb))

                                   (:bgw (veq:f3val (rndrng 0.95 1.0)))
                                   (:bgk (veq:f3val (rndrng 0.0  0.05)))
                                   (:bgkk (veq:f3val 0f0))
                                   (:bgww (veq:f3val 1f0))
                                   (:bgvv (veq:f3val 0.5))
                                   (:bgrr (veq:f3 1f0 0f0 0f0))
                                   (:bggg (veq:f3 0f0 1f0 0f0))
                                   (:bgbb (veq:f3 0f0 0f0 1f0))
                                   (:bgmm (veq:f3 1f0 0f0 1f0))
                                   (:bgcc (veq:f3 0f0 1f0 1f0))
                                   (:bgyy (veq:f3 1f0 1f0 0f0)))))

               (when (and vol (< depth vdepth)) ; [:ll-mat] volume sampling
                 (f3!@+! res (m@do-ll  hi p (f3!@+ p (f3!@*. ll hs)) dir)))
               (veq:f3 res)))))))))

  `(veq:fvprogn ; TODO: or convert to generic render macro
   (locally (declare #.*opt*)
   (veq:xlet ((canv (gmsh/scene:@canv sc))
              (cam (gmsh/scene:@cam sc))
              (f3!vpn* (f3.@- (gmsh/cam:@vpn cam)))
              (f3!pos (gmsh/cam:@pos cam))
              (f!isize (veq:ff (/ size)))
              (invvm (veq:f4minv (gmsh/cam:vm cam)))
              (invpm (veq:f4minv (gmsh/cam:pm cam)))
              (txmat (veq:f4mm invvm invpm))
              (p!blocks (floor size bs)) ; TODO: blocks do not work properly
              (timer (auxin:iter-timer blocks
                       :int info :infofx (get-info-fx size aa info)
                       :prefx (lambda (&rest rest) (declare (ignore rest))
                                (lqn:fmt "██ ~a: " (lqn:seq (lqn:now) 11 19))))))
    (declare (gmsh/cam:cam cam) (canvas::canvas canv)
             (veq:fvec invvm invpm txmat))
    (labels (,@labels* ,@(set-difference default-labels labels* :key #'car))

      (lqn:out "~&██ rendering scene at ~d pix (~d * ~d)~&" size blocks bs)
      (lqn:out "~&██   aa: ~a;  ao-rep: ~a; vlim: ~a~&" aa ao-rep vlim)
      (lqn:out "~&██   raylen: ~a;  max-depth: ~d~&" raylen max-depth)
      (if par (let ((chan (lparallel:make-channel)))
                (loop for k from 0 repeat blocks
                      do (veq:xlet ((p!k* k))
                           (labels ((row () (do-row k* 0 size 1)))
                             (lparallel:submit-task chan #'row))))
                (loop for k from 0 repeat blocks
                      do (lparallel:receive-result chan) (f@timer)))
              (loop for k of-type veq:pn from 0 repeat blocks
                    do (do-row k 0 size 1) (f@timer)))
      (f@timer) (format t "~&██~&")))))))

