(in-package :gmsh/xrend)

(declaim (inline reflect pixel-shift symbol-rgb get-normal hitmat)
         (veq:ff *dstlim*)
         (srnd::srnd *rs*)) ; TODO: this should be configurable

(defvar *dstlim* 2000.0) (defvar *vdst* 600.0)
(defvar *rs* (srnd:srnd 1))

(defmacro init (k &key (context #'gmsh/xrend::xrend-worker-context)
                       (bindings '((gmsh/bvh::*stck* . (gmsh::make-fast-stack :n 2048)))))
  `(progn (format t "~&██ starting ~a threads~&" ,k)
          (setf lparallel:*kernel* (lparallel:make-kernel ,k
                                     :context ,context :bindings ',bindings))))

; (defmacro rc-simple (&rest rest) "simple raycast using current raycaster."
;   `(gmsh/bvh:simd4/simple-raycast ,@rest))
; (defmacro rc (&rest rest) "raycast using current raycaster."
;   `(gmsh/bvh:simd4/raycast ,@rest))

(defmacro rc-simple (&rest rest) `(gmsh/bvh:int/simple-raycast ,@rest))
(defmacro rc (&rest rest) `(gmsh/bvh:int/raycast ,@rest))


(defmacro render-wrap (&body body)
  ; TODO: make this cleaner ;
  ; TODO: or convert to generic render macro
  `(veq:fvprogn
    (veq:xlet ((canv (gmsh/scene::scene-canv sc)) (proj (gmsh/scene::scene-proj sc))
               (f!ascale (/ (veq:ff aa))) (p!blocks (floor size bs))
               (f2!xy (veq:f2scale (veq:f2$ (ortho::ortho-xy proj)) -1.0))
               (f3!vpn* (f3.@- (veq:f3$ (ortho::ortho-vpn proj))))
               ; (f3!vpn* (veq:f3$ (ortho::ortho-vpn proj)))
               (f3!u (f3.@- (su proj))) (f3!v (f3.@- (sv proj))))
      (declare (ortho::ortho proj) (canvas::canvas canv))
      (progn ,@body))))

(defmacro init-srnd ()
  `(unless *rs*
     (setf *rs* (srnd:make
                  (+ (get-internal-real-time)
                     (the fixnum (* 19997 (the (unsigned-byte 6)
                                            (lparallel:kernel-worker-index)))))))))

(defun xrend-worker-context (worker-loop) ; TODO: why bind rs here?
  (let ((*rs* (srnd::srnd 1))) (funcall worker-loop)))

(veq:fvdef reflect ((:va 3 d n)) (declare #.*opt1* (veq:ff d n))
  (f3!@- d (f3!@*. n (* 2.0 (veq:f3dot d n)))))

(veq:vdef pixel-shift (rs (veq:varg 3 u v p) s)
  (declare #.*opt1* (srnd:srnd rs) (veq:ff u v p s))
  (veq:f3from (veq:f3from p u (srnd:rnd* rs s)) v (srnd:rnd* rs s)))

(veq:fvdef su (proj) (f3!@/. (veq:f3$s proj ortho::ortho- :u) (ortho::ortho-s proj)))
(veq:fvdef sv (proj) (f3!@/. (veq:f3$s proj ortho::ortho- :v) (ortho::ortho-s proj)))

; (veq:fvdef* clamp-rgb ((:va 3 rgb)) (declare #.*opt1* (veq:ff rgb))
;   (veq:f3 (veq:fclamp (:vr rgb 0)) (veq:fclamp (:vr rgb 1)) (veq:fclamp (:vr rgb 2))))
; (veq:fvdef* scale-rgb ((:va 3 rgb)) (declare (veq:ff rgb))
;   (veq:xlet ((f3!rgb* (clamp-rgb rgb))
;              (f!i (+ (* 0.299 (:vr rgb* 0)) (* 0.587 (:vr rgb* 1)) (* 0.114 (:vr rgb* 2)))))
;    (if (> i 0.95) (clamp-rgb (f3!@+ rgb* (veq:f3val (rnd:rnd* 0.05))))
;                   (veq:f3 rgb*))))

(veq:fvdef symbol-rgb (s)  ; TODO: adapt to *color*
  (case s (:w (veq:f3rep 1.0))
          (:r (veq:f3 1.0 0.0 0.0)) (:g (veq:f3 0.0 1.0 0.0)) (:b (veq:f3 0.0 0.0 1.0))
          (:gray (veq:f3val 0.4))
          (:c (veq:f3 0.0 1.0 1.0)) (:m (veq:f3 1.0 0.0 1.0)) (:y (veq:f3 1.0 1.0 0.0))
          (:k (veq:f3rep 0.0))
          (otherwise (veq:f3rep 1.0))))

(veq:fvdef get-normal (bvh i (:va 3 d))
  (declare #.*opt1* (gmsh/bvh:bvh bvh) (veq:in i) (veq:ff d))
  (veq:xlet ((f3!n (gmsh/bvh::get-norm bvh i)))
    (if (< (veq:f3dot n d) 0.0) (veq:f3 n) (f3.@- n))))

(veq:fvdef hitmat (bvh i) (declare #.*opt1* (gmsh/bvh:bvh bvh) (veq:in i))
  (when (< i 0) (return-from hitmat (veq:~ :bg (veq:f3rep 0.0))))
  (veq:mvb (flag rgbflag) (gmsh/bvh::get-mat bvh i)
    (declare (symbol flag rgbflag))
    (veq:~ flag (symbol-rgb rgbflag))))

(defun get-info-fx (size aa) (declare (veq:pn size aa))
  (lambda (i progr) (declare (veq:pn i) (veq:ff progr))
    (format nil "λ~06,2f" (auxin:me (* aa size (/ i progr))))))

