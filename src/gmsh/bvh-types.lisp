(in-package :gmsh/bvh)

(deftype bvh-nodes   () `(simple-array bvh-node))
(deftype sah-buckets () `(simple-array sah-bucket))
(deftype bbox        () `(simple-array veq:ff (6)))

(defmacro bbox (&rest rest) `(veq:f$~ (6) ,@rest))
(defmacro bb$ (b f) `(,(gmsh::symb :sah-bucket- f) ,b)) ; TODO: rename these
(defmacro bb$! (b f v) `(setf (bb$ ,b ,f) ,v))
(defmacro bb$n (b) `(bb$ ,b :num-inds))

(defmacro make-bb () `(locally (declare #.*opt1*)
                        (veq:f3$line *bvhhi* *bvhlo* *bvhhi* *bvhlo* *bvhhi* *bvhlo*)))
(defmacro reset-bb! (b*)
  `(locally (declare #.*opt*)
    (let ((b ,b*)) (declare (bbox b))
     (setf (aref b 0) *bvhhi* (aref b 1) *bvhlo* (aref b 2) *bvhhi*
           (aref b 3) *bvhlo* (aref b 4) *bvhhi* (aref b 5) *bvhlo*))))

(declaim (inline set-bb! merge-bb! bb-area))
(veq:fvdef merge-bb! (curr new) (declare #.*opt1* (bbox curr new))
  (macrolet ((mi (i) `(min (veq:$ curr ,i) (veq:$ new ,i)))
             (ma (i) `(max (veq:$ curr ,i) (veq:$ new ,i))))
    (setf (veq:$ curr 0) (mi 0) (veq:$ curr 1) (ma 1)
          (veq:$ curr 2) (mi 2) (veq:$ curr 3) (ma 3)
          (veq:$ curr 4) (mi 4) (veq:$ curr 5) (ma 5))))
(veq:fvdef set-bb! (curr new) (declare #.*opt1* (bbox curr new))
  (setf (veq:$ curr 0) (veq:$ new 0) (veq:$ curr 1) (veq:$ new 1)
        (veq:$ curr 2) (veq:$ new 2) (veq:$ curr 3) (veq:$ new 3)
        (veq:$ curr 4) (veq:$ new 4) (veq:$ curr 5) (veq:$ new 5)))
(veq:fvdef bb-area (b) (declare #.*opt1* (bbox b))
  (veq:xlet ((f!x (abs (f2_@- (veq:$ b 1 0))))
             (f!y (abs (f2_@- (veq:$ b 3 2))))
             (f!z (abs (f2_@- (veq:$ b 5 4)))))
    (* 0.1 (sqrt (+ (* x z) (* x y) (* y z))))))

(defstruct (bvh-node (:constructor -make-bvh-node) (:print-object -print-bvh-node))
  (num 0 :type veq:pn :read-only nil)
  (ref 0 :type veq:pn :read-only nil)
  (ax 0 :type (unsigned-byte 4) :read-only nil)
  (bbox (bbox (veq:vnval 6 0f0)) :type bbox :read-only t)
  (lvl 0 :type (unsigned-byte 4) :read-only nil))
(defun -print-bvh-node (o s)
  (declare (notinline bvh-node-num bvh-node-ref bvh-node-ax bvh-node-lvl bvh-node-bbox))
  (auxin:with-struct (bvh-node- num ref ax lvl bbox) o
    (format s "<@bvh/n ~d ↳~2<~d~> | ~a ~4@<~d~>~a  □~{ ~05,2f~}>"
            (aref #("x" "y" "z" "na") ax)
            lvl (if (> num 0) "∇" "↙")
            ref (if (> num 0) (format nil "❦~1<~d~>" num) "  ")
            (coerce bbox 'list))))

(defun init-bvh-nodes (n)
  (make-array n :element-type 'bvh-node :adjustable nil
                :initial-contents (loop repeat n collect (-make-bvh-node))))

(declaim (inline bvh-polyfx bvh-nodes bvh-polys))
(defstruct (bvh (:constructor -make-bvh) (:print-object -print-bvh))
  "static BVH object for rendering"
  (simd-nodes (veq:p4$zero 0)  :type veq:pvec :read-only nil)
  (simd-mima (veq:f4$zero 0)   :type veq:fvec :read-only t)
  (polyfx (veq:f3$zero 0)      :type veq:fvec :read-only t)
  (normals (veq:f3$zero 0)     :type veq:fvec :read-only t)
  (polys (veq:p3$zero 0)       :type veq:pvec :read-only t)
  (mat nil                     :type veq:svec :read-only t)
  (mima (veq:f4$zero 0)        :type veq:fvec :read-only t)
  (nodes (init-bvh-nodes 1) :type bvh-nodes :read-only nil)
  (int-nodes (veq:p4$zero 0)   :type veq:pvec :read-only nil)
  (mode :bvh4-simd             :type keyword :read-only t)
  (num-leaves 0                :type veq:pn :read-only t)
  (num-nodes 0                 :type veq:pn :read-only t)
  (num-polys 0                 :type veq:pn :read-only t)
  (cnt 0                       :type veq:pn :read-only nil)
  (lvl 0                       :type veq:pn :read-only t)
  (time 0.0                    :type veq:ff :read-only t))
(defun -print-bvh (o s)
  (declare (notinline bvh-time bvh-num-nodes bvh-num-polys bvh-lvl bvh-mode bvh-num-leaves))
  (auxin:with-struct (bvh- num-nodes num-polys lvl time mode num-leaves) o
    (format s "<@~a ❦~1<~d~> # ~a ↳~2<~d~> ∇ ~a s ~a>"
              (lqn:sdwn mode) num-leaves num-nodes lvl num-polys  time)))
; (auxin:define-struct-load-form bvh)

; (defun -print-sah-bucket (o s)
;   (auxin:with-struct (sah-bucket- mi ma fwdcnt fwdbb bckcnt bckbb inds) o
;     (format s "<@sah/b # ~d | ~05,2f ~05,2f | fwd: ~a ~05,2f | bck: ~a ~05,2f ~%~a ~%~a>"
;             (length inds) mi ma fwdcnt fwdbb bckcnt bckbb)))
; (declaim (inline sah-bucket-fwdcnt sah-bucket-bckcnt
;                  sah-bucket-mi sah-bucket-ma
;                  sah-bucket-bckbb sah-bucket-fwdbb
;                  sah-bucket-bb sah-bucket-inds))
(defstruct (sah-bucket (:constructor -make-sah-bucket)
                       ; (:print-object -print-sah-bucket)
                       )
  ; (inds (auxin:make-adjustable-vector :type 'veq:pn) :type vector)
  (inds (list) :type list) (num-inds 0 :type veq:pn)
  (mi *bvhhi* :type veq:ff)    (ma *bvhlo* :type veq:ff)
  (fwdcnt 0 :type veq:pn)      (bckcnt 0 :type veq:pn)
  (fwdbb (make-bb) :type bbox) (bckbb (make-bb) :type bbox)
  (bb (make-bb) :type bbox))

(defun init-sah-buckets (n)
  (make-array n :adjustable nil :element-type 'sah-bucket
                :initial-contents (loop repeat n collect (-make-sah-bucket))))

(defun show-nodes (bvh &optional (n 20)) "print n bvh nodes."
  (loop for i from 0 repeat n do (lqn:out "~&~03d: ~a~%" i (aref (gmsh/bvh::bvh-nodes bvh) i))))
(defun show-int-nodes (bvh &optional (num 20)) "print n int nodes."
  (veq:4$print (bvh-int-nodes bvh) :n num))
(defun show-simd-nodes (bvh &optional (num 20)) "print n bvh simd int nodes."
  (veq:$print (bvh-simd-nodes bvh) :n num :dim 8))

