(in-package :gmsh/bvh)

(deftype bvh-node-vec () `(simple-array bvh-node))
(defun -print-bvh-node (o s)
  (declare (notinline bvh-node-num bvh-node-ref bvh-node-ax bvh-node-lvl bvh-node-bbox))
  (auxin:with-struct (bvh-node- num ref ax lvl bbox) o
    (format s "<@bvh/n ~a ↳~2<~d~> | ~a ~4@<~d~>~a  □~{ ~05,2f~}>"
            (aref #("x" "y" "z") ax) lvl (if (> num 0) "∇" "↙")
            ref (if (> num 0) (format nil "❦~1<~d~>" num) "  ")
            (coerce bbox 'list))))

(defun -print-bvh (o s)
  (declare (notinline bvh-time bvh-num-nodes bvh-num-polys bvh-lvl bvh-mode bvh-num-leaves))
  (auxin:with-struct (bvh- num-nodes num-polys lvl time mode num-leaves) o
    (format s "<@~a ❦~1<~d~> # ~a ↳~2<~d~> ∇ ~a s ~a>"
              (lqn:sdwn mode) num-leaves num-nodes lvl num-polys  time)))

(deftype bbox () `(simple-array veq:ff (6)))
(defmacro bbox (&rest rest) `(veq:f$~ (6) ,@rest))

(defstruct (bvh-node (:constructor -make-bvh-node) (:print-object -print-bvh-node))
  (num 0 :type veq:pn :read-only nil)
  (ref 0 :type veq:pn :read-only nil)
  (ax 0 :type (unsigned-byte 4) :read-only nil)
  (bbox (bbox (veq:vnval 6 0f0)) :type bbox :read-only t)
  (lvl 0 :type (unsigned-byte 4) :read-only nil))

(defun init-bvh-node-vec (n)
  (make-array n :element-type 'bvh-node :adjustable nil
                :initial-contents (loop repeat n collect (-make-bvh-node))))

(declaim (inline bvh-polyfx bvh-nodes bvh-polys))
(defstruct (bvh (:constructor -make-bvh) (:print-object -print-bvh))
  "static BVH object for rendering"
  (time 0.0 :type veq:ff :read-only t)
  (nodes (init-bvh-node-vec 1) :type bvh-node-vec :read-only nil)
  (int-nodes (veq:p4$zero 0) :type veq:pvec :read-only nil)
  (simd-nodes (veq:i4$zero 0) :type veq:ivec :read-only nil)
  (polys (veq:p3$zero 0) :type veq:pvec :read-only t)
  (polyfx (veq:f3$zero 0) :type veq:fvec :read-only t)
  (normals (veq:f3$zero 0) :type veq:fvec :read-only t)
  (mima (veq:f4$zero 0) :type veq:fvec :read-only t)
  (simd-mima (veq:f4$zero 0) :type veq:fvec :read-only t)
  (lvl 0 :type fixnum :read-only t)
  (mat nil :type veq:svec :read-only t)
  (num-leaves 0 :type fixnum :read-only t)
  (num-nodes 0 :type veq:pn :read-only t)
  (num-polys 0 :type veq:pn :read-only t)
  (mode :bvh2 :type keyword :read-only t)
  (cnt 0 :type fixnum :read-only nil))
(auxin:define-struct-load-form bvh)

; (defun -print-sah-bucket (o s)
;   (auxin:with-struct (sah-bucket- mi ma fwdcnt fwdbb bckcnt bckbb inds) o
;     (format s "<@sah/b # ~d | ~05,2f ~05,2f | fwd: ~a ~05,2f | bck: ~a ~05,2f ~%~a ~%~a>"
;             (length inds) mi ma fwdcnt fwdbb bckcnt bckbb)))
(defstruct (sah-bucket (:constructor -make-sah-bucket)
                       ; (:print-object -print-sah-bucket)
                       )
  (inds (auxin:make-adjustable-vector) :type vector)
  (mi 900000f0 :type veq:ff)       (ma -900000f0 :type veq:ff)
  (fwdcnt 0 :type veq:pn)          (bckcnt 0 :type veq:pn)
  (fwdbb (make-bb) :type veq:fvec) (bckbb (make-bb) :type veq:fvec)
  (bb (make-bb) :type veq:fvec))
(defun init-sah-buckets (n &aux)
  (make-array n :adjustable nil :initial-contents (loop repeat n collect (-make-sah-bucket))))

(defun show-nodes (bvh &optional (n 20)) "print n bvh nodes."
  (loop for i from 0 repeat n do (lqn:out "~&~03d: ~a~%" i (aref (gmsh/bvh::bvh-nodes bvh) i))))
(defun show-int-nodes (bvh &optional (num 20)) "print n int nodes."
  (veq:4$print (bvh-int-nodes bvh) :n num))
(defun show-simd-nodes (bvh &optional (num 20)) "print n bvh simd int nodes."
  (veq:$print (bvh-simd-nodes bvh) :n num :dim 8))

