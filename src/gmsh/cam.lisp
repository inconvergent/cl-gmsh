(in-package :gmsh/cam)


(declaim (inline 3identity zero))
(veq:fvdef 3identity ((:va 3 x)) (values x))
(veq:fvdef zero () (veq:f3$point 0f0 0f0 0f0))

(defstruct (cam)
  (vpn (zero) :type veq:fvec :read-only nil) ; away from dop
  (up (zero) :type veq:fvec :read-only nil) ; cam up
  (pos (zero) :type veq:fvec :read-only nil) ; cam position
  (u (zero) :type veq:fvec :read-only nil) ; view plane horizontal
  (v (zero) :type veq:fvec :read-only nil) ; view plane vertical
  (su (zero) :type veq:fvec :read-only nil) ; u scaled by s
  (sv (zero) :type veq:fvec :read-only nil) ; v scaled by s
  (xy (veq:f2$val 0.0) :type veq:fvec :read-only nil) ; offset in view plane
  (s 1f0 :type veq:ff :read-only nil)) ; scale

(declaim (inline @pos @s @up @vpn @xy))
(veq:fvdef @pos (proj) "get pos." (veq:f3$s proj cam- :pos))
(veq:fvdef @s (proj) "get scale." (cam-s proj))
(veq:fvdef @up (proj) "get up." (veq:f3$s proj cam- :up))
(veq:fvdef @vpn (proj) "get vpn." (veq:f3$s proj cam- :vpn))
(veq:fvdef @xy (proj) "get view plane offset." (veq:f2$s proj cam- :xy))

(veq:fvdef -get-u-v (up* vpn* &optional (s 1f0))
  (declare #.*opt* (veq:fvec up* vpn*) (veq:ff s))
  (veq:xlet ((f3!up (veq:f3$ up*))
             (f3!vpn (veq:f3$ vpn*)))

    (unless (< (abs (veq:f3dot up vpn)) #.(- 1f0 veq:*eps*))
            (error "gmsh/cam: gimbal lock.~%up: ~a~%vpn: ~a" up* vpn*))

    (veq:xlet ((f3!v (veq:f3norm (f3.@- (veq:f3from up vpn (- (veq:f3dot up vpn))))))
               (f3!u (veq:f3rot v vpn veq:fpi5)))
      (veq:~ u v (f3!@*. u s) (f3!@*. v s)))))

(veq:fvdef -look (pos look)
  (declare #.*opt* (veq:fvec pos look))
  (veq:f3$point (veq:f3norm (f3!@- (veq:f3$ pos) (veq:f3$ look)))))

(veq:fvdef make (&key (s 1f0) (pos (veq:f3$val 1000f0))
                      vpn look (up (veq:f3$point 0f0 0f0 1f0))
                      (xy (veq:f2$val 500f0)))
  (declare (veq:fvec up pos xy) (veq:ff s))
  "make camera

  default up is (0 0 1)
  default pos is (1000.0 1000.0 1000.0)
  if look and vpn are unset, the camera will look at the origin.

  default scale is 1.0
  default xy is (0.0 0.0)"
  (assert (not (and vpn look)) (vpn look)
          "make: can only use (or vpn look)." vpn look)

  (let* ((look (if look look (veq:f3$val 0f0)))
         (vpn* (if vpn vpn (-look pos look))))
    (auxin:mvb ((:va 3 u v su sv)) (-get-u-v up vpn* s)
      (declare (veq:ff u v su sv))
      (make-cam :vpn vpn* :up up :pos pos :s s :xy xy
                :u (veq:f3$point u)   :v (veq:f3$point v)
                :su (veq:f3$point su) :sv (veq:f3$point sv)))))

(veq:fvdef update (proj &key s xy up pos vpn look) (declare #.*opt* (cam proj))
  "update projection parameters.

  use vpn to set view plane normal directly, or look to set view plane normal
  relative to camera.

  ensures that internal state is updated appropriately."
  (assert (not (and vpn look)) (vpn look)
          "update: can only use (or vpn look)." vpn look)

  (when pos (setf (cam-pos proj) pos))
  (when up (setf (cam-up proj) up))
  (when vpn (setf (cam-vpn proj) vpn))
  (when look (setf (cam-vpn proj) (-look (cam-pos proj) look)))
  (when s (setf (cam-s proj) (the veq:ff s)))
  (when xy (setf (cam-xy proj) xy))
  ; TODO: there appears to be a bug where u,v is not updated if pos is changed??
  (when (or pos s up vpn look)
        (auxin:mvb ((:va 3 u v su sv))
                   (-get-u-v (cam-up proj) (cam-vpn proj) (cam-s proj))
          (declare (veq:ff u v su sv))
          (setf (veq:3$ (cam-u proj)) (veq:f3 u)
                (veq:3$ (cam-v proj)) (veq:f3 v)
                (veq:3$ (cam-su proj)) (veq:f3 su)
                (veq:3$ (cam-sv proj)) (veq:f3 sv))))
  proj)

(veq:fvdef around (c axis val &key (look (veq:f3$point (veq:f3rep 0f0))))
  (declare (cam c) (keyword axis) (veq:ff val) (veq:fvec look))
  (unless (> (abs val) 0) (return-from around nil))
  (macrolet ((_ (&rest rest) `(veq:fvprogn (veq:f_ (veq:lst ,@rest))))
             (rot (a b) `(veq:fvprogn (veq:f3rots (veq:f3$s c cam- ,a ,b val) 0f0 0f0 0f0))))
    (case axis
      (:pitch (veq:f3let ((pos (rot :pos :u))
                          (up (veq:f3norm (rot :up :u)))
                          (vpn (veq:f3norm (f3!@- pos (veq:f3$ look)))))
                (update c :pos (_ pos) :vpn (_ vpn) :up (_ up))))
      (:yaw (veq:f3let ((pos (rot :pos :v))
                        (up (veq:f3norm (rot :up :v)))
                        (vpn (veq:f3norm (f3!@- pos (veq:f3$ look)))))
                (update c :pos (_ pos) :vpn (_ vpn) :up (_ up))))
      (:roll (update c :up (veq:f3$point
                             (veq:f3rot (veq:f3$s c cam- :up :vpn val))))))))

(veq:fvdef* vm (p (:va 3 look)) (declare #.*opt* (cam p) (veq:ff look))
  "view matrix, compatible with gmsh/scene"
  (veq::fmake-view-matrix (veq:f3$ (cam-pos p)) look
                         (veq:f3$ (cam-up p))))

; (veq:fvdef pm (p s &optional (near 0.1) (far 500f0) &aux
;                  ; (s (/ s))
;                  )
;   (declare #.*opt* (cam p) (veq:ff near far s)) "projection matrix. compatible with gmsh/scene"
;   (veq::fmake-proj-matrix s s near far))

(veq:fvdef pm (p s &optional (near 0.1) (far 500f0))
  (declare #.*opt* (cam p) (veq:ff near far s)) "projection matrix. compatible with gmsh/scene"
  (veq:fmake-ortho-proj-matrix s s near far))

(defun export-data (p) (declare (cam p))
  "export the neccessary values to recreate cam. import with gmsh/cam:import-data."
  (auxin:with-struct (cam- vpn pos up s xy) p
    `((:vpn . ,vpn) (:pos . ,pos) (:up . ,up)
      (:s . ,s) (:xy . ,xy))))

(defun import-data (p) (declare (list p))
  "recreate proj from an a list exported by gmsh/cam:export-data."
  (labels ((as (i a) (cdr (assoc i a)))
           (as-float (i a) (veq:ff (as i a)))
           (as-arr (i a) (veq:f_ (coerce (as i a) 'list))))
    ; NOTE: old (ortho) exports use :cam, instead of :pos
    (make :pos (if (car (assoc :pos p)) (as-arr :pos p) (as-arr :cam p))
          :vpn (as-arr :vpn p) :up (as-arr :up p)
          :xy (as-arr :xy p) :s (as-float :s p))))

