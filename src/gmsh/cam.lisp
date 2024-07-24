(in-package :gmsh/cam)

(declaim (inline @pos @up @vpn @u @v vm pm make-uv look-vpn))
(defparameter *nav-modes*  #(:pan :fly))
(defparameter *proj-modes* #(:persp :ortho))
(defun nxt-mode (l c) (declare (vector l) (keyword c)) "return next mode in l."
  (aref l (mod (1+ (position c l)) (length l))))

(veq:fvdef -prt (o s)
  (labels ((fmtvec (name s (:va 3 v)) (lqn:fmt "~a:~a~08,2f | ~08,2f | ~08,2f" name s v)))
    (format s "<@cam (~a ~a)~%  ~a~%  ~a~%  ~a
               s |     near |      far~%  ~a~%  ~a>"
              (@proj-mode o) (@nav-mode o)
              (m@fmtvec :pos "  " (@pos o))
              (m@fmtvec :up "   " (@up o))
              (m@fmtvec :vpn "  " (@vpn o))
              (m@fmtvec :orth " " (veq:f$ (cam-par o) 0 1 2))
              (m@fmtvec :proj " " (veq:f$ (cam-par o) 3 4 5)))))

(defstruct (cam (:constructor make) (:print-object -prt))
  (nav-mode  (aref *nav-modes* 0)            :type keyword   :read-only nil)
  (proj-mode (aref *proj-modes* 0)           :type keyword   :read-only nil)
  (pos       (veq:f3$pt 150.0 150.0 50.0) :type veq:3fvec :read-only t) ; cam position
  (up        (veq:f3$pt 0.0 1.0 0.0)      :type veq:3fvec :read-only t) ; cam up
  (vpn       (veq:f3$pt 0.0 0.0 1.0)      :type veq:3fvec :read-only t) ; away from dop
  (par (veq:f$~ (6) 1000.0 0.1 1000.0                                   ; ortho: s near far
                       0.1 0.1 1000.0)                                  ; proj:  s near far
                        :type veq:fvec  :read-only t)
  (u   (veq:f3$val 0.0) :type veq:3fvec :read-only t)                   ; cam horizontal
  (v   (veq:f3$val 0.0) :type veq:3fvec :read-only t))                  ; cam vertical

(veq:make-struct-vec-getters cam- 3 ff (pos up vpn u v par))
(veq:make-struct-vec-setters cam- 3 ff (pos up vpn u v par))
(veq:fvdef @nav-mode  (c) "get current nav mode."  (cam-nav-mode c))
(veq:fvdef @proj-mode (c) "get current proj mode." (cam-proj-mode c))

(defun set-nav-mode (c mode) (declare (cam c) (keyword mode))
  "set nav mode see: *proj-modes*"
  (setf (cam-nav-mode c) mode))
(defun roll-nav-mode (c) "next nav-mode. see *nav-modes*"
  (set-nav-mode c (nxt-mode *nav-modes* (@nav-mode c))))

(defun set-proj-mode (c mode) (declare (cam c) (keyword mode))
  "set proj mode. see: *proj-modes*"
  (setf (cam-proj-mode c) mode))
(defun roll-proj-mode (c) "next proj mode. see *proj-modes*"
  (set-proj-mode c (nxt-mode *proj-modes* (@proj-mode c))))

(veq:fvdef vm (c) (declare #.*opt* (cam c))
  "view matrix, compatible with gmsh/scene and gmsh/xrend."
  (veq::m@fmake-view-matrix (@pos c) (f3!@- (@pos c) (@vpn c)) (@up c)))
(veq:fvdef pm (c &optional (mode (@proj-mode c))) (declare #.*opt* (cam c) (keyword mode))
  "projection matrix. compatible with gmsh/scene and gmsh/xrend."
  (veq:xlet ((f3!par (proj-par c mode)))
    (ecase mode (:ortho (veq:fmake-ortho-proj-matrix (:vr par 0 0 1 2)))
                (:persp (veq:fmake-proj-matrix       (:vr par 0 0 1 2))))))

(defun scale-from-to! (c from to)
  (declare (cam c) (veq:pn from to))
  (veq:xlet ((par (cam-par c)) (f!r (veq:ff (/ to from))))
    (setf (aref par 0) (* r (aref par 0)) (aref par 3) (* r (aref par 3))))
  c)
(defun proj-par (c &optional (mode (@proj-mode c)))
  (declare #.*opt* (cam c) (keyword mode))
  "get s/near/far as values for current mode (or mode)."
  (ecase mode (:ortho (veq:f$ (cam-par c) 0 1 2))
              (:persp (veq:f$ (cam-par c) 3 4 5))))
(defun nav/s! (c val &optional (mode (@proj-mode c)))
  (declare #.*opt* (cam c) (veq:ff val) (keyword mode))
  "change s value of current projection mode."
  (ecase mode (:ortho (incf (aref (cam-par c) 0) val))
              (:persp (incf (aref (cam-par c) 3) val)))
  c)
(defun nav/near! (c val &optional (mode (@proj-mode c)))
  (declare #.*opt* (cam c) (veq:ff val) (keyword mode))
  "change near value of current projection mode."
  (ecase mode (:ortho (incf (aref (cam-par c) 1) val))
              (:persp (incf (aref (cam-par c) 4) val)))
  c)
(defun nav/axis! (c axis &optional (stp 1.0) (trans 16.0) (yaw 0.03)
                                             (pitch 0.03) (roll 0.1))
  (declare #.*opt* (cam c) (veq:fvec axis) (veq:ff stp trans yaw pitch roll))
  (labels ((trans  (ax val) (nav/trans!  c ax val))
           (around (ax val) (nav/around! c ax val)))
    (loop for (fx ax s) of-type (function keyword veq:ff)
          in (ecase (@nav-mode c)
               (:fly `((,#'trans  :u    ,(- trans)) (,#'trans  :vpn      ,trans)
                       (,#'around :yaw     ,yaw)    (,#'around :pitch    ,pitch)
                       (,#'around :roll    ,roll)   (,#'around :roll  ,(- roll))))
               (:pan `((,#'trans  :u    ,(- trans)) (,#'trans  :v     ,(- trans))
                       (,#'around :yaw     ,yaw)    (,#'around :pitch    ,pitch)
                       (,#'around :roll    ,roll)   (,#'around :roll  ,(- roll)))))
          for val across axis if (and fx (> (abs val) veq:*eps*))
          do (funcall (the function fx) ax (* stp s val))))
  c)

(veq:fvdef make-uv ((:va 3 up vpn)) (declare #.*opt* (veq:ff up vpn))
  "calculate cam plane u/v vectors."
  (veq:xlet ((f3!v (veq:f3norm (f3.@- (veq:f3from up vpn (- (veq:f3dot up vpn)))))))
    (unless (< (abs (veq:f3dot up vpn)) #.(- 1.0 veq:*eps*))
            (wrn :gimbal-lock "gmsh/cam: gimbal lock.~%up: ~a~%vpn: ~a"
                              (list up) (list vpn)))
    (veq:~ (veq:f3rot v vpn veq:fpi5) v)))

(veq:fvdef look-vpn (pos look) (declare #.*opt* (veq:3fvec pos look))
  (veq:f3norm (f3!@- (veq:f3$ pos) (veq:f3$ look))))

(veq:fvdef update! (c &key up pos vpn look) (declare #.*opt* (cam c))
  "consistently update camera values. use vpn to set view plane normal (away
from dop); or look to set view plane normal relative to position."
  (when (and vpn look) (wrn :update  "update: can only use (or vpn look)."))
  (when pos  (setf (veq:3$ (cam-pos c)) (veq:f3$ pos)))
  (when up   (setf (veq:3$ (cam-up  c)) (veq:f3$ up)))
  (when vpn  (setf (veq:3$ (cam-vpn c)) (veq:f3$ vpn)))
  (when look (setf (veq:3$ (cam-vpn c)) (look-vpn (cam-pos c) look)))
  (when (or pos up vpn look)
        (veq:xlet ((f6!uv (m@make-uv (@up c) (@vpn c))))
          (setf (veq:3$ (cam-u c)) (veq:f3 (:vr uv 0 1 2))
                (veq:3$ (cam-v c)) (veq:f3 (:vr uv 3 4 5)))))
  c)

(veq:fvdef nav/trans! (c ax val) (declare #.*opt* (cam c) (keyword ax) (veq:ff val))
  "translate cam."
  (update! c :pos (veq:f3$pt (f3!@+ (@pos c)
                               (f3!@.* val (ecase ax (:u (@u c)) (:v (@v c))
                                                     (:vpn (@vpn c))))))))

(veq:fvdef nav/around! (c ax val) (declare #.*opt* (cam c) (keyword ax) (veq:ff val))
  "rotate around cam axis."
  (unless (> (abs val) 0) (return-from nav/around! nil))
  (macrolet ((rot (a b) `(veq:f3norm (veq:f3rot (veq:f3$s c cam- ,a ,b val)))))
    (ecase ax (:yaw   (update! c :vpn (veq:f3$pt (rot :vpn :up))))
              (:roll  (update! c :up  (veq:f3$pt (rot :up  :vpn))))
              (:pitch (update! c :up  (veq:f3$pt (rot :up  :u))
                                 :vpn (veq:f3$pt (rot :vpn :u)))))))

(defun export-data (c) (declare (cam c))
  "serialize cam. see import-data."
  (auxin:with-struct (cam- vpn pos up par nav-mode proj-mode) c
    `((:vpn . ,vpn) (:pos . ,pos) (:up . ,up) (:nav-mode . ,nav-mode)
      (:proj-mode . ,proj-mode) (:par . ,par))))

(defun import-data (o) (declare (list o))
  "load serialized cam. see export-data."
  (labels ((gk (k) (cdr (assoc k o)))
           (as-arr (k) (veq:f_ (coerce (gk k) 'list))))
    ; TODO: refactor this
    (let* ((c (make :pos (as-arr :pos) :vpn (as-arr :vpn) :up (as-arr :up)))
           (par-place (cam-par c))
           (par (as-arr :par)))
      (setf (veq:3$ par-place 0) (veq:3$ par 0)
            (veq:3$ par-place 1) (veq:3$ par 1))
      (update! c))))

