(in-package :gmsh)

(declaim (inline -edg -verify-poly))
(defun -edg (a b) (declare #.*opt* (veq:pn a b)) (if (< a b) (list a b) (list b a)))
(defun -verify-poly (a b c) (declare #.*opt* (veq:pn a b c))
  (unless (and (/= a b) (/= b c) (/= a c))
          (error "bad poly: (~a ~a ~a)" a b c))
  t)

(defun poly-as-edges (p) (declare #.*opt* (list p))
  (loop for a in p and b in (append (cdr p) (list (first p)))
        collect (list a b)))

(defun compare-norm-edges (a b c d) (declare #.*opt* (veq:pn a b c d))
  (= 2 (length (intersection (list a b) (list c d)))))

(defun make-ht (&optional (tst #'equal) (s 20) (r 2f0)) (declare (number s r))
  (make-hash-table :test tst :size s :rehash-size r))

(veq:fvdef -poly-normal (v) (declare #.*opt* (veq:fvec v))
  (handler-case (veq:xlet ((f3!a (veq:f3$ v 0)))
                  (veq:f3norm (veq:f3cross (f3!@- (veq:f3$ v 1) a)
                                           (f3!@- (veq:f3$ v 2) a))))
    (error (e) (declare (ignore e)) (rnd:3on-sphere 1f0))))

(defun -print-gmsh (o s) (declare (notinline  gmsh-polys gmsh-num-uvs gmsh-num-verts))
  (auxin:with-struct (gmsh- num-verts polys num-uvs) o
    (format s "<@gmsh · ~a ∇ ~a ◩ ~a>" num-verts (hash-table-count polys) num-uvs)))

(defstruct (gmsh (:constructor make-gmsh)
                 (:print-object -print-gmsh))
  "simple triangular mesh object with mesh manipulation and texture mapping."
  (obj-props (list))
  (polys (make-ht #'equal 5000) :type hash-table) ; (a b c) -> t
  (edges->poly (make-ht #'equal 5000) :type hash-table)
  (num-uvs 0 :type veq:pn) (num-verts 0 :type veq:pn) (max-verts 100000 :type veq:pn)
  (verts nil) (norms nil) ; 3d fvec
  (uv nil)    ; 2d fvec
  (vn nil)    ; ht (poly) -> (n0 n1 n2)
  (vt nil))   ; ht (poly) -> (uv0 uv1 uv2)

; TODO: unify constructor with grph
(defun gmsh (&key (max-verts 100000) props) (declare (veq:pn max-verts) (list props))
  "make gmsh object."
  (make-gmsh :verts (veq:f3$zero max-verts) :norms (veq:f3$zero max-verts)
             :uv (veq:f2$zero max-verts)    :vt (make-ht) :vn (make-ht)
             :num-verts 0 :num-uvs 0 :max-verts max-verts
             :obj-props props))

(defun clear! (gmsh) (declare (gmsh gmsh)) "clear gmsh."
  (clrhash (gmsh-vt gmsh))          (clrhash (gmsh-vn gmsh))
  (clrhash (gmsh-edges->poly gmsh)) (clrhash (gmsh-polys gmsh))
  (setf (gmsh-obj-props gmsh) (list)
        (gmsh-num-uvs gmsh) 0
        (gmsh-num-verts gmsh) 0)
  gmsh)

(defun get-all-polys (msh) (declare (gmsh msh)) "get list of all polys."
  (loop for k of-type list being the hash-keys of (gmsh-polys msh)
        collect k of-type list))

(defun get-num-verts (msh)    (declare (gmsh msh)) "get number of verts"      (gmsh-num-verts msh))
(defun get-max-verts (msh)    (declare (gmsh msh)) "get max number of verts." (gmsh-max-verts msh))
(defun get-num-polys (msh)    (declare (gmsh msh)) "get number of polys."     (hash-table-count (gmsh-polys msh)))
(defun get-poly-edges (msh p) (declare (gmsh msh)) "get edges of poly."       (gethash (the list p) (gmsh-polys msh)))
(defun get-edge-polys (msh e) (declare (gmsh msh)) "get polys of edge."
  (gethash (the list (apply #'-edg (the list e))) (gmsh-edges->poly msh)))

(defun poly-exists (msh p) (declare (gmsh msh) (list p))
  "return poly if it exists; or nil"
  (gethash p (gmsh-polys msh)))
(defun norm-poly (poly &aux (i (find-min-ind poly))) (declare (veq:pn i))
  "normalize polygon. smallest ind first. maintains order."
  (values (roll-ind poly i) i))

(defun add-poly! (msh poly &optional vt) (declare #.*opt* (gmsh msh) (list poly vt))
  "add poly to msh. returns poly if it was created; or nil."
  (apply #'-verify-poly poly)
  (auxin:with-struct (gmsh- polys edges->poly) msh
    (declare (hash-table polys edges->poly))
    (labels ((-add-poly-edges (p) (declare (list p))
               (loop for (a b) of-type (veq:pn veq:pn) in (poly-as-edges p)
                     for e of-type list = (-edg a b)
                     do (setf (gethash e edges->poly)
                              (cons p (gethash e edges->poly (list))))))
             (-add-poly (p vt) (declare (list p vt))
               (when (poly-exists msh p)
                     (wrn :add-poly! "ignoring dupe: ~a" p)
                     (return-from -add-poly nil))
               (-add-poly-edges p)
               (setf (gethash p polys) t)
               (when vt (add-vt! msh p vt))
               p))
      (auxin:mvb (p i) (norm-poly poly)
        (values (-add-poly p (when vt (roll-ind vt i)))
                i)))))

(defun add-polys! (msh pp) (declare (gmsh msh) (list pp)) "add these polygons."
  (loop for p of-type list in pp collect (add-poly! msh p)))

(defun del-poly! (msh poly) (declare #.*opt* (gmsh msh) (list poly)) "delete this poly."
  (auxin:with-struct (gmsh- polys edges->poly) msh
    (declare (hash-table edges->poly polys))
    (labels ((-del-poly-edges (p edge)
               (declare (list p edge))
               (let ((new-poly-list (remove-if (lambda (p*) (equal (the list p*) p))
                                               (gethash edge edges->poly))))
                 (declare (list new-poly-list))
                 (if new-poly-list (setf (gethash edge edges->poly) new-poly-list)
                                   (remhash edge edges->poly)))))
      (unless (gethash poly polys) (return-from del-poly! nil))
      (loop for (a b) in (poly-as-edges poly)
            do (-del-poly-edges poly (-edg a b)))
      (remhash poly polys)
      poly)))
(defun del-polys! (msh polys) (declare (gmsh msh) (list polys)) "delete these polys."
  (loop for p of-type list in polys collect (del-poly! msh p)))

(veq:fvdef* add-vert! (msh (:va 3 x)) "add vert."
  (auxin:with-struct (gmsh- verts num-verts) msh
    (setf (veq:3$ verts num-verts) (veq:f3 x))
    (incf (gmsh-num-verts msh))
    num-verts))
(veq:fvdef add-verts! (msh v) "add these verts."
  (loop for i from 0 below (veq:f3$num v) collect (add-vert! msh (veq:f3$ v i))))
(veq:fvdef* add-uv! (msh (:va 2 x)) "add uv mapping"
  (auxin:with-struct (gmsh- uv num-uvs) msh
    (setf (veq:2$ uv num-uvs) (veq:f2 x))
    (incf (gmsh-num-uvs msh))
    num-uvs))

; TODO: rename to set? export
(defun add-vt! (msh p v) (auxin:with-struct (gmsh- vt) msh (setf (gethash p vt) v)))

(veq:fvdef get-vert (msh i) (declare (gmsh msh) (veq:pn i)) "get vert 3d pos (3d)."
  (veq:f3$ (gmsh-verts msh) i))
(veq:fvdef get-norm (msh i) (declare (gmsh msh) (veq:pn i)) "get normal vector (3d)."
  (veq:f3$ (gmsh-norms msh) i))
(veq:fvdef get-uv (msh i) (declare (gmsh msh) (veq:pn i)) "get uv pos (2d)."
  (veq:f2$ (gmsh-uv msh) i))

(veq:fvdef get-verts (msh inds) (declare (gmsh msh) (list inds)) "get verts as veq:fvec (3d)."
  (f3.@$identity (l?@ (gmsh-verts msh) inds)))
(veq:fvdef get-norms (msh inds) (declare (gmsh msh) (list inds)) "get normals as veq:fvec (3d)."
  (f3.@$identity (l?@ (gmsh-norms msh) inds)))
(veq:fvdef get-uvs (msh inds) (declare (gmsh msh) (list inds)) "get uvs as veq:fvec (2d)."
  (f2.@$identity (l?@ (gmsh-uv msh) inds)))

(defun get-connected-verts (msh &aux (res (make-hash-table :test #'eql)))
  (declare (gmsh msh)) "return all connected verts."
  (auxin:with-struct (gmsh- edges->poly) msh
    (loop for (a b) being the hash-keys of edges->poly do (setf (gethash a res) t (gethash b res) t))
    (loop for k being the hash-keys of res collect k))) ; this was sorted <. does it matter?

(veq:fvdef tx! (msh inds fx)
  (declare (gmsh msh) (list inds) (function fx)) "apply this transform to verts."
  (auxin:with-struct (gmsh- verts) msh (declare (veq:fvec verts))
    (f3%@$do-tx! (l?@ verts inds) (((:va 3 x)) (f@fx x)))))

; TODO:
; (veq:fvdef move-vert (msh v &optional (relative t)))
(veq:fvdef center! (msh &key max-side (connected t)) (declare (gmsh msh) (boolean connected))
  "center gmsh. optionally scale max side t, only affect connected verts."
  (labels ((safe/ (a b) (if (< (abs b) veq:*eps*)
                          (progn (wrn :center! "/0 for (/ ~a ~b)." a b) a)
                          (/ a b)))
           (scale-by (x y z) (declare (veq:ff x y z))
             (cond ((grph:first> x y z) (safe/ (the veq:ff max-side) x))
                   ((grph:first> y x z) (safe/ (the veq:ff max-side) y))
                   (t (safe/ (the veq:ff max-side) z)))))
    (auxin:with-struct (gmsh- verts num-verts) msh
      (declare (veq:fvec verts) (veq:pn num-verts))
      (auxin:mvb (xmi xma ymi yma zmi zma)
        (if connected (veq:f3$mima verts :inds (get-connected-verts msh))
                      (veq:f3$mima verts :n (gmsh-num-verts msh)))
        (declare (veq:ff xmi xma ymi yma zmi zma))
        (veq:xlet ((f3!mx (f3!@*. (f3!@+ xmi ymi zmi xma yma zma) 0.5f0))
                   (f3!wh (f3!@- xma yma zma xmi ymi zmi))
                   (f!s (if max-side (scale-by wh) 1f0)))
          (labels ((cent ((:va 3 x)) (f3!@*. (f3!@- x mx) s)))
            (f3_@$cent! (?@ verts 0 num-verts)))
          (values mx wh s))))))

(veq:fvdef make-bvh (msh &key (num 5) matfx (mode :bvh2-stackless))
  (declare #.*opt1* (gmsh msh) (veq:pn num)) "construct bvh for rendering."
  (gmsh/bvh::make (get-all-polys msh)
    (lambda (verts) (declare (list verts)) (get-verts msh verts))
    :matfx matfx :num num :mode mode))


(veq:fvdef add-verts-polys! (msh verts polys)
  (declare (gmsh msh) (veq:fvec verts) (list polys))
  "add verts. then add tris from polys. using the indices in poly
  relative to new-verts. returns tris"
  (let ((new-inds (auxin:to-vector (add-verts! msh verts))))
    (values (loop for p in polys
              collect (add-poly! msh (mapcar (lambda (v) (aref new-inds v)) p)))
            new-inds)))

(veq:fvdef* $shape/rect ((:va 3 xy u v)) (declare (veq:ff xy u v))
  "rectangle centered around xy offset by the two (size) vectors: u v"
  (veq:f$~ (#.(* 3 4)) (f3!@+ xy (f3!@+ u v)) (f3!@+ xy (f3!@- u v))
                       (f3!@+ xy (f3.@- (f3!@+ u v))) (f3!@+ xy (f3!@- v u))))

(veq:fvdef* add-plane! (msh (:va 3 xy) &optional (sx 1f0) (sy sx))
  (declare (gmsh msh) (veq:ff xy sy sx))
  "add plane centered at xy with size (sx sy). retunrns list with 2 tris"
  (veq:xlet ((f2!s (f2!@*. sx sy 0.5)))
    (add-verts-polys! msh ($shape/rect xy (:vr s 0) 0f0 0f0 0f0 (:vr s 1) 0f0)
                      `((0 1 2) (2 3 0)))))

(veq:fvdef $shape/box ((:va 3 xy u v w)) (declare (veq:ff xy u v))
  "box centered around xy offset by the three (size) vectors: u v w"
  (veq:f$~ (#.(* 3 4 2))
    (f3!@+ (f3!@- xy w) (f3!@+ u v)) (f3!@+ (f3!@- xy w) (f3!@- u v))
    (f3!@+ (f3!@- xy w) (f3.@- (f3!@+ u v))) (f3!@+ (f3!@- xy w) (f3!@- v u))
    (f3!@+ (f3!@+ xy w) (f3!@+ u v)) (f3!@+ (f3!@+ xy w) (f3!@- u v))
    (f3!@+ (f3!@+ xy w) (f3.@- (f3!@+ u v))) (f3!@+ (f3!@+ xy w) (f3!@- v u))))

(veq:fvdef* add-box! (msh (:va 3 xy) &optional (sx 1f0) (sy sx) (sz sy))
  (declare (gmsh msh) (veq:ff xy sx sy sz))
  "add box centered at xy, with size (sx sy sz). returns polys"
  (veq:xlet ((f3!s (f3!@*. sx sy sz 0.5)))
    (add-verts-polys! msh
      ($shape/box xy (:vr s 0) 0f0 0f0 0f0 (:vr s 1) 0f0 0f0 0f0 (:vr s 2))
      '#.(group `(1 2 3 7 6 5 4 5 1 5 6 2 2 6 7 0 3 7
                        0 1 3 4 7 5 0 4 1 1 5 2 3 2 7 4 0 7) 3))))

(defun add-vert-strip! (msh aa bb &optional closed)
  (declare (gmsh msh) (list aa bb) (boolean closed))
  "add triangle strip from list of lists of vert indices. returns tris"
  (when closed (setf aa (math:close-path aa) ; TODO: is this destructive?
                     bb (math:close-path bb)))
  (loop with res = (list)
        with curr with a = (pop aa) with b = (pop bb)
        while (and aa bb) for c = (pop aa) for d = (pop bb)
        do (push (add-poly! msh (list a b c)) res)
           (push (add-poly! msh (list b c d)) res)
           (setf a c b d)
        finally (return res)))

(defun add-vert-grid! (msh grid &optional closed)
  (declare (gmsh msh) (list grid) (boolean closed))
  "add tris from a list of lists of indices. returns tris"
  (loop for aa in grid for bb in (cdr grid)
        nconc (add-vert-strip! msh aa bb closed)))

(veq:fvdef add-grid! (msh lpts &optional closed)
  (declare (gmsh msh) (list lpts) (boolean closed))
  "add tris from list of fvecs. returns tris"
  (add-vert-grid! msh (loop for pts in lpts collect (add-verts! msh pts))
                      closed))

; TODO: generic back wall, generic plane?
; TODO: tx new-verts, using generic return format for new verts/polys?
; just use somthing like: ((verts . #(1 2 3)) (polys . ((...) (...))))
; can we make it chainable?

; (veq:fvdef add-back-wall (msh proj)
;   (auxin:with-struct (ortho::ortho- cam u v vpn) proj
;     (veq:xlet ((f3!cam* (f3!@+ (veq:f3$ cam)
;                                (f3!@*. (veq:f3$ vpn) -870f0)))
;                (f3!u* (veq:f3$ u)) (f3!v* (veq:f3$ v))
;                (v (gmsh::add-verts! msh
;                     (veq:f$~ (12) (f3!@+ (rnd:3in-sphere 3f0)
;                                          (veq:f3from cam* (f3!@+ u* v*) 30f0))
;                                   (f3!@+ (rnd:3in-sphere 3f0)
;                                          (veq:f3from cam* (f3!@- u* v*) 30f0))
;                                   (f3!@+ (rnd:3in-sphere 3f0)
;                                          (veq:f3from cam* (f3.@- (f3!@+ v* u*)) 30f0))
;                                   (f3!@+ (rnd:3in-sphere 3f0)
;                                          (veq:f3from cam* (f3!@- v* u*) 30f0))))))
;       ; (gmsh:add-polys! msh (list (list (first v) (third v) (second v))
;       ;                            (list (first v) (third v) (fourth v)))))))

