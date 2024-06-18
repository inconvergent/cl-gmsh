(in-package :gmsh)

(veq:fvdef* classify-vert-fx ((:va 3 pt n) &aux (lim #.(* -2f0 *eps*)))
  (declare (veq:ff pt n lim))
  (lambda ((:va 3 p))
    (declare (veq:ff p))
    (< (veq:f3dot (veq:f3norm (f3!@- p pt)) n) lim)))

(defun vrange (n &aux (res (veq:p$zero n)))
  (declare #.*opt* (veq:pn n) (veq:pvec res))
  (loop for i of-type veq:pn from 0 below n do (setf (aref res i) i))
  res)

(veq:fvdef* p/classify-verts (msh vfx &aux (sa (list)) (sb (list)))
  (declare #.*opt* (gmsh msh) (function vfx) (list sa sb))
  (auxin:with-struct (gmsh- verts num-verts) msh
    (declare (veq:fvec verts) (veq:pn num-verts))
    (loop for (i side) of-type (veq:pn boolean)
          across (lparallel:pmap 'vector (lambda (i) (declare (veq:pn i))
                                           (list i (f@vfx (veq:f3$ verts i))))
                                         (vrange num-verts))
          if side do (push i sa) else do (push i sb)))
  (values sa sb))


(defmacro mp (expr vars &body body &aux (g (gensym "MP")))
  (etypecase vars
    (symbol `(mapc (lambda (,vars) (progn ,@body)) ,expr))
    (cons `(mapc (lambda (,g) (auxin:dsb ,vars ,g ,@body)) ,expr))))

; TODO: smarter material handling?
(veq:fvdef* plane-split (msh (:va 3 pt norm)
                         &key matfx
                         &aux (g (grph:grph)) (new-verts (list))
                              (edge-cache (make-ht)) (lerps (make-ht)))
  (declare #.*opt* (gmsh msh) (veq:ff pt norm) (grph::grph g)
                   (list new-verts) (hash-table edge-cache lerps))
  "split plane at plane."
  (auxin:with-struct (gmsh- verts uv vt edges->poly) msh
    (labels ((getlrp (a b)
               (declare (veq:pn a b))
               (auxin:dsb (a* b*) (-edg a b)
                 (if (= a a*) (gethash (list a* b*) lerps)
                   (- 1f0 (gethash (list a* b*) lerps)))))
             (setlerp (a b s)
                (declare (veq:pn a b) (veq:ff s))
                (auxin:dsb (a* b*) (-edg a b)
                  (setf (gethash (list a* b*) lerps) (if (= a a*) s (- 1f0 s)))))
             (nilpos (x y z)
                (declare (veq:pn x y z))
                (let ((np (mapcar (lambda (e) (grph:@prop g e :cut))
                                  `((,x ,y) (,y ,z) (,z ,x)))))
                  (when (= 2 (loop for n in np if n summing 1))
                        (position nil np))))
             (get-srt-edge-cache (a b p)
                (declare (veq:pn a b) (keyword p))
                (gethash (cons p (-edg a b)) edge-cache))
             (set-srt-edge-cache (a b p v &aux (ee (-edg a b)))
                (declare (veq:pn a b) (keyword p) (list ee))
                (setf (gethash (cons p ee) edge-cache) v)
                v)
             (new-poly (a b c ta tb tc &aux (p (list a b c)))
               (declare (veq:pn a b c) (list p))
               (mvb (p i) (add-poly! msh p)
                 (when p (setf (gethash p vt)
                               (roll-ind (list ta tb tc) i)))
                 p))
             (new-vert (c a lrp &aux (v (get-srt-edge-cache c a :v)))
               (declare (veq:pn c a) (veq:ff lrp))
               (if v v (set-srt-edge-cache c a :v
                         (let ((new (add-vert! msh
                                      (veq:f3lerp (veq:f3$ verts c a) lrp))))
                           (push new new-verts)
                           new))))
             (new-tex (c a lrp &aux (v (get-srt-edge-cache c a :uv)))
               (declare (veq:pn c a) (veq:ff lrp))
               (if v v (set-srt-edge-cache c a :uv
                         (add-uv! msh (veq:f2lerp (veq:f2$ uv c a) lrp))))))
      (grph:modify! (g grp)
        (loop for e being the hash-keys of edges->poly using (hash-value polys)
              for (ea eb) of-type (veq:pn veq:pn) = e
              do (veq:mvb (isect d) (veq:f3planex norm pt (veq:f3$ verts ea eb))
                   (declare (boolean isect) (veq:ff d))
                   (when (and isect (< 0f0 d 1f0))
                         (setlerp ea eb d)
                         (mp (gethash e edges->poly) p*
                             (mp (poly-as-edges p*) (pa pb)
                                 (grp-> pa pb
                                   (when (compare-norm-edges ea eb pa pb)
                                         `(:cut)))))))))
      (grph:qry g
        :select (?x ?y ?z)
        :where (and (?x _ ?y) (?y _ ?z) (?z _ ?x)
                    (% (poly-exists msh (list ?x ?y ?z))))
        :then (let ((p (list ?x ?y ?z))
                    (i (nilpos ?x ?y ?z)))
                ; TODO: improve i, figure out why it fails sometimes?
                (if i
                  (progn
                    (del-poly! msh p)
                    (auxin:dsb (a b c) (roll-ind p i)
                      (auxin:dsb (ta tb tc) (roll-ind (gethash p vt +nilpol+) i)
                        ; (veq:vpr c a (getlrp c a))
                        ; (veq:vpr b c (getlrp b c))
                        (let* ((ca-lrp (getlrp c a))
                               (bc-lrp (getlrp b c))
                               (d (new-vert c a ca-lrp))
                               (e (new-vert b c bc-lrp))
                               (td (new-tex tc ta ca-lrp))
                               (te (new-tex tb tc bc-lrp)))
                          (if matfx
                            (progn (f@matfx p (new-poly a b e ta tb te))
                                   (f@matfx p (new-poly e c d te tc td))
                                   (f@matfx p (new-poly d a e td ta te)))
                            (progn (new-poly a b e ta tb te)
                                   (new-poly e c d te tc td)
                                   (new-poly d a e td ta te)))))))
                  (wrn :modify? "unexpected state: for ~a, ~a" p i))))))
  (grph::undup new-verts))


; TODO: is this generic enough?
(veq:fvdef* plane-sym (msh (:va 3 pt norm)
                       &key matfx
                       &aux (side-idx (make-hash-table :test #'eq))
                            (vfx (classify-vert-fx pt norm)))
  ; TODO: deal with texture
  "cut and make mesh symmetric around plane."
  (labels
    ((idx (l s) (loop for i in l do (setf (gethash i side-idx) s)))
     (build-side-idx (ign sa sb) (idx sa :sa) (idx sb :sb) (idx ign :ign))
     (vert-side (verts) (loop for i in verts collect (gethash i side-idx)))
     (split (&aux (ign (plane-split msh pt norm :matfx matfx)))
       (veq:mvb (sa sb) (p/classify-verts msh vfx)

         (let ((lsa (length sa))
               (lsb (length sb)))
           (cond ((and (< lsa 1) (< lsb 1)) nil)
                 ((> lsa lsb) (build-side-idx ign sa sb) :sb)
                 (t (build-side-idx ign sa sb) :sa)))))

     (del-side (s) (loop for poly in (get-all-polys msh)
                         do (when (find s (vert-side poly))
                              (del-poly! msh poly))))
     (sym (v) (veq:xlet ((f3!p (get-vert msh v))
                         (f3!pp (f3!@- p (veq:f3scale norm
                                           (veq:f3dot norm (f3!@- p pt))))))
                (add-vert! msh (f3!@- (veq:f3scale pp 2f0) p))))
     (vrt (v &aux (h (gethash v side-idx)))
       (cond ((eq :ign h) v)
             ((numberp h) h)
             (t (let ((new (sym v)))
                  (setf (gethash v side-idx) new)
                  new))))
     (sym-poly (poly &aux (vt (gethash poly (gmsh-vt msh)))
                          (poly* (mapcar #'vrt poly))
                          (new (add-poly! msh poly* vt)))

       (unless new (error "bad condition"))
       (when matfx (f@matfx poly new))
       new))
    (let ((dels (split)))
      ; when something has been delete, otherwise the plane missed
      (when dels (del-side dels)
                 (mapc #'sym-poly (get-all-polys msh))))))

