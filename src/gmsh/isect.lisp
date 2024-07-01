(in-package :gmsh)

(veq:fvdef* classify-vert-fx ((:va 3 pt n) &aux (lim #.(* -2.0 *eps*)))
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
               (auxin:dsb (a* b*) (-edg a b) (declare (veq:pn a* b*))
                 (if (= a a*) (gethash (list a* b*) lerps)
                   (- 1.0 (the veq:ff (gethash (list a* b*) lerps))))))
             (setlerp (a b s)
                (declare (veq:pn a b) (veq:ff s))
                (auxin:dsb (a* b*) (-edg a b) (declare (veq:pn a* b*))
                  (setf (gethash (list a* b*) lerps) (if (= a a*) s (- 1.0 s)))))
             (nilpos (x y z)
                (declare (veq:pn x y z))
                (let ((np (mapcar (lambda (e) (grph:@prop g e :cut))
                                  `((,x ,y) (,y ,z) (,z ,x)))))
                  (when (= 2 (loop for n in np if n summing (the veq:pn 1)))
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
                   (when (and isect (< 0.0 d 1.0))
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
                (add-vert! msh (f3!@- (veq:f3scale pp 2.0) p))))
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

(veq:fvdef split-edge! (msh e &key matfx)
  (declare #.*opt* (gmsh msh) (list e)) "split this edge"
  (auxin:with-struct (gmsh- edges->poly) msh
    (declare (hash-table edges->poly))
    ; TODO: unless edge exists, exit
    ; TODO: textures
    ; TODO: inject edge
    (unless (gethash e edges->poly) (return-from split-edge! nil))
    (dsb (a b) e
     (let ((nv (add-vert! msh (veq:f3mid (veq:f3$ (get-verts msh e) 0 1)))))
       (labels ((mat (from to) (when matfx (f@matfx from to)) from)
                (do-split (p &aux (c (car (set-difference p e))))
                  (list (mat p (add-poly! msh `(,a ,nv ,c)))
                        (mat p (add-poly! msh `(,nv ,b ,c))))))
         (loop for p in (del-polys! msh (gethash e edges->poly))
               nconc (do-split p)))))))

(defmacro reorder (a &rest rest)
  (declare (symbol a))
  `(concatenate 'vector
    ,@(loop for ind in rest
            collect (typecase ind (number `(list (aref ,a ,ind)))
                                  (symbol `(list (aref ,a ,ind)))
                                  (cons (case (length ind)
                                          (1 `(list (aref ,a ,@ind)))
                                          (2 `(subseq ,a ,@ind))))))))

; (veq:fvdef 3triangulate (wer edge-set &key (fx '-idxy))
;   (declare (weir wer) (list edge-set))
;   "triangulate the hull defined by edge set, using the projection provided
; by fx where fx: R3 -> R2. default projection is xyz -> xy."
;   (let ((path (weird:tv (graph:edge-set->path edge-set)))
;         (res (list)))
;     (declare (vector path))
;     (labels ((gv (i) (declare (veq:pn i)) (veq:mvcgrp (3 fx) (3gv wer i)))
;              (ypos (v) (declare (veq:pn v)) (veq:fsel (:y) (gv v)))
;              (ind-rotate (path ymost)
;                (declare (vector path) (veq:pn ymost))
;                (weird:reorder path (ymost nil) (0 ymost)))
;              (y-most (path &aux (ypos (ypos (aref path 0))) (cv 0) )
;                (loop for v across (subseq path 1)
;                      for i from 1
;                      if (< (ypos v) ypos)
;                      do (setf ypos (ypos v) cv i))
;                cv)
;              (cross (path &aux (n (length path)))
;                 "does any segment in path cross the line (aref path 1) (aref path -1)."
;                 (veq:f2let ((a (gv (aref path 1)))
;                             (b (gv (weird:vl path))))
;                   (loop for i from 0 below n
;                         do ; weird precision issue. override veq eps
;                            ; TODO: can we fix this somehow?
;                            (let ((veq::*eps* (* 1000f0 veq::*eps*)))
;                              (when (veq:f2segx a b (veq:mvcgrp (3 fx)
;                                                      (3$verts wer (aref path i)
;                                                        (aref path (mod (1+ i) n)))))
;                                (return-from cross t)))))
;                 nil)
;              (uw-farthest (path &aux (n (length path)) dst (curr -1))
;               "find the vertex in triangle ((aref path 1) (aref path 0) (aref path -1))
;                that is the farthest away from (aref path 1) (aref path -1)."
;                (loop for i from 2 below (1- n)
;                      if ; TODO: this feels very sketchy
;                        (let ((veq::*eps* (* 1000f0 veq::*eps*)))
;                           (veq:f2in-triangle
;                             (veq:mvcgrp (3 fx)
;                               (3$verts wer (aref path 0) (aref path 1)
;                                (weird:vl path) (aref path i)))))

;                      do (let ((d (veq:f2segdst
;                                    (veq:mvcgrp (3 fx)
;                                      (3$verts wer (aref path 1) (weird:vl path)
;                                                   (aref path i))))))
;                           (when (or (= curr -1) (> d dst))
;                                 (setf curr i dst d))))
;                (values curr dst))
;              (split-diag (path i)
;                "split into two paths by introducing a new edge that divides
;                 path in two loops."
;                (when (= i -1) (error "bad split (-1) at: ~a" path))
;                (when (< (length path) 3) (error "diag: too few elements in ~a" path))
;                (when (>= i (length path)) (error "diag bad ind: ~a ~a" path i))
;                (do-step (weird:reorder path (i) (0 i)))
;                (do-step (weird:reorder path (0) (i nil))))
;              (split-tri (path)
;                "split path into a triangle and a new path."
;                (do-step (weird:reorder path ((1- (length path))) (0 2)))
;                (do-step (subseq path 1)))
;              (do-step (path &aux (n (length path)))
;                "recursively subdived the path into triangles and/or new loops."
;                ; (when (< n 3) (error "do-step: too few elements in ~a" path))
;                (when (< n 3) (return-from do-step nil))
;                (when (< n 4) (push (add-poly! wer (weird:tl path)) res)
;                              (return-from do-step t))

;                ; this is confusing, but i was unable to find a better way. the
;                ; problem is that sometimes cross path detects an intersection,
;                ; but uw-farthest is unable to find a valid candidate. this is
;                ; probably due to precision issues in either cross, uw-farthest,
;                ; or both?
;                (let ((path (ind-rotate path (y-most path))))
;                  (if (not (cross path))
;                      (split-tri path) ; -> no intersections
;                      (let ((uw (uw-farthest path))) ; -> possible intersection
;                        (if (> uw -1)
;                            (split-diag path uw) ; intersection
;                            (progn (warn "possble intersection, unable to detect farthest uw.")
;                                   (split-tri path)))))))) ; unable to detect intersection
;       (do-step path)
;       res)))

