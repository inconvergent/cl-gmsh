(in-package :gmsh)


(doc BISECT/SLICE POLY CONFIGURATION
                         (e c d)   c
                                  /|
                                 / |
                           e ---x--x--- d
                               / \ |
                  (a b e)   b /____| a    (d a e))

; TODO: smarter material handling?
(veq:fvdef* plane-slice! (msh (:va 3 pt norm) &key matfx
                         &aux (g (grph:grph)) (edge-cache (make-ht)) (lerps (make-ht))
                              (new-verts (list)) (new-edge-set (list)))
  (declare #.*opt* (gmsh msh) (veq:ff pt norm) (grph:grph g)
                   (list new-verts) (hash-table edge-cache lerps))
  "slice mesh with plane."
  (auxin:with-struct (gmsh- verts uv vt edges->poly) msh
    (labels ((getlrp (a b &aux (e (-edg a b)))
               (declare (veq:pn a b) (list e))
               (if #5=(= a (the veq:pn (car e)))
                   #4=(the veq:ff (gethash e lerps)) (- 1.0 #4#)))
             (setlerp (a b s &aux (e (-edg a b)))
                (declare (veq:pn a b) (veq:ff s) (list e))
                (setf (gethash e lerps) (if #5# s (- 1.0 s))))
             (nilpos (x y z) (declare (veq:pn x y z)) ; find edge that is not marked :cut
                (let ((np (mapcar (lambda (e) (grph:@prop g e :cut))
                                  `((,x ,y) (,y ,z) (,z ,x)))))
                  (when (= 2 (loop for n in np if n summing (the veq:pn 1)))
                        (position nil np))))
             (@srt-edge-cache (a b p) (declare (keyword p))
                (gethash (cons p (-edg a b)) edge-cache))
             (set-srt-edge-cache (a b p v) (declare (keyword p))
                (setf (gethash (cons p (-edg a b)) edge-cache) v))
             (new-poly (a b c ta tb tc &aux (p (list a b c)))
               (declare (veq:pn a b c) (list p))
               (mvb (p i) (add-poly! msh p)
                 (when p (setf (gethash p vt) (roll-ind (list ta tb tc) i)))
                 p))
             (new-vert (c a lrp &aux (v (@srt-edge-cache c a :v)))
               (declare (veq:pn c a) (veq:ff lrp))
               (or v (set-srt-edge-cache c a :v
                       (veq:xlet ((p!new (add-vert! msh
                                           (veq:f3lerp (veq:f3$ verts c a) lrp))))
                         (push new new-verts) new))))
             (new-tex (c a lrp &aux (v (@srt-edge-cache c a :uv)))
               (declare (veq:pn c a) (veq:ff lrp))
               (or v (set-srt-edge-cache c a :uv
                       (add-uv! msh (veq:f2lerp (veq:f2$ uv c a) lrp)))))
             (new-poly-and-tex (p a b c ta tb tc lca lbc)
               (declare (list p) (veq:pn a b c ta tb tc) (veq:ff lca lbc))
               (veq:xlet ((p!d  (new-vert  c  a lca)) (p!e  (new-vert b   c lbc))
                          (p!td (new-tex  tc ta lca)) (p!te (new-tex  tb tc lbc)))
                 (push (list d e) new-edge-set)
                 (if matfx (progn (f@matfx p #1=(new-poly a b e ta tb te))
                                  (f@matfx p #2=(new-poly e c d te tc td))
                                  (f@matfx p #3=(new-poly d a e td ta te)))
                           (progn #1# #2# #3#)))))
      (grph:modify! (g grp)
        (loop for e being the hash-keys of edges->poly using (hash-value polys)
              for (a b) of-type (veq:pn veq:pn) = e
              ; check if edge is sliced by plane
              do (veq:mvb (isect s) (veq:f3planex norm pt (veq:f3$ verts a b))
                   (declare (boolean isect) (veq:ff s))
                   (when (and isect (< veq:*eps* s 1.0))
                         (setlerp a b s)
                         ; for all polys adjacent to edge:
                         (dsbmapc ((gethash e edges->poly) p*)
                           ; for all edges in adj. polys: add to grph, mark the :cut edges
                           (dsbmapc ((poly-as-edges p*) (pa pb))
                                    (grp-> pa pb (when (compare-norm-edges a b pa pb) :cut))))))))

      (grph:qry g :select (?x ?y ?z)
        :where (and (?x _ ?y) (?y _ ?z) (?z _ ?x)
                    (% (@poly msh (list ?x ?y ?z))))
        :then (veq:xlet ((l!p (list ?x ?y ?z)) (i (nilpos ?x ?y ?z)))
                (if i (progn (del-poly! msh p)
                             (veq:dsb (a b c) (roll-ind p i)
                               (veq:dsb (ta tb tc) (roll-ind (gethash p vt +nilpol+) i)
                                 (veq:xlet ((f!lca (getlrp c a)) (f!lbc (getlrp b c)))
                                   (new-poly-and-tex p a b c ta tb tc lca lbc)))))
                      (wrn :plane-slice-nilpos "inconsistent state for: ~a, ~a" p i))))))
  (values (grph::undup new-verts) new-edge-set))

; TODO: this is incomplete, side-idx does too much
; TODO: handle textures
(veq:fvdef* plane-sym! (msh (:va 3 pt norm)
                       &key matfx &aux (side-idx (make-hash-table :test #'eq)))
  (declare #.*opt* (gmsh msh) (veq:ff pt norm) (hash-table side-idx))
  "cut and make mesh symmetric around plane."
  (labels ((vert-side (verts) (loop for i in verts if (eq :sb (gethash i side-idx))
                                    do (return-from vert-side t)))
           (sym (v) (veq:xlet ((f3!p (@vert msh v))
                               (f3!pp (f3!@- p (f3!@*. norm (veq:f3dot norm (f3!@- p pt))))))
                      (add-vert! msh (f3!@- (f3!@*. pp 2.0) p))))
           (vrt (v &aux (h (gethash v side-idx)))
             (cond ((eq :ign h) v)
                   ((numberp h) h)
                   (t (let ((new (sym v)))
                        (setf (gethash v side-idx) new)
                        new))))
           (sym-poly (poly &aux (vt (gethash poly (gmsh-vt msh)))
                                (poly* (mapcar #'vrt poly))
                                (new (add-poly! msh poly* vt)))
             (unless new (wrn :plen-sym "duplicate poly? ~a" poly*))
             (when matfx (f@matfx poly new))
             new))
    (let ((ign (plane-slice! msh pt norm :matfx matfx)))
      (loop for i across (msk/v msh x (< (veq:f3dot (veq:f3norm (f3!@- x pt)) norm) 0f0))
            for k from 0 ; TODO: double check msk/v
            if (= i 1) do (setf (gethash k side-idx) :sb)
            else do (setf (gethash k side-idx) :sa))
      (loop for i in ign do (setf (gethash i side-idx) :ign))
      (loop for p in (@all-polys msh) ; this is kinda inefficient
            if (vert-side p) do (del-poly! msh p) else do (sym-poly p))
      ign)))

; TODO: make this more generic?
; TODO: ported from weir; untested
(veq:fvdef triangulate-edge-set (msh edge-set pfx)
  (declare (gmsh msh) (list edge-set) (function pfx))
  "triangulate the hull defined by edge set, using the projection provided
by pfx where pfx: R3 -> R2."
  (let ((path (auxin:tv (grph:edge-set->path edge-set)))
        (res (list)))
    (declare (vector path))
    (labels ((2gv (i) (declare (veq:pn i))
               (veq:mvcgrp (3 pfx) (@vert msh i)))
             (ypos (v) (declare (veq:pn v))
               (veq:fsel (:y) (2gv v)))
             (ind-rotate (path ymost) (declare (vector path) (veq:pn ymost))
               (reorder path (ymost nil) (0 ymost)))
             (y-most (path &aux (ypos (ypos (aref path 0))) (cv 0) )
               (loop for v across (subseq path 1) for i from 1
                     if (< (ypos v) ypos) do (setf ypos (ypos v) cv i))
               cv)
             (cross (path &aux (n (length path)))
                "does any segment in path cross the line (aref path 1) (aref path -1)."
                (veq:xlet ((f2!a (2gv (aref path 1)))
                           (f2!b (2gv (auxin:vl path))))
                  (loop for i from 0 below n
                        do ; weird precision issue. override veq eps
                           ; TODO: can we fix this somehow?
                           (let ((veq::*eps* (* 1000f0 veq::*eps*)))
                             (when (veq:f2segx a b (veq:mvcgrp (3 pfx)
                                                     ($verts msh (aref path i)
                                                       (aref path (mod (1+ i) n)))))
                               (return-from cross t)))))
                nil)
             (uw-farthest (path &aux (n (length path)) dst (curr -1))
              "find the vertex in triangle ((aref path 1) (aref path 0) (aref path -1))
               that is the farthest away from (aref path 1) (aref path -1)."
               (loop for i from 2 below (1- n)
                     if ; TODO: this feels very sketchy
                       (let ((veq::*eps* (* 1000f0 veq::*eps*)))
                          (veq:f2in-triangle
                            (veq:mvcgrp (3 pfx)
                              ($verts msh (aref path 0) (aref path 1)
                               (auxin:vl path) (aref path i)))))

                     do (let ((d (veq:f2segdst
                                   (veq:mvcgrp (3 pfx)
                                     ($verts msh (aref path 1) (auxin:vl path)
                                                  (aref path i))))))
                          (when (or (= curr -1) (> d dst))
                                (setf curr i dst d))))
               (values curr dst))
             (split-diag (path i)
               "split into two paths by introducing a new edge that divides
                path in two loops."
               (when (= i -1) (error "bad split (-1) at: ~a" path))
               (when (< (length path) 3) (error "diag: too few elements in ~a" path))
               (when (>= i (length path)) (error "diag bad ind: ~a ~a" path i))
               (do-step (reorder path (i) (0 i)))
               (do-step (reorder path (0) (i nil))))
             (split-tri (path)
               "split path into a triangle and a new path."
               (do-step (reorder path ((1- (length path))) (0 2)))
               (do-step (subseq path 1)))
             (do-step (path &aux (n (length path)))
               "recursively subdived the path into triangles and/or new loops."
               ; (when (< n 3) (error "do-step: too few elements in ~a" path))
               (when (< n 3) (return-from do-step nil))
               (when (< n 4) (push (add-poly! msh (auxin:tl path)) res)
                             (return-from do-step t))

               ; this is confusing, but i was unable to find a better way. the
               ; problem is that sometimes cross path detects an intersection,
               ; but uw-farthest is unable to find a valid candidate. this is
               ; probably due to precision issues in either cross, uw-farthest,
               ; or both?
               (let ((path (ind-rotate path (y-most path))))
                 (if (not (cross path))
                     (split-tri path) ; -> no intersections
                     (let ((uw (uw-farthest path))) ; -> possible intersection
                       (if (> uw -1)
                           (split-diag path uw) ; intersection
                           (progn (wrn :triangulate "possble intersection, unable to detect farthest uw.")
                                  (split-tri path)))))))) ; unable to detect intersection
      (do-step path)
      res)))

