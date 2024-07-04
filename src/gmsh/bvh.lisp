(in-package :gmsh/bvh)

; used in bvh-raycast
(defun int/bbox-leap ()   (floor (- (log +int-mima-leap+ 2) (log +int-leap+))))
(defun simd4/bbox-leap () (floor (log +simd-mima-leap+ 2)))

(defmacro -eps-div (&rest rest)
  `(values ,@(loop for x in rest collect
               `(if (> (the veq:ff (abs (the veq:ff ,x))) #.*eps*)
                    (the veq:ff (/ ,x)) #.*eps*))))

(veq:fvdef default-mat (n &aux  ; :c :x or 0 0
                               (res (veq:$make :n n :dim 2 :v :c :type keyword)))
  "make n default materials"
  (loop for i from 1 below (* 2 n) by 2 do (setf (aref res i) :x)) res)

; objs list: ((2 3 4) #(xmi xma ...) #normal)
(veq:fvdef make (input-objs vfx &key (num 5)  (mode :bvh4-simd) matfx
                                     (sort-num num) (num-buckets 13) (sort-lvl 100))
  (declare #.*opt* (list input-objs) (function vfx) (symbol mode)
                   (veq:pn num sort-lvl sort-num num-buckets))
  "build bvh from these objects"
  (macrolet (($ (ni field)
              `(,(gmsh::symb (string-upcase (gmsh::mkstr "bvh-node-" field)))
                 (aref nodes ,ni))))
    (let* ((t0 (auxin:now))
           (npolys (+ 10 (length input-objs)))
           (all-objs (-make-objects-and-normals vfx input-objs))
           ; TODO: WTF is this number, and why is it multiplied by 3?
           (max-node-num (* 3 (expt 2 (ceiling (log npolys 2)))))
           (polyind 1) (ni 0)
           (nodes (init-bvh-node-vec max-node-num))
           (polys (veq:p4$zero npolys))
           (mima (veq:f$zero (* +int-mima-leap+ max-node-num)))
           (simd-mima (veq:f$zero (* 6 max-node-num)))
           (mat (default-mat npolys))
           (polyfx (veq:f3$zero (* 3 npolys)))
           (normals (veq:f3$zero npolys))
           (numlvls 0)
           (int-nodes (veq:p4$zero 0)) ; TODO: make this with leap 2, not 3
           (simd-nodes (veq:p4$zero 0))) ; TODO: make this pvec
      (declare (veq:pn ni npolys max-node-num polyind) (veq:pvec polys int-nodes)
               (bvh-node-vec nodes) (veq:kvec mat) (veq:fvec polyfx normals))
      (labels
        ((nxt-index () (incf ni) (1- ni))
         (do-poly (o &aux (p (car o)))
           (setf (veq:3$ polys polyind) (veq:from-lst p)
                 (veq:3$ normals polyind) (veq:f3$ (third o)))
           (veq:$nvset (polyfx #.+polyleap+ (* #.+polyleap+ polyind))
                       (m@make-polyx (veq:f3$ (f@vfx p) 0 1 2)))
           (when matfx (veq:$nvset (mat 2 (* 2 polyind)) (f@matfx p)))
           (incf polyind))
         (set-leaf-node (n ni objs)
           (setf ($ ni :num) n ($ ni :ref) (* #.+polyleap+ polyind))
           (loop for o in objs do (do-poly o)))

         (split-axis (objs n lvl) (declare (list objs) (veq:pn n lvl))
           (handler-case
             (progn (when (> lvl sort-lvl) (error "deep batch ~a" lvl))
                    (when (< n sort-num) (error "small batch ~a" n))
                    (sah-split-by-best-axis objs num-buckets))
             (simple-error (e) (declare (ignorable e))
               (veq:xlet ((ax (-longaxis objs))
                          (objs* (-axissort objs :ax ax))
                          (p!mid (if (= n 1) 0 (round n 2))))
                 (values (subseq objs* 0 mid)
                         (subseq objs* mid)
                         ax)))))

         (build (ni objs &optional (lvl 0) &aux (n (length objs)))
           (declare (veq:pn ni n) (list objs))
           (setf numlvls (max numlvls lvl))
           (when (< (length objs) 1) (wrn :make "empty node"))
           (veq:$nvset (($ ni :bbox) 6) (-objs-list-bbox objs))
           (if (<= n num)
               (set-leaf-node n ni objs)
               (veq:mvb (ll rr ax) (split-axis objs n lvl)
                 (veq:xlet ((p!l (nxt-index)) (p!r (nxt-index)))
                   (setf ($ ni :ref) l ($ ni :ax) ax)
                   (build l ll (1+ lvl))
                   (build r rr (1+ lvl)))))))

        (build (nxt-index) all-objs)

        (ecase mode
          ((:bvh2-int :bvh2) (veq:mvb (int-nodes* mima*) (int/build-int-nodes nodes ni
                                                            :leap +int-mima-leap+)
                              (setf int-nodes int-nodes* mima mima*)))
          ((:bvh2-stackless) (veq:mvb (int-nodes* mima*) (int/build-int-nodes nodes ni
                                                              :leap +int-mima-leap+)
                               (setf int-nodes (int/make-stackless! int-nodes*)
                                     mima mima*)))
          ((:bvh4-simd) (veq:mvb (int-nodes* mima*) (int/build-int-nodes nodes ni
                                                        :leap +int-mima-leap+)
                          (setf int-nodes (int/make-stackless! int-nodes*)
                                mima mima*))
                        (veq:mvb (simd-nodes* simd-mima*) (simd/build-int-nodes int-nodes mima)
                          (setf simd-nodes simd-nodes* simd-mima simd-mima*)
                          ; (simd/compress-int-nodes simd-nodes* simd-mima*)
                          )))

        (-make-bvh :nodes nodes :polys polys :lvl numlvls :polyfx polyfx
                   :normals normals :mat mat :num-polys (1- polyind) :num-nodes ni
                   :simd-nodes simd-nodes :int-nodes int-nodes
                   :mima mima :simd-mima simd-mima
                   :num-leaves num :time (auxin:now t0) :mode mode)))))

(declaim (inline get-norm get-poly get-mat))
(veq:fvdef get-norm (bvh i) (declare #.*opt* (bvh bvh) (veq:in i))
  "get this normal from bvh" (veq:f3$ (bvh-normals bvh) i))

(veq:fvdef get-poly (bvh i) (declare #.*opt* (bvh bvh) (veq:in i))
  "get this poly from bvh." (veq:3$ (bvh-polys bvh) i))

(veq:fvdef get-mat (bvh i) (declare #.*opt* (bvh bvh) (veq:in i))
  "get this material from bvh." (veq:2$ (bvh-mat bvh) i))

