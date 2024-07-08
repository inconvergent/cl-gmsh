(in-package #:gmsh)

(defun v? (&optional (silent t)
           &aux (v (slot-value (asdf:find-system 'gmsh) 'asdf:version)))
  (unless silent (format t "~&GMSH version: ~a~&." v))
  v)
(defun d? (f) (describe f))
(defun i? (f) (inspect f))

(defmacro doc (&rest body) (declare (ignore body)) "ignores body. returns nil." nil)
(defmacro abbrev (short long) `(defmacro ,short (&rest args) `(,',long ,@args)))

(defun internal-path-string (path &optional (pkg :gmsh)) (declare (string path))
  (namestring (asdf:system-relative-pathname pkg path)))

(defun mkstr (&rest args) (with-output-to-string (s) (dolist (a args) (princ a s))))
(defun reread (&rest args) (values (read-from-string (apply #'mkstr args))))
(defun symb (&rest args) (values (intern (apply #'mkstr args))))
(defun mapqt (l) (mapcar (lambda (s) `(quote ,s)) l))

(abbrev mvc multiple-value-call) (abbrev mvb multiple-value-bind)
(abbrev dsb destructuring-bind) (abbrev awg veq::with-gensyms)
(abbrev awf veq::flatten)

(declaim (inline find-min-ind roll-ind))
(defun find-min-ind (p) (declare (optimize speed (safety 1)) (list p))
  (loop with i of-type fixnum = 0 with v of-type fixnum = (car p)
        for c of-type fixnum in (cdr p) for k of-type fixnum from 1
        if (< c v) do (setf i k v c)
        finally (return-from find-min-ind i)))
(defun roll-ind (l i) (declare (optimize speed (safety 1)) (list l) (fixnum i))
  (concatenate 'list (subseq l i) (subseq l 0 i)))

(defun undup (e &optional (flatten t)) (declare (optimize speed))
  (remove-duplicates (if flatten (awf e) e)))

(defun at-most (n &rest rest) (declare (veq:pn n))
  (<= (length (remove-if-not #'identity rest)) n))

(defun split-string (x s &key prune) (declare (character x) (string s) (boolean prune))
  (labels ((splt (s) (loop for c across s for i from 0
                           if (equal c x)
                           do (return-from splt
                                (cons (subseq s 0 i) (splt (subseq s (1+ i))))))))
    (let ((res (splt (concatenate 'string s (string x)))))
      (if prune (remove-if (lambda (s) (= 0 (length s))) res)
                res))))

(defun group (source n) (declare (veq:pn n))
  (labels ((rec (source acc &aux (rest (nthcdr n source)))
             (if (consp rest) (rec rest (cons (subseq source 0 n) acc))
                              (nreverse (cons source acc)))))
    (when source (rec source nil))))
(defun ungroup (source &aux (res (list)))
  (loop for s in source do (loop for k in s do (push k res)))
  (reverse res))

(defun last* (l) (declare (list l)) (first (last l)))
(defun close-path (l) (declare (list l)) (cons (last* l) l))
(defun ensure-list (l)
  (the list (typecase l (null nil) (list l) (t (list l)))))
(defun to-list (a) (declare (sequence a)) (coerce a 'list))
(defun vector-last (a) (declare (vector a)) (aref a (1- (length a))))
(defun vector-first (a) (declare (vector a)) (aref a 0))
(defun to-vector (init) (declare (list init))
  (make-array (length init)
    :initial-contents init :adjustable nil :element-type 'list))

; (deftype stack-ind () `(integer 0 #.(expt 2 7)))
; (deftype stack () `(simple-array veq:pn))

; (defmacro make-fast-stack (&key (n #.(expt 2 7)) (safe-z 8) (v 0))
;   `(make-array ,(+ n (* 2 safe-z))
;      :element-type 'veq:pn :initial-element ,v))

; (defmacro with-fast-stack ((sym &key (n #.(expt 2 7)) (v 0) (safe-z 8) stack)
;                                 &rest body)
;   (declare (symbol sym stack) (veq:pn n safe-z))
;   (labels ((sym (s) (auxin:psymb (symbol-package sym) s sym)))
;     (auxin:awg (mem ind)
;       `(let ((,mem ,(if stack stack `(make-fast-stack
;                                        :n ,n :safe-z ,safe-z :v ,v)))
;              (,ind ,safe-z))
;          (declare (stack ,mem) (stack-ind ,ind))
;          (macrolet
;            ((,(sym 'push-) (val) ; add this element to the stack
;              `(progn (setf (aref ,',mem ,',ind) ,val)
;                      (setf ,',ind (the stack-ind (1+ ,',ind)))))
;            (,(sym 'pop-) () ; pop the next element
;              `(progn (setf ,',ind (the stack-ind (1- ,',ind)))
;                      (aref ,',mem ,',ind)))
;            (,(sym 'ind-) () ',ind) ; current stack index
;            (,(sym 'peek-) () `(aref ,',mem (1- ,',ind))) ; next element in stack
;            (,(sym 'nil-) () ; reset stack index to safe-z (zero)
;              `(progn (setf ,',ind (the stack-ind ,,safe-z))))
;            (,(sym 'con-) () `(< (the stack-ind ,,safe-z) ,',ind)) ; is the stack empty?
;            (,(sym 'stack-) () ; check state
;              `(progn
;                 (when (< ,',ind (the stack-ind ,,safe-z))
;                   (error "stack underflow in: ~a. ind: ~a" ',',sym ,',ind))
;                 (when (< ,,(the stack-ind (- n safe-z)) ,',ind)
;                   (error "stack overflow in: ~a. ind: ~a" ',',sym ,',ind)))))
;            ,@body)))))

