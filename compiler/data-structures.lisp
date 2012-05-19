(in-package "BSP.COMPILER")
(deftype index ()
  `(mod ,most-positive-fixnum))

(declaim (inline ensure-function))
(defun ensure-function (x)
  (if (functionp x) x (fdefinition x)))

(defun vectorify (x)
  (if (typep x 'sequence)
      (coerce x 'simple-vector)
      (vector x)))

(defclass vec ()
  ((eltype  :initarg :eltype  :reader eltype-of)
   (initial-element :initarg :initial-element :reader initial-element-of
                    :initform nil)))

(defgeneric data-of (vec))
(defgeneric (setf data-of) (value vec))

;; CLOS classes?
(defclass op ()
  ((dsts  :initarg :dsts  :reader op-dsts)
   (masks :initarg :masks :reader op-masks)
   (fun   :initarg :fun   :reader op-fun)
   (args  :initarg :args  :reader op-args)))

(defun op-p (x)
  (typep x 'op))

(defun make-op-with-class (class fun dsts masks args &rest rest)
  (apply 'make-instance class
         :fun   (ensure-function fun)
         :dsts  (vectorify dsts)
         :masks (vectorify masks)
         :args  (vectorify args)
         rest))

(defun make-op (fun dsts masks args)
  (make-op-with-class 'op fun dsts masks args))

(defclass reducer (op)
  ((2arg-reducer :initarg :2arg-reducer :reader reducer-2arg-reducer)
   (1arg-reducer :initarg :1arg-reducer :reader reducer-1arg-reducer)))

(defun reducer-p (x)
  (typep x 'reducer))

(defun make-reducer-with-class (class fun dsts masks args 2arg-reducer 1arg-reducer
                                &rest rest)
  (apply 'make-op-with-class class fun dsts masks args
         :2arg-reducer (ensure-function 2arg-reducer)
         :1arg-reducer (ensure-function 1arg-reducer)
         rest))

(defun make-reducer (fun dsts masks args 2arg-reducer 1arg-reducer)
  (make-reducer-with-class 'reducer fun dsts masks args 2arg-reducer 1arg-reducer))

(defgeneric set-reducer-value (reducer value))

(defstruct (state)
  ;; vector -> op that writes to it
  (var-op    (make-hash-table) :type hash-table :read-only t)
  (live-ops  (make-hash-table) :type hash-table :read-only t)
  ;; set of vectors that are read from
  (live-vars (make-hash-table) :type hash-table :read-only t)
  ;; vectors used as mask
  (mask-vars (make-hash-table) :type hash-table :read-only t)
  ;; returned as result
  (roots     (make-hash-table) :type hash-table :read-only t))

(declaim (type state *state*))
(defvar *state*)

(defun var-op (var)
  (declare (type vec var))
  (gethash var (state-var-op *state*)))

(defun (setf var-op) (op var)
  (declare (type op  op)
           (type vec var))
  (assert (not (var-op var)))
  (setf (gethash var (state-var-op *state*)) op))

(defun livep (var)
  (declare (type (or vec op) var))
  (etypecase var
    (vec (gethash var (state-live-vars *state*)))
    (op  (gethash var (state-live-ops  *state*)))))

(defun set-live (var)
  (declare (type (or vec op) var))
  (etypecase var
    (vec (setf (gethash var (state-live-vars *state*)) t))
    (op  (setf (gethash var (state-live-ops  *state*)) t))))

(defun maskp (var)
  (declare (type vec var))
  (gethash var (state-mask-vars *state*)))

(defun set-mask (var)
  (declare (type vec var))
  (setf (gethash var (state-mask-vars *state*)) t))

(defun rootp (var)
  (declare (type vec var))
  (gethash var (state-roots *state*)))

(defun set-root (var)
  (declare (type vec var))
  (setf (gethash var (state-roots *state*)) t))
