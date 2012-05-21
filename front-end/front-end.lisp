(defpackage "BSP.FRONT"
  (:use "CL")
  (:import-from "BSP.COMPILER" "ELTYPE-OF" "DATA-OF" "SET-REDUCER-VALUE")
  (:export "ELTYPE-OF" "DATA-OF"
           "MAKE-VECTOR" "MAKE-CONSTANT" "MAKE-OP"
           "REDUCER" "MAKE-REDUCER" "SET-REDUCER-VALUE"))

(in-package "BSP.FRONT")
(defclass bsp:vector (bsp.compiler:vec)
  ((data    :initarg :data    :initform nil :accessor data-of)
   (mask    :initarg :mask    :initform nil :reader mask-of :type (or null cons bsp:vector))
   (mask-stack :initarg :mask-stack :initform nil :reader mask-stack-of)))

(defun has-root (stack root)
  (or (null root)
      (loop for substack on stack by #'cdr
            thereis (eq substack root))))

;; Roots: dx list of mutable cells to potentially-accessible
;; vector or reduce values
(defvar *roots* '())
(defmacro bsp:let ((&rest args) &body body)
  (unless args
    (return-from bsp:let `(locally ,@body)))
  (let* ((args (mapcar (lambda (x)
                         (if (consp x)
                             x
                             (list x nil)))
                       args))
         (gensyms (mapcar (lambda (arg)
                            (gensym (string (first arg))))
                          args)))
    `(let* (,@(loop for (nil value) in args
                    for prev in (cons '*roots* gensyms)
                    for gensym in gensyms
                    collect `(,gensym (cons ,value ,prev)))
            (*roots* ,(car (last gensyms))))
       (symbol-macrolet ,(loop for (var) in args
                               for gensym in gensyms
                               collect `(,var (car ,gensym)))
         ,@body))))

(defvar *default-chunk-size* 1024)

(defclass bsp-context ()
  ((count  :initarg :count :reader count-of)
   (chunk  :initarg :chunk :initform *default-chunk-size* :reader chunk-of)
   (ops    :initform (make-array 32 :adjustable t :fill-pointer 0)
           :accessor ops-of)
   (intern-table :initform (make-hash-table) :reader intern-table-of)))

(declaim (type bsp-context *context*))
(defvar *context*)
(declaim (type list *mask-stack*))
(defvar *mask-stack* '())

(define-symbol-macro bsp:n (count-of *context*))

(defun make-constant (eltype value)
  (let ((table (intern-table-of *context*))
        (key   (cons eltype value)))
    (or (gethash key table)
        (setf (gethash key table)
              (make-instance 'bsp:vector
                             :eltype eltype
                             :initial-element value)))))

(declaim (inline countify))
(defun countify (x)
  (etypecase x
    (sequence (length x))
    (unsigned-byte x)))

(defmacro bsp:with-context ((count &optional (chunk-size '*default-chunk-size*))
                            &body body)
  (let ((_count (gensym "COUNT")))
    `(let* ((*context* (let ((,_count (countify ,count)))
                         (make-instance 'bsp-context
                                        :count ,_count
                                        :chunk (min ,_count ,chunk-size))))
            (*mask-stack* (list (make-constant bsp:bool
                                               (ldb (byte 32 0) -1)))))
       ,@body)))

(defun make-vector (source)
  (declare (type (simple-array * 1) source))
  (assert (>= (length source) (count-of *context*)))
  (let ((table (intern-table-of *context*)))
    (or (gethash source table)
        (setf (gethash source table)
              (make-instance 'bsp:vector
                             :eltype  (let ((type (array-element-type source)))
                                        (cond ((equal type 'double-float)
                                               bsp:double)
                                              ((equal type '(unsigned-byte 32))
                                               bsp:u32)
                                              (t
                                               (error "Can't construct vector for eltype ~S" type))))
                             :data    source)))))

(defun %make-op (eltype fun &rest args)
  (let* ((dst (make-instance 'bsp:vector
                             :eltype eltype
                             :mask   (car *mask-stack*)
                             :mask-stack *mask-stack*))
         (mask (car *mask-stack*))
         (flipped (consp mask))
         (mask (if flipped (second mask) mask))
         (op   (bsp.compiler:make-op fun
                                     dst
                                     mask
                                     (concatenate 'simple-vector
                                                  (vector mask flipped dst)
                                                  args))))
    (vector-push-extend op (ops-of *context*))
    dst))

(defun make-op (eltype fun &rest args)
  (assert (every (lambda (arg)
                   (or (not (typep arg 'bsp:vector))
                       (has-root *mask-stack* (mask-stack-of arg))))
                 args))
  (apply '%make-op eltype fun args))

(defun vmerge (condition x y)
  (assert (equal (eltype-of condition) bsp:bool))
  (let ((type (eltype-of x)))
    (assert (equal type (eltype-of y)))
    (ecase type
      ((bsp:bool bsp:u32)
       (%make-op type 'bsp.vm-op:merge-unsigned
                 condition x y))
      (bsp:double
       (%make-op type 'bsp.vm-op:merge-double
                 condition x y)))))

(defun vcomplement (condition)
  (assert (equal (eltype-of condition) bsp:bool))
  (make-op bsp:bool
           'bsp.vm-op:complement-mask
           condition))

(defun call-with-mask (condition then-thunk else-thunk)
  (unless (typep condition 'bsp:vector)
    (return-from call-with-mask (funcall (if condition then-thunk else-thunk))))
  (assert (equal (eltype-of condition) bsp:bool))
  (assert (has-root *mask-stack* (mask-stack-of condition)))
  (unless (eql *mask-stack* (mask-stack-of condition))
    (setf condition (make-op bsp:bool
                             'bsp.vm-op:canonicalise-mask condition)))
  (let ((then
          (let ((*mask-stack* (cons condition *mask-stack*)))
            (multiple-value-list (funcall then-thunk))))
        (else
          (let ((*mask-stack* (cons (if (null *mask-stack*)
                                        `(not ,condition)
                                        (vcomplement condition))
                                    *mask-stack*)))
            (multiple-value-list (funcall else-thunk)))))
    (values-list (mapcar (lambda (then else)
                           (vmerge condition
                                   (%vectorify then)
                                   (%vectorify else)))
                         then else))))

(defun %vectorify (x)
  (etypecase x
    (boolean (make-constant bsp:bool (if x (ldb (byte 32 0) -1) 0)))
    (bsp.compiler:vec x)
    (double-float (make-constant bsp:double x))
    ((unsigned-byte 32) (make-constant bsp:u32 x))
    ((simple-array * 1) (make-vector x))))

(defmacro vectorify (place &optional (really-p t))
  `(when ,really-p
     (setf ,place (%vectorify ,place))))

(defmacro bsp:if (condition then &optional else)
  `(call-with-mask (%vectorify ,condition)
                   (lambda () ,then)
                   (lambda () ,else)))

(defun %barrier (context new-roots reducers)
  (let* ((roots  (append new-roots *roots*))
         (ops    (ops-of context))
         (bblock (bsp.compiler:to-bblock
                  (count-of context) (chunk-of context)
                  (coerce roots 'simple-vector)
                  (coerce ops 'simple-vector)
                  (coerce reducers 'simple-vector))))
    (clrhash (intern-table-of context))
    (setf (ops-of context)
          (make-array (* 2 (length ops))
                      :adjustable t :fill-pointer 0))
    (values (bsp.vm:execute-bblock bblock)
            (coerce new-roots 'simple-vector))))

(defun bsp:barrier ()
  (%barrier *context* '() #())
  (values))

(defmethod bsp:value ((vector bsp:vector))
  (or (data-of vector)
      (progn
        (%barrier *context* (list vector) #())
        (data-of vector))))
