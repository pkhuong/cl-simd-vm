(defpackage "BSP.FRONT"
  (:use "CL")
  (:export "BSP-VECTOR" "GLOBALP" "ELTYPE-OF" "DATA-OF" "OP-OF" "THUNKS-OF" "MASK-OF" "MASK-STACK-OF"
           "HAS-ROOT"
           ))

(in-package "BSP.FRONT")

;; We currently assume a 1-1 relationship between operations and
;; destination vectors.  However, it shouldn't be too hard to break
;; this up and instead have a 1:n: only the front-end and compiler
;; assume that kind of structure.  The VM is oblivious to in/out
;; attributes, and would only need to be slightly modified to also
;; represent whether vectors are ever used (in the summary vector).

(defclass bsp-vector ()
  ((globalp :initarg :globalp :initform nil :accessor globalp)
   (eltype  :initarg :eltype  :reader eltype-of)
   (data    :initarg :data    :initform nil :accessor data-of)
   ;; op: vectors summaries size mask flip-p for-mask-p dst will be spliced in.
   (op      :initarg :op      :initform nil :reader op-of :type (or null simple-vector))
   (mask    :initarg :mask    :initform nil :reader mask-of :type (or null cons bsp-vector))
   (mask-stack :initarg :mask-stack :initform nil :reader mask-stack-of)))

(defun has-root (stack root)
  (or (null root)
      (loop for substack on stack by #'cdr
            thereis (eq substack root))))

(defvar *roots* '())
(defmacro vlet ((&rest args) &body body)
  (unless args
    (return-from vlet `(locally ,@body)))
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

(defvar *default-chunk-size* 128)

(defclass bsp-context ()
  ((count  :initarg :count :reader count-of)
   (chunk  :initarg :chunk :initform *default-chunk-size* :reader chunk-of)
   (thunks :initform (make-array 32 :adjustable t :fill-pointer 0)
           :accessor thunks-of)))

(defvar *context*)
(defmacro with-bsp-context ((count &optional (chunk-size '*default-chunk-size*))
                            &body body)
  `(let ((*context* (make-instance 'bsp-context
                                   :count ,count
                                   :chunk ,chunk-size)))
     ,@body))

(defvar *mask-stack* '())

(defun make-vector (source)
  (declare (type (simple-array * 1) source))
  (assert (>= (length source) (count-of *context*)))
  (make-instance 'bsp-vector
                 :globalp t
                 :eltype  (array-element-type source)
                 :data    source))

(defun make-thunk (eltype &rest op)
  (let ((instance
          (make-instance 'bsp-vector
                         :eltype eltype
                         :op     (coerce op 'simple-vector)
                         :mask   (car *mask-stack*)
                         :mask-stack *mask-stack*)))
    (vector-push-extend instance (thunks-of *context*))
    instance))

(defun vmerge (condition x y)
  (assert (equal (eltype-of x) (eltype-of y)))
  (assert (equal (eltype-of condition) '(unsigned-byte 32)))
  (let ((type (eltype-of x)))
    (cond ((equal type '(unsigned-byte 32))
           (make-thunk type 'bsp.vm-op:merge-unsigned
                       condition x y))
          ((eql   type 'double-float)
           (make-thunk type 'bsp.vm-op:merge-double
                       condition x y))
          (t (error "Don't know how to ~S vectors of ~S" 'vmerge type)))))

(defun vcomplement (condition)
  (assert (equal (eltype-of condition) '(unsigned-byte 32)))
  (make-thunk '(unsigned-byte 32)
              'bsp.vm-op:complement-mask
              condition))

(defun call-with-mask (condition then-thunk else-thunk)
  (assert (equal (eltype-of condition) '(unsigned-byte 32)))
  (unless (typep condition 'bsp-vector)
    (return-from call-with-mask (funcall (if condition then-thunk else-thunk))))
  (assert (has-root *mask-stack* (mask-stack-of condition)))
  (unless (eql *mask-stack* (mask-stack-of condition))
    (setf condition (make-thunk '(unsigned-byte 32)
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
                           (vmerge condition then else))
                         then else))))

(defmacro vif (condition then &optional else)
  `(call-with-mask ,condition
                   (lambda () ,then)
                   (lambda () ,else)))

(defun %barrier (context new-roots reducers)
  (let* ((roots  (append new-roots *roots*))
         (thunks (thunks-of context))
         (bblock (bsp.compiler:to-bblock
                  (count-of context) (chunk-of context)
                  (coerce roots 'simple-vector)
                  (coerce reducers 'simple-vector)
                  (coerce thunks 'simple-vector))))
    (break "bblock ~A~%" bblock)
    (setf (thunks-of context)
          (make-array (* 2 (length thunks))
                      :adjustable t :fill-pointer 0))
    (values (bsp.vm:execute-bblock bblock)
            (coerce new-roots 'simple-vector))))

(macrolet ((def (name (&optional binary-double unary-double)
                      (&optional binary-unsigned unary-unsigned))
             `(defun ,name (x ,@(and (or unary-double unary-unsigned) '(&optional)) y)
                (declare (type bsp-vector x)
                         (type (or ,@(and (or unary-double unary-unsigned) '(null)) bsp-vector)
                               y))
                (when (and x y)
                  (assert (equal (eltype-of x) (eltype-of y))))
                (let ((eltype (eltype-of x)))
                  (if y
                      (cond ,@(and binary-double
                                   `(((equal eltype 'double-float)
                                      (make-thunk eltype ',binary-double x y))))
                            ,@(and binary-unsigned
                                   `(((equal eltype '(unsigned-byte 32))
                                      (make-thunk eltype ',binary-unsigned x y))))
                            (t (error "Don't know how to ~S vectors of ~S" ',name eltype)))
                      (cond ,@(and unary-double
                                   `(((equal eltype 'double-float)
                                      ,(if (eql unary-double 'identity)
                                           'x
                                           `(make-thunk eltype ',unary-double x)))))
                            ,@(and unary-unsigned
                                   `(((equal eltype '(unsigned-byte 32))
                                      ,(if (eql unary-unsigned 'identity)
                                           'x
                                           `(make-thunk eltype ',unary-unsigned x)))))
                            (t (error "Don't know how to ~S a vector of ~S" ',name eltype))))))))
  (def v+ (bsp.vm-op:double+ identity)             (bsp.vm-op:unsigned+ identity))
  (def v- (bsp.vm-op:double- bsp.vm-op:double-neg) (bsp.vm-op:unsigned- bsp.vm-op:unsigned-neg))
  (def v* (bsp.vm-op:double* identity)             (bsp.vm-op:unsigned* identity))
  (def v/ (bsp.vm-op:double/ bsp.vm-op:double-inv) (bsp.vm-op:unsigned/))
  (def v% () (bsp.vm-op:unsigned%)))

(defun v~ (x)
  (declare (type bsp-vector x))
  (assert (equal (eltype-of x) '(unsigned-byte 32)))
  (make-thunk '(unsigned-byte 32) 'bsp.vm-op:unsigned-complement x))

(macrolet ((def (name unsigned-op &optional double-op)
             `(defun ,name (x y)
                (declare (type bsp-vector x y))
                (assert (equal (eltype-of x) (eltype-of y)))
                (let ((eltype (eltype-of x)))
                  (cond ,@(and unsigned-op
                               `(((equal eltype '(unsigned-byte 32))
                                  (make-thunk '(unsigned-byte 32) ',unsigned-op x y))))
                        ,@(and double-op
                               `(((equal eltype 'double-float)
                                  (make-thunk '(unsigned-byte 32) ',double-op x y))))
                        (t
                         (error "Don't know how to ~S vectors of ~S" ',name eltype)))))))
  (def v=  bsp.vm-op:unsigned=  bsp.vm-op:double=)
  (def v/= bsp.vm-op:unsigned/= bsp.vm-op:double/=)
  (def v<  bsp.vm-op:unsigned<  bsp.vm-op:double<)
  (def v<= bsp.vm-op:unsigned<= bsp.vm-op:double<=)
  (def v>  bsp.vm-op:unsigned<  bsp.vm-op:double>)
  (def v>= bsp.vm-op:unsigned>= bsp.vm-op:double>=)
  (def vand bsp.vm-op:unsigned-and)
  (def vor  bsp.vm-op:unsigned-or)
  (def vxor bsp.vm-op:unsigned-xor))

(defparameter *a* (make-array 1000
                              :element-type 'double-float
                              :initial-element 10d0))

(defparameter *b* (make-array 1000
                              :element-type 'double-float
                              :initial-element 3d0))

(defparameter *c* (map-into (make-array 1000
                                        :element-type '(unsigned-byte 32))
                            (lambda ()
                              (ldb (byte 32 0) (- (random 2))))))

(defparameter *d* (map-into (make-array 1000
                                        :element-type '(unsigned-byte 32))
                            (lambda ()
                              (ldb (byte 32 0) (- (random 2))))))
