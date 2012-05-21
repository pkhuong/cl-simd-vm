(in-package "BSP.COMPILER")
(defstruct codegen-state
  (registers (make-array 32 :fill-pointer 0 :adjustable t)
             :read-only t)
  (ops       (make-array 32 :fill-pointer 0 :adjustable t)
             :read-only t)
  (vec-register (make-hash-table)
                :read-only t)
  (reduces   (make-array 32 :fill-pointer 0 :adjustable t)
             :read-only t)
  (summarised (make-hash-table)
              :read-only t))

(declaim (type codegen-state *codegen-state*))
(defvar *codegen-state*)

(defun var-reg (var &optional creationp)
  (declare (type vec var))
  (cond ((gethash var (codegen-state-vec-register
                       *codegen-state*)))
        (creationp
         (let ((register
                 (bsp.vm:make-var
                  (length (codegen-state-registers
                           *codegen-state*))
                  (and (or (rootp var)
                           (and (not (initial-element-of var))
                                (not (var-op var))
                                (data-of var)))
                       (lambda (&optional value)
                         (if value
                             (setf (data-of var) value)
                             (data-of var))))
                  (let ((writer (var-op var)))
                    (or (null writer)
                        (not (livep writer))))
                  (livep var)
                  (maskp var)
                  (eltype-of var)
                  (initial-element-of var))))
           (vector-push-extend register
                               (codegen-state-registers
                                *codegen-state*))
           (setf (gethash var (codegen-state-vec-register
                               *codegen-state*))
                 register)))
        (t
         (error "No known register for var ~S" var))))

(defun ensure-summarised (var)
  (when (or (gethash var (codegen-state-summarised *codegen-state*)))
    (return-from ensure-summarised))
  (setf (gethash var (codegen-state-summarised *codegen-state*))
        t)
  (unless (or (var-op var)
              (and (null (data-of var))
                   (initial-element-of var)))
    (vector-push-extend (bsp.vm:make-op #'bsp.vm-op:summarise
                                        (vector
                                         (bsp.vm:var-loc
                                          (var-reg var
                                                   t))))
                        (codegen-state-ops *codegen-state*))))

(defun emit-one-op (op)
  (declare (type op op))
  (map nil (lambda (var)
             (var-reg var t))
       (op-dsts op))
  (map nil #'ensure-summarised (op-masks op))
  (let ((args (map 'simple-vector
                   (lambda (x)
                     (if (typep x 'vec)
                         (bsp.vm:var-loc
                          (var-reg x (or (initial-element-of x)
                                         (data-of x))))
                         x))
                   (op-args op))))
    (vector-push-extend (bsp.vm:make-op (op-fun op) args)
                        (codegen-state-ops *codegen-state*))
    (when (typep op 'reducer)
      (vector-push-extend (bsp.vm:make-reduce-step
                           (reducer-2arg-reducer op)
                           (reducer-1arg-reducer op)
                           args
                           (lambda (value)
                             (set-reducer-value op value)))
                          (codegen-state-reduces *codegen-state*)))))

(defun emit-code (ops)
  (let ((*codegen-state* (make-codegen-state)))
    (map nil #'emit-one-op ops)
    *codegen-state*))
