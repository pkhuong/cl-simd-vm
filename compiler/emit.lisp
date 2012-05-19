(in-package "BSP.COMPILER")
(defstruct register
  loc
  globalp
  read-onlyp
  livep
  maskp
  eltype
  initial-element)

(defstruct codegen-state
  (registers
   (make-array 32 :fill-pointer 0 :adjustable t)
   :read-only t)
  (ops
   (make-array 32 :fill-pointer 0 :adjustable t)
   :read-only t)
  (vec-register (make-hash-table)
   :read-only t)
  (reduces
   (make-array 32 :fill-pointer 0 :adjustable t)))

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
                  (rootp var)
                  (let ((writer (var-op var)))
                    (not (and writer
                              (livep writer))))
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

(defun emit-one-op (op)
  (declare (type op op))
  (map nil (lambda (var)
             (var-reg var t))
       (op-dsts op))
  (let ((args (map 'simple-vector
                   (lambda (x)
                     (if (typep x 'vec)
                         (bsp.vm:var-loc
                          (var-reg x (initial-element-of x)))
                         x))
                   (op-args op))))
    (vector-push-extend (bsp.vm:make-op (op-fun op) args)
                        (codegen-state-ops *codegen-state*))
    (when (typep op 'reducer)
      (vector-push-extend (bsp.vm:make-reduce-step
                           (reducer-2arg-reducer op)
                           (reducer-1arg-reducer op)
                           args)
                          (codegen-state-reduces *codegen-state*)))))

(defun emit-code (ops)
  (let ((*codegen-state* (make-codegen-state)))
    (map nil #'emit-one-op ops)
    *codegen-state*))
