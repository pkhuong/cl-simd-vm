(defpackage "BSP.VM"
  (:use "CL")
  (:export "VAR" "MAKE-VAR"
           "VAR-P" "VAR-LOC" "VAR-GLOBALP" "VAR-READ-ONLYP"
           "VAR-LIVEP" "VAR-MASKP" "VAR-ELTYPE" "VAR-INITIAL-ELEMENT"
           "OP"          "MAKE-OP"
           "REDUCE-STEP" "MAKE-REDUCE-STEP"
           "BBLOCK"      "MAKE-BBLOCK"
           "EXECUTE-BBLOCK"))

(in-package "BSP.VM")
(deftype index ()
  `(mod ,most-positive-fixnum))

(defmacro missing-arg (arg)
  `(error "Missing argument to constructor ~S" ',arg))

(declaim (inline ensure-function))
(defun ensure-function (x)
  (if (functionp x) x (fdefinition x)))

(defstruct (var
            (:constructor
                make-var (loc globalp constantp livep maskp
                          eltype initial-element)))
  (loc  (missing-arg loc)  :read-only t
                           :type      index)
  (globalp    nil          :read-only t)
  (constantp  nil          :read-only t)
  (livep      nil          :read-only t)
  (maskp      nil          :read-only t)
  (eltype     nil          :read-only t)
  (initial-element nil     :read-only t))
(declaim (sb-ext:freeze-type var))

(defstruct (op
            (:constructor make-op (function rest
                                   &aux (fun  (ensure-function function))
                                        (args (coerce rest 'simple-vector)))))
  "Operation: executed as (fun vectors summaries count args...); if any arg is
   a VAR, it's replaced with the VAR's LOC and the starting index."
  (fun  (missing-arg  fun) :read-only t
                           :type      function)
  (args (missing-arg args) :read-only t
                           :type      simple-vector))
(declaim (sb-ext:freeze-type op))

(deftype vectors ()
  '(simple-array sb-ext:word 1))

(deftype properties ()
  '(simple-array (unsigned-byte 32) 1))

(defconstant +constant-flag+  (ash 1 0))
(defconstant +dead-flag+      (ash 1 1))
(defconstant +mask-flag+      (ash 1 2))
(defconstant +global-flag+    (ash 1 3))

(declaim (inline execute-op))
(defun execute-op (op vectors properties chunk-start chunk-size)
  (declare (type op op)
           (type vectors vectors)
           (type properties properties)
           (type index chunk-start)
           (type index chunk-size)
           (optimize speed))
  (let ((fun  (op-fun op))
        (args (op-args op)))
    (macrolet ((emit-call (n)
                 (declare (optimize (speed 0)))
                 `(funcall fun vectors properties chunk-start chunk-size
                    ,@(loop for i below n collect
                            `(aref args ,i))))
               (emit-calls (max &body default)
                 (declare (optimize (speed 0)))
                 `(case (length args)
                    ,@(loop for i upto max collect
                            `(,i
                              (locally (declare (type (simple-array t (,i)) args))
                                (emit-call ,i))))
                    (t ,@default))))
      (emit-calls 7
       (apply fun vectors properties chunk-start chunk-size
              (map 'list #'identity args))))
    nil))

(defstruct (reduce-step
            (:constructor make-reduce-step
                (2arg-reducer 1arg-reducer args thunk
                 &aux
                   (2arg-reducer (ensure-function 2arg-reducer))
                   (1arg-reducer (ensure-function 1arg-reducer))
                   (args (coerce args 'simple-vector))
                   (thunk (ensure-function thunk)))))
  (2arg-reducer nil :read-only t
                    :type      function)
  (1arg-reducer nil :read-only t
                    :type      function)
  (args         nil :read-only t
                    :type      simple-vector)
  (thunk        nil :read-only t
                    :type function))
(declaim (sb-ext:freeze-type reduce-step))

(declaim (inline execute-2arg execute-1arg))
(defun execute-2arg (op
                     vectors1 properties1
                     vectors2 properties2
                     chunk-start chunk-size)
  (declare (type reduce-step op)
           (type vectors    vectors1    vectors2)
           (type properties properties1 properties2)
           (type index chunk-start)
           (type index chunk-size)
           (optimize speed))
  (let ((fun  (reduce-step-2arg-reducer op))
        (args (reduce-step-args op)))
    (macrolet ((emit-call (n)
                 (declare (optimize (speed 0)))
                 `(funcall fun vectors1 properties1 vectors2 properties2
                           chunk-start chunk-size
                    ,@(loop for i below n collect
                            `(aref args ,i))))
               (emit-calls (max &body default)
                 (declare (optimize (speed 0)))
                 `(case (length args)
                    ,@(loop for i upto max collect
                            `(,i
                              (locally (declare (type (simple-array t (,i)) args))
                                (emit-call ,i))))
                    (t ,@default))))
      (emit-calls 7
       (apply fun vectors1 properties1 vectors2 properties2
              chunk-start chunk-size
              (map 'list #'identity args))))
    nil))

(defun execute-1arg (op
                     vectors1 properties1
                     chunk-start chunk-size)
  (declare (type reduce-step op)
           (type vectors    vectors1)
           (type properties properties1)
           (type index chunk-start)
           (type index chunk-size)
           (optimize speed))
  (let ((fun  (reduce-step-1arg-reducer op))
        (args (reduce-step-args op)))
    (macrolet ((emit-call (n)
                 (declare (optimize (speed 0)))
                 `(funcall fun vectors1 properties1
                           chunk-start chunk-size
                    ,@(loop for i below n collect
                            `(aref args ,i))))
               (emit-calls (max &body default)
                 (declare (optimize (speed 0)))
                 `(case (length args)
                    ,@(loop for i upto max collect
                            `(,i
                              (locally (declare (type (simple-array t (,i)) args))
                                (emit-call ,i))))
                    (t ,@default))))
      (let ((value
              (emit-calls 7
               (apply fun vectors1 properties1
                      chunk-start chunk-size
                      (map 'list #'identity args)))))
        (funcall (reduce-step-thunk op) value)
        value))))

(defstruct (bblock
            (:constructor make-bblock (count chunk _vars _ops _reduced
                                       &aux
                                         (vars (coerce _vars 'simple-vector))
                                         (ops  (coerce _ops  'simple-vector))
                                         (reduced (coerce _reduced 'simple-vector)))))
  (count (missing-arg count) :type index :read-only t)
  (chunk (missing-arg chunk) :type index :read-only t)
  (vars (missing-arg vars) :read-only t
                           :type (simple-array var 1))
  (ops  (missing-arg  ops) :read-only t
                           :type (simple-array op 1))
  (reduced (missing-arg reduced) :read-only t
                                 :type (simple-array reduce-step 1)))
(declaim (sb-ext:freeze-type bblock))

(defvar *to-free*)
(sb-alien:define-alien-routine malloc (* t) (size sb-alien:unsigned-long))
(sb-alien:define-alien-routine free sb-alien:void (ptr (* t)))

(defmacro with-allocation-pool (&body body)
  `(let ((*to-free* (make-array 32 :adjustable t :fill-pointer 0
                                :initial-element nil)))
     (unwind-protect (locally ,@body)
       (map nil (lambda (x)
                  (when x (free x)))
            *to-free*))))

(defun alloc-double-vector (size &optional initial-element)
  (declare (type index size)
           (type (or null double-float) initial-element)
           (optimize speed))
  (let* ((count (the index (* size 8)))
         (alloc (sb-alien:alien-sap (malloc count))))
    (vector-push-extend alloc *to-free*)
    (when initial-element
      (let ((initial-element initial-element))
        (loop for i of-type index from 0 below count by 8 do
          (setf (sb-sys:sap-ref-double alloc i) initial-element))))
    alloc))

(defun alloc-unsigned-vector (size &optional initial-element)
  (declare (type index size)
           (type (or null (unsigned-byte 32)) initial-element)
           (optimize speed))
  (let* ((count (the index (* size 4)))
         (alloc (sb-alien:alien-sap (malloc count))))
    (vector-push-extend alloc *to-free*)    
    (when initial-element
      (let ((initial-element initial-element))
        (loop for i of-type index from 0 below count by 4 do
          (setf (sb-sys:sap-ref-32 alloc i) initial-element))))
    alloc))

(defun alloc-vector (type size &optional initial-element)
  (ecase type
    ((bsp:bool bsp:u32)
     (alloc-unsigned-vector size initial-element))
    (bsp:double
     (alloc-double-vector size initial-element))))

(defun globally-setup-bblock (bblock)
  (declare (type bblock bblock)
           (optimize debug))
  (let* ((count      (bblock-count bblock))
         (chunk      (bblock-chunk bblock))
         (vars       (bblock-vars  bblock))
         (nvars      (length vars))
         (vectors    (make-array nvars))
         (properties (make-array nvars :element-type '(unsigned-byte 32)
                                       :initial-element 0))
         (strides    (make-array nvars :element-type 'sb-ext:word)))
    (loop for i below nvars
          for var across vars
          for type = (var-eltype var)
          do (let* ((initial-element (var-initial-element var))
                    (globalp         (var-globalp var))
                    (size            (if globalp count chunk))
                    (data            (cond
                                       (globalp
                                        (or (funcall globalp)
                                            (if initial-element
                                                (make-array
                                                 size
                                                 :element-type type
                                                 :initial-element initial-element)
                                                (make-array
                                                 size
                                                 :element-type type))))
                                       ((var-constantp var)
                                        (alloc-vector type size initial-element))))
                    (flags           (logior
                                      (if (var-globalp var)
                                          +global-flag+
                                          0)
                                      (if (var-livep var)
                                          0
                                          +dead-flag+)
                                      (if (var-maskp var)
                                          +mask-flag+
                                          0)
                                      (if (var-constantp var)
                                          +constant-flag+
                                          0))))
               (when globalp
                 (funcall globalp data))
               (setf (aref vectors    i) data
                     (aref properties i) flags
                     (aref strides    i) (if globalp
                                             (ecase type
                                               ((bsp:bool bsp:u32) 4)
                                               (bsp:double         8))
                                             0))))
    (values vectors properties strides)))

(defun locally-setup-bblock (bblock base-vectors base-properties)
  (declare (type bblock bblock)
           (type (simple-array t 1) base-vectors)
           (type properties base-properties))
  (let* ((chunk      (bblock-chunk bblock))
         (vars       (bblock-vars  bblock))
         (nvars      (length vars))
         (vectors    (copy-seq base-vectors))
         (properties (copy-seq base-properties)))
    (loop for i below nvars
          for var across vars
          for type = (var-eltype var)
          unless (aref base-vectors i)
            do (let ((initial-element (var-initial-element var)))
               (setf (aref vectors i)
                     (alloc-vector type chunk initial-element))))
    (values vectors properties)))

(defun %merge-local-data (bblock vectors1 properties1 vectors2 properties2)
  (declare (type bblock bblock)
           (type vectors    vectors1 vectors2)
           (type properties properties1 properties2))
  (loop with chunk = (bblock-chunk bblock)
        for reducer across (bblock-reduced bblock)
        do (execute-2arg reducer
                         vectors1 properties1
                         vectors2 properties2
                         0 chunk))
  vectors1)

(defun merge-local-data (bblock vectors properties)
  (declare (type bblock bblock)
           (type (simple-array vectors    1) vectors)
           (type (simple-array properties 1) properties))
  (let ((accumulator (aref vectors 0))
        (props       (aref properties 0)))
    (loop for i from 1 below (length vectors)
          do (%merge-local-data bblock accumulator props
                                (aref vectors i) (aref properties i)))
    (let ((chunk (bblock-chunk bblock)))
      (map 'simple-vector
           (lambda (reducer)
             (execute-1arg reducer accumulator props 0 chunk))
           (bblock-reduced bblock)))))

(declaim (inline execute-one-chunk))
(defun execute-one-chunk (ops base-vectors vectors
                          strides
                          properties
                          chunk-start chunk-size)
  (declare (type (simple-array op 1) ops)
           (type (simple-array sb-ext:word 1) strides)
           (type vectors    base-vectors vectors)
           (type properties properties)
           (type index chunk-start chunk-size)
           (optimize speed))
  (map-into vectors (lambda (x stride)
                      (ldb (byte sb-vm:n-word-bits 0)
                           (+ x (* stride chunk-start))))
            base-vectors strides)
  (map nil (lambda (op)
             (execute-op op vectors properties chunk-start chunk-size))
       ops))

(defun call-with-pinned-vectors (function vectors)
  (declare (type (simple-array t 1) vectors))
  (labels ((rec (i)
             (if (>= i (length vectors))
                 (funcall function
                          (map '(simple-array sb-ext:word 1)
                               (lambda (x)
                                 (sb-sys:sap-int
                                  (etypecase x
                                    (sb-sys:system-area-pointer x)
                                    ((simple-array * 1)
                                     (sb-sys:vector-sap x)))))
                               vectors))
                 (sb-sys:with-pinned-objects ((aref vectors i))
                   (rec (1+ i))))))
    (rec 0)))

(defun execute-bblock (bblock)
  (declare (type bblock bblock))
  (with-allocation-pool
    (multiple-value-bind (base-vectors base-properties strides)
        (globally-setup-bblock bblock)
      (let ((count (bblock-count bblock))
            (chunk (bblock-chunk bblock))
            (ops   (bblock-ops   bblock)))
        (multiple-value-bind (vectors properties)
            (locally-setup-bblock bblock base-vectors base-properties)
          (declare (type properties properties))
          (call-with-pinned-vectors
           (lambda (base-vectors &aux (copies (copy-seq base-vectors)))
             (declare (type vectors base-vectors copies)
                      (optimize speed))
             (let ((properties-copy
                     (make-array (length properties)
                                 :element-type '(unsigned-byte 32))))
               (loop for start of-type index from 0 below count by chunk
                     for end   of-type index = (min (+ start chunk) count)
                     for size  of-type index = (- end start)
                     do (replace properties-copy properties)
                        (execute-one-chunk ops
                                           base-vectors copies
                                           strides      properties-copy
                                           start        size))))
           vectors)
          (call-with-pinned-vectors
           (lambda (vectors)
             (merge-local-data bblock
                               (vector vectors)
                               (vector properties)))
           vectors))))))
