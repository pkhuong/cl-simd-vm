(defpackage "BSP.VM"
  (:use "CL")
  (:export "VAR" "GVAR" "LVAR" "CVAR" "RVAR"
           "MAKE-GVAR" "MAKE-LVAR" "MAKE-CVAR" "MAKE-RVAR"
           "+NO-INITIAL-ELEMENT+"
           "OP"      "BBLOCK"
           "MAKE-OP" "MAKE-BBLOCK"
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
            (:constructor nil))
  (loc  (missing-arg loc)  :read-only t
                           :type      index)
  (type (missing-arg type) :read-only t))

(defstruct (gvar
            (:include var)
            (:constructor make-gvar (loc type storage-thunk
                                     &aux (storage-thunk
                                           (ensure-function storage-thunk)))))
  "Global variable: the storage thunk is called without argument
   to find the backing array, with a single argument to set it if
   missing."
  (storage-thunk (missing-arg storage-thunk)
        :read-only t
        :type      function))

(defstruct (lvar
            (:include var)
            (:constructor make-lvar (loc type)))
  "Local variable: a local vector is consed by each worker thread")

(defconstant +no-initial-element+ '+no-initial-element+)
(defstruct (cvar
            (:include lvar)
            (:constructor make-cvar (loc type initial-element)))
  "Variables with constant values"
  (initial-element (missing-arg initial-element)
                   :type (not (eql +no-initial-element+))
                   :read-only t))

(defstruct (rvar
            (:include lvar)
            (:constructor make-rvar (loc type initial-element
                                     2arg 1arg
                                     &aux (2arg (ensure-function 2arg))
                                       (1arg (ensure-function 1arg)))))
  "Reducing variable: local vectors that are aggregated by 2arg, and finally
   1arg at the end."
  (initial-element +no-initial-element+ :read-only t)
  (2arg   (missing-arg two-arg-reducer) :type function
                                        :read-only t)
  (1arg   (missing-arg one-arg-reducer) :type function
                                        :read-only t))
(declaim (sb-ext:freeze-type var gvar lvar cvar rvar))

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

(declaim (inline execute-op))
(defun execute-op (op vectors summaries chunk-start chunk-size)
  (declare (type op op)
           (type simple-vector vectors)
           (type (simple-array fixnum 1) summaries)
           (type index chunk-start)
           (type index chunk-size)
           (optimize speed))
  (let ((fun  (op-fun op))
        (args (op-args op)))
    (macrolet ((emit-call (n)
                 (declare (optimize (speed 0)))
                 `(multiple-value-call fun
                    vectors summaries chunk-size
                    ,@(loop for i below n collect
                            `(let ((arg (aref args ,i)))
                               (if (var-p arg)
                                   (values (var-loc arg)
                                           (etypecase arg
                                             (gvar chunk-start)
                                             (lvar 0)))
                                   arg)))))
               (emit-calls (max &body default)
                 (declare (optimize (speed 0)))
                 `(case (length args)
                    ,@(loop for i upto max collect
                            `(,i
                              (locally (declare (type (simple-array t (,i)) args)
                                                (optimize (safety 0)))
                                (emit-call ,i))))
                    (t ,@default))))
      (emit-calls 7
       (apply fun vectors summaries chunk-size
              (loop for arg across args
                    if (var-p arg)
                      collect (var-loc arg)
                      and collect (etypecase arg
                                    (gvar chunk-start)
                                    (lvar 0))
                    else collect arg))))
    nil))

(defstruct (bblock
            (:constructor make-bblock (count chunk vars ops reduced
                                       &aux (vars (coerce vars 'simple-vector))
                                            (ops  (coerce ops  'simple-vector))
                                            (reduced (coerce reduced 'simple-vector)))))
  (count (missing-arg count) :type index :read-only t)
  (chunk (missing-arg chunk) :type index :read-only t)
  (vars (missing-arg vars) :read-only t
                           :type (simple-array var 1))
  (ops  (missing-arg  ops) :read-only t
                           :type (simple-array op 1))
  ;; array of RVARs, to return values in order.
  (reduced (missing-arg reduced) :read-only t
                                 :type (simple-array rvar 1)))
(declaim (sb-ext:freeze-type bblock))

;; FIXME: make this portable (:
(defun summarise (vector)
  (declare (type (simple-array * 1) vector))
  #+sbcl
  (let ((w1 (sb-kernel:%vector-raw-bits vector 0))
        (w2 (sb-kernel:%vector-raw-bits vector 1)))
    (cond ((= (logior w1 w2) 0) -1)
          ((= (logand w1 w2) (ldb (byte sb-vm:n-word-bits 0) -1))
           1)
          (t 0)))
  #-sbcl 0)

(defun globally-setup-bblock (bblock)
  (declare (type bblock bblock))
  (let* ((count     (bblock-count bblock))
         (chunk     (bblock-chunk bblock))
         (vars      (bblock-vars  bblock))
         (nvars     (length vars))
         (vectors   (make-array nvars))
         (summaries (make-array nvars :element-type 'fixnum
                                      :initial-element 0)))
    (loop for i below nvars
          for var across vars
          for type = (var-type var)
          do (typecase var
               (gvar
                (setf (aref vectors i)
                      (let ((thunk (gvar-storage-thunk var)))
                        (or (funcall thunk)
                            (let ((data (make-array count
                                                    :element-type type)))
                              (funcall thunk data)
                              data)))))
               (cvar
                (let* ((initial-element (cvar-initial-element var))
                       (data            (make-array chunk
                                                    :element-type type
                                                    :initial-element initial-element)))
                  (setf (aref vectors   i) data
                        (aref summaries i) (summarise data))))))
    (values vectors summaries)))

(defun locally-setup-bblock (bblock base-vectors base-summaries)
  (declare (type bblock bblock)
           (type (simple-array t 1) base-vectors)
           (type (simple-array fixnum 1) base-summaries))
  (let* ((chunk     (bblock-chunk bblock))
         (vars      (bblock-vars  bblock))
         (nvars     (length vars))
         (vectors   (copy-seq base-vectors))
         (summaries (copy-seq base-summaries)))
    (loop for i below nvars
          for var across vars
          for type = (var-type var)
          when (typep var '(and lvar (not cvar)))
          do (let ((initial-element
                     (etypecase var
                       (rvar (rvar-initial-element var))
                       (lvar +no-initial-element+))))
               (setf (aref vectors i)
                     (if (eql initial-element +no-initial-element+)
                         (make-array chunk :element-type type)
                         (make-array chunk :element-type type
                                           :initial-element initial-element)))))
    (values vectors summaries)))

(defun %merge-local-data (bblock vectors1 vectors2)
  (declare (type bblock bblock)
           (type (simple-array t 1) vectors1)
           (type (simple-array t 1) vectors2))
  (loop with chunk = (bblock-chunk bblock)
        for rvar across (bblock-reduced bblock)
        for loc = (rvar-loc rvar)
        for 2arg = (rvar-2arg rvar)
        do (funcall 2arg chunk (aref vectors1 loc) (aref vectors2 loc)))
  vectors1)

(defun merge-local-data (bblock vectors)
  (declare (type bblock bblock)
           (type (simple-array simple-vector 1) vectors))
  (let ((accumulator (aref vectors 0)))
    (loop for i from 1 below (length vectors)
          do (%merge-local-data bblock accumulator (aref vectors i)))
    (let ((chunk (bblock-chunk bblock)))
      (map 'simple-vector
           (lambda (rvar)
             (funcall (rvar-1arg rvar)
                      chunk
                      (aref accumulator (rvar-loc rvar))))
           (bblock-reduced bblock)))))

(declaim (inline execute-one-chunk))
(defun execute-one-chunk (ops vectors summaries chunk-start chunk-size)
  (declare (type (simple-array op 1) ops)
           (type (simple-array (simple-array * 1) 1) vectors)
           (type (simple-array fixnum 1) summaries)
           (type index chunk-start chunk-size)
           (optimize speed))
  (map nil (lambda (op)
             (execute-op op vectors summaries chunk-start chunk-size))
       ops))

(defun execute-bblock (bblock)
  (declare (type bblock bblock))
  (multiple-value-bind (base-vectors base-summaries)
      (globally-setup-bblock bblock)
    (let ((count (bblock-count bblock))
          (chunk (bblock-chunk bblock))
          (ops   (bblock-ops   bblock)))
      (multiple-value-bind (vectors summaries)
          (locally-setup-bblock bblock base-vectors base-summaries)
        (loop for start from 0 below count by chunk
              for end = (min (+ start chunk) count)
              for size = (- end start)
              do (execute-one-chunk ops vectors summaries
                                    start size))
        (merge-local-data bblock (vector vectors))))))
