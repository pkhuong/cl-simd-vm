(defvar *vvar-roots* '())

(defmacro with-vvar ((&rest var-names) &body body)
  (let ((gensyms (mapcar (lambda (var)
                           (gensym (string var)))
                         var-names)))
    (unless var-names
      (return-from with-vvar `(locally ,@body)))
    `(let* (,@(loop for g in gensyms
                    for prev in (cons '*vvar-roots* gensyms)
                    collect `(,g (cons nil ,prev)))
            (*vvar-roots* ,(car (last gensyms))))
       (symbol-macrolet ,(loop for v in var-names
                               for g in gensyms
                               collect `(,v (car ,g)))
         ,@body))))

(defvar *default-chunk-size 10)

(defclass bsp-context ()
  (size chunk-size live transient cache))

(defun make-context (count &optional (chunk *default-chunk-size))
  (let ((context (make-instance 'bsp-context)))
    (with-slots (size chunk-size live transient cache) context
      (setf size       count
            chunk-size chunk
            live      (make-array 16 :adjustable t :fill-pointer 0)
            transient (make-array 16 :adjustable t :fill-pointer 0)
            cache     (make-hash-table :test #'equal)))
    context))

(defvar *bsp-context* nil)

(defmacro with-context ((size) &body body)
  (let ((thunk (gensym "THUNK"))
        (_size (gensym "SIZE")))
    `(let ((,_size ,size))
       (flet ((,thunk ()
                ,@body))
         (cond (*bsp-context*
                (assert (= ,_size (slot-value *bsp-context* 'size)))
                (,thunk))
               (t
                (let ((*bsp-context* (make-context ,_size)))
                  (,thunk))))))))

(defclass vector-thunk ()
  (globalp type data thunk mask mask-stack))

;; stack of (var not-p)
(defvar *mask-stack* '())

(defun make-thunk (eltype &rest thunk-args)
  (declare (dynamic-extent thunk-args))
  (let ((instance (make-instance 'vector-thunk)))
    (with-slots (globalp type data thunk mask mask-stack)
        instance
      (setf globalp nil
            type    eltype
            data    nil
            thunk   (coerce thunk-args 'simple-vector)
            mask    (car *mask-stack*)
            mask-stack *mask-stack*))
    (vector-push-extend instance
                        (slot-value *bsp-context* 'transient))
    instance))

(defun make-vector (vector)
  (declare (type (simple-array * 1) vector))
  (when *bsp-context*
    (assert (>= (length vector)
                (slot-value *bsp-context* 'size))))
  (let ((instance (make-instance 'vector-thunk)))
    (with-slots (globalp type data thunk mask mask-stack)
        instance
      (setf globalp t
            type    (array-element-type vector)
            data    vector
            thunk   nil
            mask    nil
            mask-stack nil))
    instance))

(defun mark-live-vars (context reducers)
  (with-slots (live transient) context
    (let ((marked (make-hash-table))
          (masks  (make-hash-table))
          (roots  (make-hash-table)))
      (labels ((mark (obj)
                 (unless (and (typep obj 'vector-thunk)
                              (not (gethash obj marked)))
                   (return-from mark))
                 (setf (gethash obj marked) t)
                 (let ((args (slot-value obj 'thunk))
                       (mask (slot-value obj 'mask)))
                   (when args
                     (map nil #'mark args))
                   (when mask
                     (setf (gethash mask masks) t)
                     (mark mask)))))
        (map nil (lambda (root)
                   (when (typep root 'vector-thunk)
                     (when (slot-value root 'thunk)
                       (setf (gethash root roots) t))
                     (mark root)))
             *vvar-roots*)
        (map nil (lambda (reducer)
                   (mark (second reducer)))
             reducers))
      (values marked roots masks))))

(defun slide-if-not (predicate vector)
  (declare (type (array * 1) vector))
  (let ((index 0))
    (declare (type (mod #.most-positive-fixnum) index))
    (dotimes (i (length vector))
      (let ((obj (aref vector i)))
        (unless (funcall predicate obj)
          (setf (aref vector index) obj
                index               (1+ index)))))
    (fill vector nil :start index)
    (setf (fill-pointer vector) index)
    vector))

(defstruct vector-arg
  loc globalp
  type
  (vector        nil)
  (initial-value nil)
  (reduce-op     nil))

(defvar *ones-vector-arg* (make-vector-arg :loc     0
                                           :globalp nil
                                           :type    '(unsigned-byte 32)
                                           :vector  nil))

(defmethod print-object ((obj vector-arg) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A ~A ~A"
            (if (eql obj *ones-vector-arg*)
                'ones
                (vector-arg-type obj))
            (vector-arg-loc obj)
            (if (vector-arg-globalp obj)
                "G"
                "L"))))

(defun compile-pipeline (pipeline reducers roots masks)
  (let ((registers  (make-array 32 :adjustable t :fill-pointer 1))
        (ops        (make-array (length pipeline)
                                :adjustable t :fill-pointer 0))
        (reduce-registers (make-array (length reducers)
                                      :adjustable t :fill-pointer 0))
        (summarised (make-hash-table))
        (location   (make-hash-table))
        (reducees   (make-hash-table)))
    (loop for reducer in reducers
          for reducee = (second reducer)
          do (push reducer (gethash reducee reducees)))
    (setf (aref registers 0) *ones-vector-arg*)
    (labels ((location (x)
               (assert (typep x 'vector-thunk))
               (cond ((gethash x location))
                     (t
                      (let ((arg (make-vector-arg
                                  :loc (length registers)
                                  :globalp (or (slot-value x 'globalp)
                                               (gethash x roots))
                                  :type (slot-value x 'type)
                                  :vector x)))
                        (setf (gethash x location) arg)
                        (vector-push-extend arg registers)
                        arg))))
             (reducer (op arg)
               (let* ((type (slot-value arg 'type))
                      (arg  (make-vector-arg
                             :loc     (length registers)
                             :globalp nil
                             :type    type
                             :initial-value 0d0
                             :reduce-op op)))
                 (vector-push-extend arg registers)
                 arg))
             (emit-reducer (reducer)
               (destructuring-bind (op source) reducer
                 (let ((reduce-var (reducer op source)))
                   (vector-push-extend reduce-var reduce-registers)
                   (with-slots (type thunk mask) source
                     (let ((flip-p (and (consp mask)
                                        (prog1 t
                                          (setf mask (second mask)))))
                           (mask   (summarise mask)))
                       (vector-push-extend
                        `(,op ,mask ,flip-p nil
                              ,reduce-var
                              ,(location source))
                        ops))))))
             ;; summarise only called on empty mask
             (summarise (mask)
               (check-type mask (or null vector-thunk))
               (cond ((null mask)
                      *ones-vector-arg*)
                     ((location mask)
                      (unless (gethash mask summarised)
                        (setf (gethash mask summarised) t)
                        (vector-push-extend `(summary ,(location mask))
                                            ops))
                      (location mask))
                     (t (error "WTF?")))))
      (loop for op across pipeline do
        (with-slots (type thunk mask) op
          (let ((flip-p (and (consp mask)
                             (prog1 t
                               (setf mask (second mask)))))
                (mask   (summarise mask))
                (used-as-mask (gethash op masks)))
            (when used-as-mask
              (setf (gethash op summarised) t))
            (let ((fun  (aref thunk 0))
                  (args (subseq thunk 1)))
              (vector-push-extend
               `(,fun ,mask ,flip-p ,used-as-mask
                      ,(location op)
                      ,@(map 'list (lambda (x)
                                     (if (typep x 'vector-thunk)
                                         (location x)
                                         x))
                             args))
               ops)
              (map nil #'emit-reducer
                   (gethash op reducees))))))
      (values ops registers reduce-registers))))

(defun execute-pipeline (ops registers reducers size chunk)
  (let ((vectors   (make-array (length registers)))
        (summaries (make-array (length registers)
                               :initial-element 0
                               :element-type 'fixnum)))
    (setf (aref vectors   0) (make-array
                              chunk
                              :element-type '(unsigned-byte 32)
                              :initial-element (ldb (byte 32 0) -1))
          (aref summaries 0) 1)
    (map-into vectors
              (lambda (register)
                (if (eql register *ones-vector-arg*)
                    (make-array chunk
                                :element-type '(unsigned-byte 32)
                                :initial-element (ldb (byte 32 0) -1))
                    (let ((data (if (vector-arg-globalp register)
                                    (or (slot-value
                                         (vector-arg-vector register)
                                         'data)
                                        (setf (slot-value
                                               (vector-arg-vector register)
                                               'data)
                                              (make-array
                                               size
                                               :element-type
                                               (vector-arg-type register))))
                                    (make-array
                                     size
                                     :element-type
                                     (vector-arg-type register)))))
                      (assert (arrayp data))
                      (when (vector-arg-initial-value register)
                        (fill data (vector-arg-initial-value register)))
                      (when (vector-arg-globalp register)
                        (setf (slot-value (vector-arg-vector register)
                                          'data)
                              data))
                      data)))
         registers)
    (loop
      for start from 0 below size by chunk
      for end   = (min size (+ start chunk))
      for count = (- end start)
      do (loop for (op . args) across ops do
        (format t "~A ~A~%"  op args)
            (apply op vectors summaries count
                   (mapcan (lambda (arg)
                             (if (vector-arg-p arg)
                                 (list (vector-arg-loc arg)
                                       (if (vector-arg-globalp arg)
                                           start
                                           0))
                                 (list arg)))
                           args))))
    (map 'simple-vector (lambda (reducer)
                          (format t "~A ~A~%"
                                  reducer
                                  (aref registers
                                        (vector-arg-loc reducer)))
                          (aref vectors
                                (vector-arg-loc reducer)))
         reducers)))

(defun barrier (context reducers &rest new-roots
                &aux (*vvar-roots*
                      (append new-roots *vvar-roots*)))
  (dolist (root new-roots)
    (setf (slot-value root 'globalp) t))
  (multiple-value-bind (marked roots masks)
      (mark-live-vars context reducers)
    (with-slots (live transient) context
      (slide-if-not (lambda (x)
                      (gethash x marked))
                    live)
      (let ((to-compute (remove-if-not (lambda (x)
                                         (gethash x marked))
                                       transient)))
        (map nil (lambda (x)
                   (when (gethash x roots)
                     (vector-push-extend x live)))
             transient)
        (fill transient nil)
        (setf (fill-pointer transient) 0)
        (multiple-value-call
            #'values
          (values
           (multiple-value-call #'execute-pipeline
             (compile-pipeline to-compute reducers roots masks)
             (slot-value context 'size)
             (slot-value context 'chunk-size)))
          (values-list new-roots))))))

(defun has-root (stack root)
  (or (null root)
      (loop for substack on stack by #'cdr
            thereis (eq substack root))))

(defun call-with-condition (condition if-thunk else-thunk)
  (let ((current-stack    *mask-stack*)
        (condition-stack (slot-value condition 'mask-stack)))
    (assert (has-root current-stack condition-stack))
    (unless (eq current-stack condition-stack)
      (setf condition
            (make-thunk '(unsigned-byte 32)
                        'videntity condition)))
    (values
     (let ((*mask-stack* (cons condition current-stack)))
       (multiple-value-list (funcall if-thunk)))
     (when else-thunk
       (let ((*mask-stack* (cons (if current-stack
                                     (make-thunk '(unsigned-byte 32)
                                                 'vcomplement condition)
                                     (list 'not condition))
                                 current-stack)))
         (multiple-value-list (funcall else-thunk)))))))

(defmacro vwhen (condition &rest body)
  `(values-list
    (call-with-condition ,condition
                         (lambda () ,@body))))

(defun %merge-if (condition then else)
  (values-list
   (loop for then in then
         for else in else
         collect (make-thunk (slot-value then 'type)
                             'vmerge condition then else))))

(defmacro vif (condition then else)
  (let ((_condition (gensym "CONDITION")))
    `(let ((,_condition ,condition))
       (multiple-value-call #'%merge-if
         ,_condition
         (call-with-condition ,_condition
                              (lambda () ,then)
                              (lambda () ,else))))))

(defun summary (vectors summaries count src-index src-start)
  (let ((all-t   t)
        (all-nil t)
        (src     (aref vectors src-index)))
    (loop for i from src-start below (+ src-start count)
          for x = (aref src i)
          do (unless (zerop x)
               (setf all-nil nil))
             (unless (eql x (ldb (byte 32 0) -1))
               (setf all-t   nil))
          (unless (or all-t all-nil)
            (return)))
    (setf (aref summaries src-index)
          (cond (all-nil -1)
                (all-t    1)
                (t        0)))))

(defun vmerge (vectors summaries
               count mask-index mask-start flip-p for-mask-p
               dst-index dst-start
               select-index select-start
               then-index then-start
               else-index else-start)
  (let ((mask   (aref vectors mask-index))
        (dst    (aref vectors dst-index))
        (select (aref vectors select-index))
        (then   (aref vectors then-index))
        (else   (aref vectors else-index))
        (logand -1)
        (logior 0))
    (dotimes (i count)
      (setf (aref dst (+ i dst-start))
            (if (let ((mask (aref mask (+ i mask-start))))
                  (if flip-p
                      (not (zerop mask))
                      (zerop mask)))
                (prog1 (coerce 0 (array-element-type dst))
                  (setf logand 0))
                (let ((v (if (zerop (aref select (+ i select-start)))
                             (aref else (+ i else-start))
                             (aref then (+ i then-start)))))
                  (when for-mask-p
                    (setf logand (logand logand v)
                          logior (logior logior v)))
                  v))))
    (cond ((eql logior 0)
           (setf (aref summaries dst-index) -1))
          ((eql logand (ldb (byte 32 0) -1))
           (setf (aref summaries dst-index) 1))
          (t
           (setf (aref summaries dst-index) 0)))))

(defun vcomplement (vectors summaries
                    count mask-index mask-start flip-p for-mask-p
                    dst-index dst-start
                    src-index src-start)
  (let ((mask   (aref vectors mask-index))
        (dst    (aref vectors dst-index))
        (src    (aref vectors src-index))
        (flip   (if flip-p (ldb (byte 32 0) -1) 0))
        (logand -1)
        (logior 0))
    (dotimes (i count)
      (let ((v (logxor (aref src  (+ i src-start))
                       (aref mask (+ i mask-start))
                       flip)))
        (when for-mask-p
          (setf logand (logand logand v)
                logior (logior logior v)))
        (setf (aref dst (+ i dst-start)) v)))
    (cond ((eql logior 0)
           (setf (aref summaries dst-index) -1))
          ((eql logand (ldb (byte 32 0) -1))
           (setf (aref summaries dst-index) 1))
          (t
           (setf (aref summaries dst-index) 0)))))

(macrolet
    ((def-unop (op &optional (name (intern (format nil "V~A" op))))
       `(defun ,name (vectors summaries count
                      mask-index mask-start flip-p for-mask-p
                      dst-index dst-start
                      x-index x-start)
          (let ((mask   (aref vectors mask-index))
                (dst    (aref vectors dst-index))
                (x      (aref vectors x-index)))
            #+nil
            (assert (not for-mask-p))
            (dotimes (i count)
              (setf (aref dst (+ i dst-start))
                    (if (let ((mask (aref mask (+ i mask-start))))
                          (if flip-p
                              (not (zerop mask))
                              (zerop mask)))
                        (coerce 0 (array-element-type dst))
                        (,op (aref x (+ i x-start))))))
            (setf (aref summaries dst-index) 0)))))
  (def-unop / vinv)
  (def-unop - vneg)
  (def-unop (lambda (x)
              (ldb (byte 32 0) (lognot x)))
      vnot)
  (def-unop identity))

(defun reduce/+ (vectors summaries count
                 mask-index mask-start flip-p for-mask-p
                 acc-index acc-start
                 x-index   x-start)
  summaries for-mask-p
  (let ((mask   (aref vectors mask-index))
        (acc    (aref vectors acc-index))
        (x      (aref vectors x-index)))
    (dotimes (i count)
      (incf (aref acc (+ i acc-start))
            (if (let ((mask (aref mask (+ i mask-start))))
                  (if flip-p
                      (not (zerop mask))
                      (zerop mask)))
                0
                (aref x (+ i x-start)))))))

(macrolet
    ((def-unop (op &optional (name (intern (format nil "V~A" op))))
       `(defun ,name (vectors summaries count
                      mask-index mask-start flip-p for-mask-p
                      dst-index dst-start
                      x-index x-start)
          (let ((mask   (aref vectors mask-index))
                (dst    (aref vectors dst-index))
                (x      (aref vectors x-index)))
            #+nil
            (assert (not for-mask-p))
            (dotimes (i count)
              (setf (aref dst (+ i dst-start))
                    (if (let ((mask (aref mask (+ i mask-start))))
                          (if flip-p
                              (not (zerop mask))
                              (zerop mask)))
                        (coerce 0 (array-element-type dst))
                        (,op (aref x (+ i x-start))))))
            (setf (aref summaries dst-index) 0)))))
  (def-unop / vinv)
  (def-unop - vneg)
  (def-unop (lambda (x)
              (ldb (byte 32 0) (lognot x)))
      vnot)
  (def-unop identity))

(macrolet
    ((def-binop (op &optional (name (intern (format nil "V~A" op))))
       `(defun ,name (vectors summaries count
                      mask-index mask-start flip-p for-mask-p
                      dst-index dst-start
                      x-index x-start
                      y-index y-start)
          (let ((mask   (aref vectors mask-index))
                (dst    (aref vectors dst-index))
                (x      (aref vectors x-index))
                (y      (aref vectors y-index)))
            #+nil(assert (not for-mask-p))
            (dotimes (i count)
              (setf (aref dst (+ i dst-start))
                    (if (let ((mask (aref mask (+ i mask-start))))
                          (if flip-p
                              (not (zerop mask))
                              (zerop mask)))
                        (coerce 0 (array-element-type dst))
                        (,op (aref x (+ i x-start))
                             (aref y (+ i y-start))))))
            (setf (aref summaries dst-index) 0)))))
  (def-binop +)
  (def-binop *)
  (def-binop -)
  (def-binop /)
  (def-binop (lambda (x y)
               (if (eql x y)
                   (ldb (byte 32 0) -1)
                   0))
             v=))

(defvar *x* (make-array 100 :element-type 'double-float
                           :initial-element 3d0))

(defvar *y* (make-array 100 :element-type 'double-float
                           :initial-element 10d0))

(defvar *condition* (map-into (make-array
                               100 :element-type '(unsigned-byte 32))
                              (lambda ()
                                (ldb (byte 32 0)
                                     (- (random 2))))))

(defvar *condition2* (map-into (make-array
                                100 :element-type '(unsigned-byte 32))
                               (lambda ()
                                 (ldb (byte 32 0)
                                      (- (random 2))))))

#||
TODO:
 emit IR for pipelined parallel-for
   handle mask stack and mask index:
    each maskful op always write 0 when parent mask
     is 0!
    and always return when asked for it:
         trinary value: all 0, all 1, mixed (-1, 1, 0)
          because only asked when used for mask, and masks
           never have don't care.
    -> individual opcodes can dispatch on that
    -> mask vector is potentially garbage if known all 0/all 1

 - need a binary summary op for mask on global
 - definitely need maskify [identity] op
 --> need to detect when have to insert temporary mask vector

 ** if/case are implemented as masked execution + merge

         op size mask offset params dst offset args
 maskful-op size mask offset params dst offset args

params: trinary summary, flip-p, for-mask
  for-mask: summary needed, and undefined must be zero
||#
