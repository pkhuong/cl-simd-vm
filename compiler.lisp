(defpackage "BSP.COMPILER"
  (:export "MAKE-REDUCER" "REDUCER" "TO-BBLOCK")
  (:use "CL"))

(in-package "BSP.COMPILER")
(deftype index ()
  `(mod ,most-positive-fixnum))

(declaim (inline ensure-function))
(defun ensure-function (x)
  (if (functionp x) x (fdefinition x)))

;;; Used by the front-end to specify on-the-fly reduce ops
(defstruct (reducer
            (:constructor %make-reducer (args mask eltype
                                         initial-element
                                         vector-reducer
                                         2arg-reducer
                                         1arg-reducer)))
  (args           #() :type (simple-array t 1)
                      :read-only t)
  (mask           nil :type (or null cons bsp.front:bsp-vector))
  (eltype         nil :read-only t)
  (initial-element nil :read-only t)
  (vector-reducer nil :type function :read-only t)
  (2arg-reducer   nil :type function :read-only t)
  (1arg-reducer   nil :type function :read-only t))

(defun make-reducer (args eltype initial-element
                     vector-reducer 2arg-reducer 1arg-reducer)
  (let* ((args (coerce args 'simple-vector))
         (sources (remove-if-not (lambda (x)
                                   (typep x 'bsp.front:bsp-vector))
                                 args))
         (stack
           (reduce (lambda (x y)
                     (if (> (length x) (length y))
                         x y))
                   sources :key #'bsp.front:mask-stack-of
                   :initial-value nil)))
    (assert (every (lambda (source)
                     (bsp.front:has-root
                      stack
                      (bsp.front:mask-stack-of source)))
                   sources))
    (%make-reducer args
                   (car stack)
                   eltype initial-element
                   (ensure-function vector-reducer)
                   (ensure-function 2arg-reducer)
                   (ensure-function 1arg-reducer))))

;;; Initial in-order traversal of the graph, starting at
;;; roots and reducer args.

;; hash set of live bsp-vector
(defvar *live-vars*)
;; hash set of bsp-vector used as masks
(defvar *mask-vars*)
;; hash set of bsp-vectors that are used as results
(defvar *roots*)
;; hash map of bsp var -> callbacks
(defvar *var-callbacks*)

(defun mark-live (root)
  (unless (and (typep root '(or bsp.front:bsp-vector reducer))
               (not (gethash root *live-vars*)))
    (return-from mark-live))
  (when (and (typep root 'bsp.front:bsp-vector)
             (bsp.front:data-of root))
    (return-from mark-live))
  (when (typep root 'bsp.front:bsp-vector)
    (setf (gethash root *live-vars*) t))
  (multiple-value-bind (mask args)
      (etypecase root
        (bsp.front:bsp-vector
         (values (bsp.front:mask-of root)
                 (bsp.front:op-of root)))
        (reducer
         (values (reducer-mask root)
                 (reducer-args root))))
    (when mask
      (when (consp mask)
        (setf mask (second mask)))
      (setf (gethash mask *mask-vars*) t)
      (mark-live mask))
    (map nil #'mark-live args)))

(defun mark-reducer-args (reducer ops)
  (declare (type reducer reducer)
           (type (simple-array bsp.front:bsp-vector 1) ops))
  (loop with args = (reducer-args reducer)
        with mask = (let ((mask (reducer-mask reducer)))
                      (if (consp mask)
                          (second mask)
                          mask))
        for i from (1- (length ops)) downto 0
        for op = (aref ops i)
        do (when (or (position op args)
                     (eql op mask))
             (push (lambda ()
                     (compile-reducer reducer))
                   (gethash op *var-callbacks*))
             (return))))

;;; In-order codegen for live vars. Backward flowing
;;;  info happened during mark-live.  Forward flowing
;;;  happens during in-order emission.
;;; TODO: stuff like regalloc

;; vectors of bsp-vm vars, ops
(defvar *vm-vars*)
(defvar *vm-ops*)
;; hash table bsp-vector -> vm-var
(defvar *var-vm-var*)
;; special var: constant all -1.
(defvar *ones-var*)

;;; Lazily associate vars with bsp variables
(defun vm-var (type globalp)
  ;; globalp is the storage thunk
  (let* ((vars *vm-vars*)
         (loc  (length vars))
         (var  (if globalp
                   (bsp.vm:make-gvar loc
                                     type
                                     globalp)
                   (bsp.vm:make-lvar loc
                                     type))))
    (vector-push-extend var vars)
    var))

(defun var-vm-var (bsp-var &optional creating)
  (declare (type bsp.front:bsp-vector bsp-var))
  (or (gethash bsp-var *var-vm-var*)
      (setf (gethash bsp-var *var-vm-var*)
            (vm-var (bsp.front:eltype-of bsp-var)
                    (cond ((bsp.front:data-of bsp-var)
                           (let ((x (bsp.front:data-of bsp-var)))
                             (lambda (&optional dst)
                               (assert (not dst))
                               x)))
                          ((gethash bsp-var *roots*)
                           (lambda (&optional dst)
                             (if dst
                                 (setf (bsp.front:data-of bsp-var) dst)
                                 (bsp.front:data-of bsp-var))))
                          (t (assert creating)
                             nil))))))

;; Lazily summarise inputs
(defvar *summarised-vm-vars*)
(defun ensure-summary (mask)
  (when (and (typep mask 'bsp.front:bsp-vector)
             (bsp.front:data-of mask)
             (not (gethash mask *summarised-vm-vars*)))
    (setf (gethash mask *summarised-vm-vars*) t)
    (vector-push-extend (bsp.vm:make-op 'bsp.vm-op:summarise
                                        (vector (var-vm-var mask)))
                        *vm-ops*)))

(defun compile-op (op)
  (declare (type bsp.front:bsp-vector op))
  (let ((thunk          (bsp.front:op-of   op))
        (mask           (bsp.front:mask-of op))
        (flip-p         nil)
        (summary-needed (gethash op *mask-vars*)))
    (when (consp mask)
      (setf mask   (second mask)
            flip-p t))
    (unless mask
      (setf mask *ones-var*))
    (ensure-summary mask)
    (vector-push-extend
     (bsp.vm:make-op
      (aref thunk 0)
      (map 'simple-vector
           (lambda (x)
             (if (typep x 'bsp.front:bsp-vector)
                 (var-vm-var x (eql x op))
                 x))
           (concatenate 'simple-vector
                        (vector mask flip-p summary-needed
                                op)
                        (subseq thunk 1))))
     *vm-ops*)
    (map nil #'funcall (gethash op *var-callbacks*))))

;; hash table reducer -> vm-reducer op
(defvar *reducer-vm-reducer*)

(defun compile-reducer (reducer)
  (let ((cache (gethash reducer *reducer-vm-reducer*)))
    (when cache
      (return-from compile-reducer cache)))
  (let ((rvar (bsp.vm:make-rvar (length *vm-vars*)
                                (reducer-eltype reducer)
                                (reducer-initial-element reducer)
                                (reducer-2arg-reducer reducer)
                                (reducer-1arg-reducer reducer)))
        (mask (reducer-mask reducer))
        (flip-p nil))
    (vector-push-extend rvar *vm-vars*)
    (when (consp mask)
      (setf mask   (second mask)
            flip-p t))
    (unless mask
      (setf mask *ones-var*))
    (vector-push-extend
     (bsp.vm:make-op
      (reducer-vector-reducer reducer)
      (map 'simple-vector
           (lambda (x)
             (if (typep x 'bsp.front:bsp-vector)
                 (var-vm-var x)
                 x))
           (concatenate 'simple-vector
                        (vector mask flip-p nil rvar)
                        (reducer-args reducer))))
     *vm-ops*)
    (setf (gethash reducer *reducer-vm-reducer*) rvar)))

(defun sequence-hashset (seq &optional (test #'eql))
  (let ((table (make-hash-table :test test)))
    (map nil (lambda (x)
               (setf (gethash x table) t))
         seq)
    table))

(defun to-bblock (count chunk-size roots reducers ops)
  (declare (type index count chunk-size)
           (type (simple-array bsp.front:bsp-vector 1) roots ops)
           (type (simple-array reducer 1) reducers))
  (let ((*live-vars*  (make-hash-table))
        (*mask-vars*  (make-hash-table))
        (*roots*      (sequence-hashset roots))
        (*var-callbacks* (make-hash-table))
        (*vm-vars*    (make-array 32 :adjustable t :fill-pointer 0))
        (*vm-ops*     (make-array 32 :adjustable t :fill-pointer 0))
        (*var-vm-var* (make-hash-table))
        (*ones-var*   (bsp.vm:make-cvar 0
                                        '(unsigned-byte 32)
                                        (ldb (byte 32 0) -1)))
        (*summarised-vm-vars* (make-hash-table))
        (*reducer-vm-reducer* (make-hash-table)))
    (map nil #'mark-live roots)
    (map nil #'mark-live reducers)
    (map nil (lambda (reducer)
               (mark-reducer-args reducer ops))
         reducers)
    (vector-push-extend *ones-var* *vm-vars*)
    (let ((count 0))
      (map nil (lambda (op)
                 (when (gethash op *live-vars*)
                   (incf count)
                   (compile-op op)))
           ops)
      (assert (= count (hash-table-count *live-vars*))))
    (map nil #'compile-reducer reducers)
    (bsp.vm:make-bblock count chunk-size
                        (coerce *vm-vars* 'simple-vector)
                        (coerce *vm-ops*  'simple-vector)
                        (map 'simple-vector
                             (lambda (x)
                               (or (gethash x *reducer-vm-reducer*)
                                   (error "WTF?")))
                             reducers))))
