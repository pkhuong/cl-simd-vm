(in-package "BSP.FRONT")

(macrolet ((def (name (&optional binary-double unary-double)
                      (&optional binary-unsigned unary-unsigned))
             `(defun ,name (x ,@(and (or unary-double unary-unsigned) '(&optional)) y)
                (vectorify x x)
                (vectorify y y)
                (assert (typep x 'bsp:vector))
                (when (and x y)
                  (assert (equal (eltype-of x) (eltype-of y))))
                (let ((eltype (eltype-of x)))
                  (if y
                      (case eltype
                        ,@(and binary-double
                           `((bsp:double (make-op eltype ',binary-double x y))))
                        ,@(and binary-unsigned
                           `((bsp:u32    (make-op eltype ',binary-unsigned x y))))
                        (t (error "Don't know how to ~S two ~S vectors" ',name eltype)))
                      (case eltype
                        ,@(and unary-double
                           `((bsp:double ,(if (eql unary-double 'identity)
                                              'x
                                              `(make-op eltype ',unary-double x)))))
                        ,@(and unary-unsigned
                           `((bsp:u32 ,(if (eql unary-unsigned 'identity)
                                           'x
                                           `(make-op eltype ',unary-unsigned x)))))
                        (t (error "Don't know how to ~S a ~S vector" ',name eltype))))))))
  (def bsp:+ (bsp.vm-op:double+ identity)             (bsp.vm-op:unsigned+ identity))
  (def bsp:- (bsp.vm-op:double- bsp.vm-op:double-neg) (bsp.vm-op:unsigned- bsp.vm-op:unsigned-neg))
  (def bsp:* (bsp.vm-op:double* identity)             (bsp.vm-op:unsigned* identity))
  (def bsp:/ (bsp.vm-op:double/ bsp.vm-op:double-inv) (bsp.vm-op:unsigned/))
  (def bsp:% () (bsp.vm-op:unsigned%)))

(defun bsp:~ (x)
  (vectorify x)
  (assert (member (eltype-of x) '(bsp:bool bsp:u32)))
  (make-op (eltype-of x) 'bsp.vm-op:unsigned-complement x))

(macrolet ((def (name unsigned-op &optional double-op boolean-op (test-p t))
             `(defun ,name (x y)
                (vectorify x)
                (vectorify y)
                (assert (equal (eltype-of x) (eltype-of y)))
                (let ((eltype (eltype-of x)))
                  (case eltype
                    ,@(and unsigned-op
                       `(((bsp:u32 ,@(and boolean-op '(bsp:bool)))
                          (make-op ,(if test-p 'bsp:bool 'eltype)
                                   ',unsigned-op x y))))
                    ,@(and double-op
                       `((bsp:double
                          (make-op ,(if test-p 'bsp:bool 'eltype) ',double-op x y))))
                    (t
                     (error "Don't know how to ~S vectors of ~S" ',name eltype)))))))
  (def bsp:=  bsp.vm-op:unsigned=  bsp.vm-op:double=  t)
  (def bsp:/= bsp.vm-op:unsigned/= bsp.vm-op:double/= t)
  (def bsp:<  bsp.vm-op:unsigned<  bsp.vm-op:double<  t)
  (def bsp:<= bsp.vm-op:unsigned<= bsp.vm-op:double<= t)
  (def bsp:>  bsp.vm-op:unsigned<  bsp.vm-op:double>  t)
  (def bsp:>= bsp.vm-op:unsigned>= bsp.vm-op:double>= t)
  (def bsp:and bsp.vm-op:unsigned-and nil t)
  (def bsp:or  bsp.vm-op:unsigned-or  nil t)
  (def bsp:xor bsp.vm-op:unsigned-xor nil t)
  (def bsp:max bsp.vm-op:unsigned-max bsp.vm-op:double-max t nil)
  (def bsp:min bsp.vm-op:unsigned-min bsp.vm-op:double-min t nil))

(defclass reducer (bsp.compiler:reducer)
  (value))

(defmethod set-reducer-value ((reducer reducer) value)
  (setf (slot-value reducer 'value) value))

(defmethod bsp:value ((reducer reducer))
  (unless (slot-boundp reducer 'value)
    (%barrier *context* '() (vector reducer)))
  (slot-value reducer 'value))

(defun make-reducer (args eltype initial-element
                     vector-reducer 2arg-reducer 1arg-reducer)
  (let* ((args (coerce args 'simple-vector))
         (sources (remove-if-not (lambda (x)
                                   (typep x 'bsp:vector))
                                 args))
         (stack
           (reduce (lambda (x y)
                     (if (> (length x) (length y))
                         x y))
                   sources :key #'mask-stack-of
                   :initial-value nil)))
    (assert (every (lambda (source)
                     (has-root
                      stack
                      (mask-stack-of source)))
                   sources))
    (let* ((dst (make-instance 'bsp:vector
                               :eltype eltype
                               :initial-element initial-element
                               :mask   (car *mask-stack*)
                               :mask-stack *mask-stack*))
           (mask (car *mask-stack*))
           (flipped (consp mask))
           (mask (if flipped (second mask) mask))
           (reducer
             (bsp.compiler:make-reducer-with-class
              'reducer vector-reducer 
              dst mask (concatenate 'simple-vector
                                    (vector mask flipped dst)
                                    args)
              2arg-reducer 1arg-reducer)))
      (vector-push-extend reducer (ops-of *context*))
      reducer)))

(macrolet ((def (name op (unsigned-op unsigned-neutral &optional (boolean-op nil))
                         (&optional double-op double-neutral)
                 &aux (%name (intern (format nil "/~A" name)
                              "BSP")))
             `(progn
                (defun ,%name (x)
                  (vectorify x)
                  (let ((eltype (eltype-of x)))
                    (case eltype
                      ,@(and unsigned-op
                         `(((bsp:u32 ,@(and boolean-op '(bsp:bool)))
                            (make-reducer (vector x)
                                          eltype
                                          ,unsigned-neutral
                                          ',unsigned-op
                                          (sb-int:named-lambda unsigned-2arg
                                              (vec1 prop1 vec2 prop2 chunk-start chunk-size
                                               mask flip-p
                                               dst src)
                                            (declare (type (and fixnum (integer 4)) chunk-size)
                                                     (type (simple-array sb-ext:word 1) vec1 vec2)
                                                     (type (unsigned-byte 32) dst src)
                                                     (ignore chunk-start chunk-size prop1 prop2
                                                             mask flip-p src))
                                            (let ((dst1 (sb-sys:int-sap (aref vec1 dst)))
                                                  (dst2 (sb-sys:int-sap (aref vec2 dst))))
                                              (dotimes (i 8)
                                                (setf (sb-sys:sap-ref-32 dst1 (* i 4))
                                                      (,op (sb-sys:sap-ref-32 dst1 (* i 4))
                                                           (sb-sys:sap-ref-32 dst2 (* i 4)))))))
                                          (sb-int:named-lambda unsigned-1arg
                                              (vec prop chunk-start chunk-size
                                               mask flip-p
                                               dst src)
                                            (declare (type (and fixnum (integer 4)) chunk-size)
                                                     (type (simple-array sb-ext:word 1) vec)
                                                     (type (unsigned-byte 32) dst src)
                                                     (ignore chunk-start chunk-size prop
                                                             mask flip-p src))
                                            (let* ((src (sb-sys:int-sap (aref vec dst)))
                                                   (acc (sb-sys:sap-ref-32 src 0)))
                                              (declare (type (unsigned-byte 32) acc))
                                              (loop for i from 1 below 8 do
                                                (setf acc (ldb (byte 32 0)
                                                               (,op acc
                                                                    (sb-sys:sap-ref-32
                                                                     src (* 4 i))))))
                                              (if (eql eltype 'bsp:bool)
                                                  ,(if (eql boolean-op 'flip-sign)
                                                       `(ldb (byte 32 0) (- acc))
                                                       `(not (zerop acc)))
                                                  acc)))))))
                      ,@(and double-op
                         `((bsp:double
                            (make-reducer (vector x)
                                          eltype
                                          ,double-neutral
                                          ',double-op
                                          (sb-int:named-lambda double-2arg
                                              (vec1 prop1 vec2 prop2 chunk-start chunk-size
                                               mask flip-p
                                               dst src)
                                            (declare (type (and fixnum (integer 4)) chunk-size)
                                                     (type (simple-array sb-ext:word 1) vec1 vec2)
                                                     (type (unsigned-byte 32) dst src)
                                                     (ignore chunk-start chunk-size prop1 prop2
                                                             mask flip-p src))
                                            (let ((dst1 (sb-sys:int-sap (aref vec1 dst)))
                                                  (dst2 (sb-sys:int-sap (aref vec2 dst))))
                                              (dotimes (i 4)
                                                (setf (sb-sys:sap-ref-double dst1 (* i 8))
                                                      (,op (sb-sys:sap-ref-double dst1 (* i 8))
                                                           (sb-sys:sap-ref-double dst2 (* i 8)))))))
                                          (sb-int:named-lambda double-1arg
                                              (vec prop chunk-start chunk-size
                                               mask flip-p
                                               dst src)
                                            (declare (type (and fixnum (integer 4)) chunk-size)
                                                     (type (simple-array sb-ext:word 1) vec)
                                                     (type (unsigned-byte 32) dst src)
                                                     (ignore chunk-start chunk-size prop
                                                             mask flip-p src))
                                            (let* ((src (sb-sys:int-sap (aref vec dst)))
                                                   (acc (sb-sys:sap-ref-double src 0)))
                                              (declare (type double-float acc))
                                              (loop for i from 1 below 4 do
                                                (setf acc (,op acc (sb-sys:sap-ref-double src (* 8 i)))))
                                              acc))))))
                      (t (error "Don't know how to ~S vectors of ~S" ',name eltype)))))
                (defun ,name (x)
                  (v:value (,%name x))))))
  (def bsp:/+ + (bsp.vm-op:unsigned/+ 0 flip-sign) (bsp.vm-op:double/+ 0d0))
  (def bsp:/* * (bsp.vm-op:unsigned/* 1) (bsp.vm-op:double/* 1d0))
  (def bsp:/min min (bsp.vm-op:unsigned/min (ldb (byte 32 0) -1) t)
                 (bsp.vm-op:double/min   sb-ext:double-float-positive-infinity))
  (def bsp:/max max (bsp.vm-op:unsigned/min 0 t)
                 (bsp.vm-op:double/min   sb-ext:double-float-negative-infinity))
  (def bsp:/or  logior (bsp.vm-op:unsigned/or  0 t) ())
  (def bsp:/and logand (bsp.vm-op:unsigned/and 0 t) ())
  (def bsp:/xor logxor (bsp.vm-op:unsigned/xor 0 t) ()))
