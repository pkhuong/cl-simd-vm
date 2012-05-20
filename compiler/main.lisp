(in-package "BSP.COMPILER")

(defun to-bblock (count chunk-size roots ops reducers)
  (declare (type index count chunk-size))
  (let ((reducers (concatenate 'simple-vector
                               reducers
                               (remove-if-not (lambda (x)
                                                (typep x 'reducer))
                                              roots)))
        (roots (coerce
                (remove-if-not (lambda (x)
                                 (typep x 'vec))
                               roots)
                'simple-vector))
        (*state* (make-state)))
    (assert (every #'op-p ops))
    (assert (every #'reducer-p reducers))
    (let* ((ops (setup-state roots ops reducers))
           (codegen-state
             (emit-code (coerce (remove-if-not #'livep ops)
                                'simple-vector))))
      (bsp.vm:make-bblock count chunk-size
                          (codegen-state-registers codegen-state)
                          (codegen-state-ops       codegen-state)
                          (codegen-state-reduces   codegen-state)))))
