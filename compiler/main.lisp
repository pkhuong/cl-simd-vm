(in-package "BSP.COMPILER")

(defun to-bblock (count chunk-size roots ops reducers)
  (declare (type index count chunk-size))
  (let ((roots (coerce
                (remove-if-not (lambda (x)
                                 (typep x 'vec))
                               roots)
                'simple-vector))
        (*state* (make-state)))
    (assert (every #'op-p ops))
    (assert (every #'reducer-p reducers))
    (setup-state roots ops reducers)
    (let ((codegen-state
            (emit-code (concatenate 'simple-vector
                                    (remove-if-not #'livep ops)
                                    reducers))))
      (bsp.vm:make-bblock count chunk-size
                          (codegen-state-registers codegen-state)
                          (codegen-state-ops       codegen-state)
                          (codegen-state-reduces   codegen-state)))))
