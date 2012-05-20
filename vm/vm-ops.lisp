(defpackage "BSP.VM-OP"
  (:use)
  (:export "SUMMARISE" "CANONICALISE-MASK" "COMPLEMENT-MASK"
           "MERGE-DOUBLE" "MERGE-UNSIGNED"

           "DOUBLE-NEG" "DOUBLE-INV"
           "DOUBLE+" "DOUBLE-" "DOUBLE*" "DOUBLE/"
           "DOUBLE=" "DOUBLE/=" "DOUBLE<" "DOUBLE<=" "DOUBLE>" "DOUBLE>="
           "DOUBLE-MAX" "DOUBLE-MIN"
           
           "DOUBLE/+" "DOUBLE/*" "DOUBLE/MAX" "DOUBLE/MIN"
           
           "UNSIGNED-NEG" "UNSIGNED-COMPLEMENT"
           "UNSIGNED+" "UNSIGNED-" "UNSIGNED*" "UNSIGNED/" "UNSIGNED%"
           "UNSIGNED=" "UNSIGNED/=" "UNSIGNED<" "UNSIGNED<=" "UNSIGNED>" "UNSIGNED>="
           "UNSIGNED-AND" "UNSIGNED-OR" "UNSIGNED-XOR"
           "UNSIGNED-MAX" "UNSIGNED-MIN"
           
           "UNSIGNED/+" "UNSIGNED/*" "UNSIGNED/OR" "UNSIGNED/AND" "UNSIGNED/XOR"
           "UNSIGNED/MAX" "UNSIGNED/MIN"))

(defpackage "BSP.VM-OP.IMPL"
  (:use "CL" "SB-ALIEN"))
(in-package "BSP.VM-OP.IMPL")
(deftype index ()
  `(mod ,most-positive-fixnum))

(deftype vectors (&optional length)
  `(simple-array sb-ext:word (,length)))

(deftype summaries (&optional length)
  `(simple-array (unsigned-byte 32) (,length)))

(defmacro with-array-saps ((&rest arrays) &body body)
  `(sb-sys:with-pinned-objects (,@arrays)
     (let ,(mapcar (lambda (var)
                     `(,var (sb-sys:vector-sap ,var)))
                   arrays)
       ,@body)))

#+darwin
(load-shared-object "/Users/pkhuong/mini-bsp/primops/vm-ops.dylib")
#+linux
(load-shared-object "/home/pkhuong/mini-bsp/primops/vm-ops.so")

(declaim (inline summarise))
(define-alien-routine summarise void
  (vectors (* (* t)))
  (properties (* unsigned))
  (start unsigned-long)
  (size  unsigned)
  (index unsigned))

(defun bsp.vm-op:summarise (vectors summaries start size index)
  (declare (type vectors   vectors)
           (type summaries summaries)
           (type index     size index start)
           (optimize speed))
  (with-array-saps (vectors summaries)
    (summarise vectors summaries start size index)))

(declaim (inline canonicalise-mask))
(define-alien-routine canonicalise-mask void
  (vectors (* (* t)))
  (properties (* unsigned))
  (start unsigned-long)
  (size unsigned)
  (mask unsigned)
  (flipped boolean)
  (dst  unsigned)
  (src  unsigned))

(defun bsp.vm-op:canonicalise-mask (vectors summaries start size
                                    mask-idx flip-p
                                    dst-idx src-idx)
  (declare (type vectors   vectors)
           (type summaries summaries)
           (type index     size mask-idx
                           dst-idx src-idx)
           (optimize speed))
  (with-array-saps (vectors summaries)
    (canonicalise-mask vectors summaries start size
                       mask-idx flip-p dst-idx src-idx)))

(declaim (inline merge64 merge32))
(define-alien-routine merge64 void
  (vectors (* (* t)))
  (properties (* unsigned))
  (start unsigned-long)
  (size unsigned)
  (mask unsigned)
  (flipped boolean)
  (dst  unsigned)
  (select unsigned)
  (x  unsigned)
  (y  unsigned))

(defun bsp.vm-op:merge-double (vectors summaries start size
                               mask-idx flip-p
                               dst-idx
                               select-idx
                               x-idx
                               y-idx)
  (declare (type vectors   vectors)
           (type summaries summaries)
           (type index     size mask-idx
                           dst-idx select-idx
                           x-idx y-idx)
           (optimize speed))
  (with-array-saps (vectors summaries)
    (merge64 vectors summaries start size
             mask-idx flip-p dst-idx select-idx x-idx y-idx)))

(define-alien-routine merge32 void
  (vectors (* (* t)))
  (properties (* unsigned))
  (start unsigned-long)
  (size unsigned)
  (mask unsigned)
  (flipped boolean)
  (dst  unsigned)
  (select unsigned)
  (x  unsigned)
  (y  unsigned))

(defun bsp.vm-op:merge-unsigned (vectors summaries start size
                                 mask-idx flip-p
                                 dst-idx
                                 select-idx
                                 x-idx
                                 y-idx)
  (declare (type vectors   vectors)
           (type summaries summaries)
           (type index     size mask-idx
                 dst-idx select-idx
                 x-idx y-idx)
           (optimize speed))
  (with-array-saps (vectors summaries)
    (merge32 vectors summaries start size
             mask-idx flip-p dst-idx select-idx x-idx y-idx)))

;; unop
(macrolet ((def (vm-name alien-name)
             `(progn
                (declaim (inline ,alien-name))
                (define-alien-routine ,alien-name void
                  (vectors (* (* t)))
                  (properties (* unsigned))
                  (start unsigned-long)
                  (size unsigned)
                  (mask unsigned)
                  (flip-p boolean)
                  (dst  unsigned)
                  (src  unsigned))
                (defun ,vm-name (vectors summaries start size
                                 mask-idx flip-p
                                 dst-idx src-idx)
                  (declare (type vectors   vectors)
                           (type summaries summaries)
                           (type index     size mask-idx
                                 dst-idx src-idx)
                           #+nil(optimize speed))
                  (with-array-saps (vectors summaries)
                    (,alien-name vectors summaries
                                 start size
                                 mask-idx flip-p
                                 dst-idx src-idx))))))
  (def bsp.vm-op:double-neg neg-double)
  (def bsp.vm-op:double-inv inv-double)
  (def bsp.vm-op:double/+   reduce-add-double)
  (def bsp.vm-op:double/*   reduce-mul-double)
  (def bsp.vm-op:double/max reduce-max-double)
  (def bsp.vm-op:double/min reduce-min-double)

  (def bsp.vm-op:unsigned-neg neg-unsigned)
  (def bsp.vm-op:unsigned-complement complement-unsigned)
  (def bsp.vm-op:complement-mask complement-mask)
  (def bsp.vm-op:unsigned/+   reduce-add-unsigned)
  (def bsp.vm-op:unsigned/*   reduce-mul-unsigned)
  (def bsp.vm-op:unsigned/or  reduce-or-unsigned)
  (def bsp.vm-op:unsigned/and reduce-and-unsigned)
  (def bsp.vm-op:unsigned/xor reduce-xor-unsigned)
  (def bsp.vm-op:unsigned/max reduce-max-unsigned)
  (def bsp.vm-op:unsigned/min reduce-min-unsigned))

;; binop
(macrolet ((def (vm-name alien-name)
             `(progn
                (declaim (inline ,alien-name))
                (define-alien-routine ,alien-name void
                  (vectors (* (* t)))
                  (properties (* unsigned))
                  (start unsigned-long)
                  (size unsigned)
                  (mask unsigned)
                  (flip-p boolean)
                  (dst  unsigned)
                  (x    unsigned)
                  (y    unsigned))
                (defun ,vm-name (vectors summaries start size
                                 mask-idx flip-p
                                 dst-idx x-idx y-idx)
                  (declare (type vectors   vectors)
                           (type summaries summaries)
                           (type index     size mask-idx
                                 dst-idx x-idx y-idx)
                           #+nil(optimize speed))
                  (with-array-saps (vectors summaries)
                    (,alien-name vectors summaries
                                 start size
                                 mask-idx flip-p
                                 dst-idx x-idx y-idx))))))
  (def bsp.vm-op:double+ add-double)
  (def bsp.vm-op:double- sub-double)
  (def bsp.vm-op:double* mul-double)
  (def bsp.vm-op:double/ div-double)
  (def bsp.vm-op:double-max max-double)
  (def bsp.vm-op:double-min min-double)

  (def bsp.vm-op:double=  eq-double)
  (def bsp.vm-op:double/= neq-double)
  (def bsp.vm-op:double<  lt-double)
  (def bsp.vm-op:double<= lte-double)
  (def bsp.vm-op:double>  gt-double)
  (def bsp.vm-op:double>= gte-double)

  (def bsp.vm-op:unsigned+ add-unsigned)
  (def bsp.vm-op:unsigned- sub-unsigned)
  (def bsp.vm-op:unsigned* mul-unsigned)
  (def bsp.vm-op:unsigned/ div-unsigned)
  (def bsp.vm-op:unsigned% mod-unsigned)
  
  (def bsp.vm-op:unsigned=  eq-unsigned)
  (def bsp.vm-op:unsigned/= neq-unsigned)
  (def bsp.vm-op:unsigned<  lt-unsigned)
  (def bsp.vm-op:unsigned<= lte-unsigned)
  (def bsp.vm-op:unsigned>  gt-unsigned)
  (def bsp.vm-op:unsigned>= gte-unsigned)
  
  (def bsp.vm-op:unsigned-and and-unsigned)
  (def bsp.vm-op:unsigned-or  or-unsigned)
  (def bsp.vm-op:unsigned-xor xor-unsigned)

  (def bsp.vm-op:unsigned-max max-unsigned)
  (def bsp.vm-op:unsigned-min min-unsigned))
