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
  `(simple-array (simple-array * 1) (,length)))

(deftype summaries (&optional length)
  `(simple-array fixnum (,length)))

(load-shared-object "/Users/pkhuong/mini-bsp/vm-ops.dylib")

(defmacro with-array-saps ((&rest bindings) &body body)
  (let ((gensyms (mapcar (lambda (binding)
                           (gensym (string (first binding))))
                         bindings)))
    `(let ,(loop for g in gensyms
                 for (nil array) in bindings
                 collect `(,g ,array))
       (declare ,@(loop for g in gensyms
                        for (nil nil . rest) in bindings
                        for type = (if rest
                                       (ecase (second rest)
                                         (unsigned '(unsigned-byte 32))
                                         (double   'double-float))
                                       t)
                        collect `(type (simple-array ,type 1) ,g)))
       (sb-sys:with-pinned-objects (,@gensyms)
         (let ,(loop for (v nil . rest) in bindings
                     for  g  in gensyms
                     collect `(,v (sb-sys:sap+ (sb-sys:vector-sap ,g)
                                               ,(if rest
                                                    `(* ,(or (first  rest) 0)
                                                        ,(ecase (second rest)
                                                           (unsigned 4)
                                                           (double   8)))
                                                    0))))
           ,@body)))))

(declaim (inline summarise))
(define-alien-routine summarise int
  (size unsigned)
  (data (* unsigned)))

(defun bsp.vm-op:summarise (vectors summaries size index start)
  (declare (type vectors   vectors)
           (type summaries summaries)
           (type index     size index start))
  (with-array-saps ((vec (aref vectors index) start unsigned))
    (setf (aref summaries index)
          (summarise size vec)))
  nil)

(declaim (inline canonicalise-mask))
(define-alien-routine canonicalise-mask int
  (size unsigned)
  (mask (* unsigned))
  (summary int)
  (flipped boolean)
  (for-mask boolean)
  (dst  (* unsigned))
  (src  (* unsigned)))

(defun bsp.vm-op:canonicalise-mask (vectors summaries size
                                    mask-idx mask-start flip-p for-mask-p
                                    dst-idx dst-start src-idx src-start)
  (declare (type vectors   vectors)
           (type summaries summaries)
           (type index     size mask-idx mask-start
                           dst-idx dst-start src-idx src-start))
  (with-array-saps ((mask (aref vectors mask-idx) mask-start unsigned)
                    (dst  (aref vectors dst-idx)  dst-start  unsigned)
                    (src  (aref vectors src-idx)  src-start  unsigned))
    (setf (aref summaries dst-idx)
          (canonicalise-mask size mask (aref summaries mask-idx)
                             flip-p
                             for-mask-p
                             dst src))))

(declaim (inline merge64 merge32))
(define-alien-routine merge64 int
  (size unsigned)
  (mask (* unsigned))
  (summary int)
  (flipped boolean)
  (for-mask boolean)
  (dst  (* double))
  (select (* unsigned))
  (x   (* double))
  (y   (* double)))

(defun bsp.vm-op:merge-double (vectors summaries size
                               mask-idx mask-start flip-p for-mask-p
                               dst-idx dst-start
                               select-idx select-start
                               x-idx x-start
                               y-idx y-start)
  (declare (type vectors   vectors)
           (type summaries summaries)
           (type index     size mask-idx mask-start
                           dst-idx dst-start select-idx select-start
                           x-idx x-start y-idx y-start))
  (with-array-saps ((mask (aref vectors mask-idx) mask-start unsigned)
                    (select (aref vectors select-idx) select-start unsigned)
                    (dst  (aref vectors dst-idx)  dst-start  double)
                    (x    (aref vectors x-idx)  x-start  double)
                    (y    (aref vectors y-idx)  y-start  double))
    (setf (aref summaries dst-idx)
          (merge64 size mask (aref summaries mask-idx)
                   flip-p
                   for-mask-p
                   dst select x y))))

(define-alien-routine merge32 int
  (size unsigned)
  (mask (* unsigned))
  (summary int)
  (flipped boolean)
  (for-mask boolean)
  (dst  (* unsigned))
  (select (* unsigned))
  (x   (* unsigned))
  (y   (* unsigned)))

(defun bsp.vm-op:merge-unsigned (vectors summaries size
                                 mask-idx mask-start flip-p for-mask-p
                                 dst-idx dst-start
                                 select-idx select-start
                                 x-idx y-start
                                 y-idx x-start)
  (declare (type vectors   vectors)
           (type summaries summaries)
           (type index     size mask-idx mask-start
                           dst-idx dst-start select-idx select-start
                           x-idx x-start y-idx y-start))
  (with-array-saps ((mask (aref vectors mask-idx) mask-start unsigned)
                    (select (aref vectors select-idx) select-start unsigned)
                    (dst  (aref vectors dst-idx)  dst-start  unsigned)
                    (x    (aref vectors x-idx)  x-start  unsigned)
                    (y    (aref vectors y-idx)  y-start  unsigned))
    (setf (aref summaries dst-idx)
          (merge32 size mask (aref summaries mask-idx)
                   flip-p
                   for-mask-p
                   dst select x y))))

(macrolet ((def (vm-name alien-name)
             `(progn
                (declaim (inline ,alien-name))
                (define-alien-routine ,alien-name int
                  (size unsigned)
                  (mask (* unsigned))
                  (summary  int)
                  (flipped boolean)
                  (for-mask boolean)
                  (dst (* double))
                  (src (* double)))
                (defun ,vm-name (vectors summaries size
                                 mask-idx mask-start flip-p for-mask-p
                                 dst-idx dst-start src-idx src-start)
                  (declare (type vectors   vectors)
                           (type summaries summaries)
                           (type index     size mask-idx mask-start
                                 dst-idx dst-start src-idx src-start))
                  (with-array-saps ((mask (aref vectors mask-idx) mask-start unsigned)
                                    (dst  (aref vectors dst-idx)  dst-start  double)
                                    (src  (aref vectors src-idx)  src-start  double))
                    (setf (aref summaries dst-idx)
                          (,alien-name size mask (aref summaries mask-idx)
                                       flip-p
                                       for-mask-p
                                       dst src)))))))
  (def bsp.vm-op:double-neg neg-double)
  (def bsp.vm-op:double-inv inv-double)
  (def bsp.vm-op:double/+   reduce-add-double)
  (def bsp.vm-op:double/*   reduce-mul-double)
  (def bsp.vm-op:double/max reduce-max-double)
  (def bsp.vm-op:double/min reduce-min-double))

(macrolet ((def (vm-name alien-name)
             `(progn
                (declaim (inline ,alien-name))
                (define-alien-routine ,alien-name int
                  (size unsigned)
                  (mask (* unsigned))
                  (summary  int)
                  (flipped boolean)
                  (for-mask boolean)
                  (dst (* double))
                  (x (* double))
                  (y (* double)))
                (defun ,vm-name (vectors summaries size
                                 mask-idx mask-start flip-p for-mask-p
                                 dst-idx dst-start
                                 x-idx x-start y-idx y-start)
                  (declare (type vectors   vectors)
                           (type summaries summaries)
                           (type index     size mask-idx mask-start
                                 dst-idx dst-start
                                 x-idx x-start y-idx y-start))
                  (with-array-saps ((mask (aref vectors mask-idx) mask-start unsigned)
                                    (dst  (aref vectors dst-idx)  dst-start  double)
                                    (x    (aref vectors x-idx)    x-start    double)
                                    (y    (aref vectors y-idx)    y-start    double))
                    (setf (aref summaries dst-idx)
                          (,alien-name size mask (aref summaries mask-idx)
                                       flip-p
                                       for-mask-p
                                       dst x y)))))))
  (def bsp.vm-op:double+ add-double)
  (def bsp.vm-op:double- sub-double)
  (def bsp.vm-op:double* mul-double)
  (def bsp.vm-op:double/ div-double)
  (def bsp.vm-op:double-max max-double)
  (def bsp.vm-op:double-min min-double))

(macrolet ((def (vm-name alien-name)
             `(progn
                (declaim (inline ,alien-name))
                (define-alien-routine ,alien-name int
                  (size unsigned)
                  (mask (* unsigned))
                  (summary  int)
                  (flipped boolean)
                  (for-mask boolean)
                  (dst (* unsigned))
                  (x (* double))
                  (y (* double)))
                (defun ,vm-name (vectors summaries size
                                 mask-idx mask-start flip-p for-mask-p
                                 dst-idx dst-start
                                 x-idx x-start y-idx y-start)
                  (declare (type vectors   vectors)
                           (type summaries summaries)
                           (type index     size mask-idx mask-start
                                 dst-idx dst-start
                                 x-idx x-start y-idx y-start))
                  (with-array-saps ((mask (aref vectors mask-idx) mask-start unsigned)
                                    (dst  (aref vectors dst-idx)  dst-start  unsigned)
                                    (x    (aref vectors x-idx)    x-start    double)
                                    (y    (aref vectors y-idx)    y-start    double))
                    (setf (aref summaries dst-idx)
                          (,alien-name size mask (aref summaries mask-idx)
                                       flip-p
                                       for-mask-p
                                       dst x y)))))))
  (def bsp.vm-op:double=  eq-double)
  (def bsp.vm-op:double/= neq-double)
  (def bsp.vm-op:double<  lt-double)
  (def bsp.vm-op:double<= lte-double)
  (def bsp.vm-op:double>  gt-double)
  (def bsp.vm-op:double>= gte-double))

(macrolet ((def (vm-name alien-name)
             `(progn
                (declaim (inline ,alien-name))
                (define-alien-routine ,alien-name int
                  (size unsigned)
                  (mask (* unsigned))
                  (summary  int)
                  (flipped boolean)
                  (for-mask boolean)
                  (dst (* unsigned))
                  (src (* unsigned)))
                (defun ,vm-name (vectors summaries size
                                 mask-idx mask-start flip-p for-mask-p
                                 dst-idx dst-start src-idx src-start)
                  (declare (type vectors   vectors)
                           (type summaries summaries)
                           (type index     size mask-idx mask-start
                                 dst-idx dst-start src-idx src-start))
                  (with-array-saps ((mask (aref vectors mask-idx) mask-start unsigned)
                                    (dst  (aref vectors dst-idx)  dst-start  unsigned)
                                    (src  (aref vectors src-idx)  src-start  unsigned))
                    (setf (aref summaries dst-idx)
                          (,alien-name size mask (aref summaries mask-idx)
                                       flip-p
                                       for-mask-p
                                       dst src)))))))
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

(macrolet ((def (vm-name alien-name)
             `(progn
                (declaim (inline ,alien-name))
                (define-alien-routine ,alien-name int
                  (size unsigned)
                  (mask (* unsigned))
                  (summary  int)
                  (flipped boolean)
                  (for-mask boolean)
                  (dst (* unsigned))
                  (x (* unsigned))
                  (y (* unsigned)))
                (defun ,vm-name (vectors summaries size
                                 mask-idx mask-start flip-p for-mask-p
                                 dst-idx dst-start
                                 x-idx x-start y-idx y-start)
                  (declare (type vectors   vectors)
                           (type summaries summaries)
                           (type index     size mask-idx mask-start
                                 dst-idx dst-start
                                 x-idx x-start y-idx y-start))
                  (with-array-saps ((mask (aref vectors mask-idx) mask-start unsigned)
                                    (dst  (aref vectors dst-idx)  dst-start  unsigned)
                                    (x    (aref vectors x-idx)    x-start    unsigned)
                                    (y    (aref vectors y-idx)    y-start    unsigned))
                    (setf (aref summaries dst-idx)
                          (,alien-name size mask (aref summaries mask-idx)
                                       flip-p
                                       for-mask-p
                                       dst x y)))))))
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
