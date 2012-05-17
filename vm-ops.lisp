(defpackage "BSP.VM-OP"
  (:use)
  (:export "SUMMARISE" "CANONICALISE-MASK" "COMPLEMENT-MASK" "MERGE"))

(defpackage "BSP.VM-OP.IMPL"
  (:use "CL"))
(in-package "BSP.VM-OP.IMPL")
(deftype index ()
  `(mod ,most-positive-fixnum))

(deftype vectors (&optional length)
  `(simple-array (simple-array * 1) (,length)))

(deftype summaries (&optional length)
  `(simple-array fixnum (,length)))

(defun bsp.vm-op:summarise (vectors summaries size index start)
  (declare (type vectors   vectors)
           (type summaries summaries)
           (type index     size index start))
  (setf (aref summaries index) 0)
  nil)

(defun bsp.vm-op:canonicalise-mask (vectors summaries size
                                    mask-idx mask-start flip-p for-mask-p
                                    dst-idx dst-start src-idx src-start)
  (error "Unimplemented"))

(defun bsp.vm-op:complement-mask (vectors summaries size
                                  mask-idx mask-start flip-p for-mask-p
                                  dst-idx dst-start src-idx src-start)
  (error "Unimplemented"))

(defun bsp.vm-op:merge (vectors summaries size
                        mask-idx mask-start flip-p for-mask-p
                        dst-idx dst-start
                        select-idx select-start
                        x-idx y-start
                        y-idx x-start)
  (error "Unimplemented"))
