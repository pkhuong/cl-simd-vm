(asdf:defsystem "simd-vm"
  :version "0.0.1"
  :licence "BSD"
  :description "SIMD-oriented vector map/reduce VM and front-end"
  :serial t
  :components ((:file "package")
               (:file "vm/vm-ops")
               (:file "vm/vm")
               (:file "compiler/package")
               (:file "compiler/data-structures")
               (:file "compiler/setup")
               (:file "compiler/emit")
               (:file "compiler/main")
               (:file "front-end/front-end")
               (:file "front-end/ops")))
