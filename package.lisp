(defpackage "BSP"
  (:use)
  (:export "VECTOR" "VALUE" "LET" "WITH-CONTEXT" "N"
           "BARRIER"
           "IF" "+" "-" "*" "/" "%"
           "~"
           "=" "/=" "<" ">" "<=" ">=" "AND" "OR" "XOR" "MAX" "MIN"

           "/+" "/*" "/MIN" "/MAX" "/OR" "/AND" "/XOR"
           "//+" "//*" "//MIN" "//MAX" "//OR" "//AND" "//XOR"

           "DOUBLE" "BOOL" "U32")
  (:nicknames "V"))
(in-package "BSP")

(defconstant bsp:double 'bsp:double)
(deftype bsp:double () 'double-float)

(defconstant bsp:bool 'bsp:bool)
(deftype bsp:bool () '(unsigned-byte 32))

(defconstant bsp:u32 'bsp:u32)
(deftype bsp:u32 () '(unsigned-byte 32))

(defgeneric bsp:value (x))
