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

(cl:defconstant bsp:double 'bsp:double)
(cl:deftype bsp:double () 'cl:double-float)

(cl:defconstant bsp:bool 'bsp:bool)
(cl:deftype bsp:bool () '(cl:unsigned-byte 32))

(cl:defconstant bsp:u32 'bsp:u32)
(cl:deftype bsp:u32 () '(cl:unsigned-byte 32))

(cl:defgeneric bsp:value (x))
