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
