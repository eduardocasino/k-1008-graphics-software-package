.macro  U op, ad, reg
    .if  ad < 0
        op ad+$10000,reg
    .else
        op ad,reg
    .endif
.endmacro