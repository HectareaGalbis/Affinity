
(in-package :mcffi)

(adp:write-in-file #P"README.md")

(adp:header "More-cffi")

(adp:text "This project defines some macros that help you to create wrappers to foreign callbacks and structs. Also, it supports " @w("ADP" "https://github.com/Hectarea1996/adp") " to generate automatic documentation.")

(adp:table-of-functions)


(adp:subheader "Installation")

(adp:text "It is available on Quicklisp! Just eval " @c("(ql:quickload :more-cffi)") ".")

