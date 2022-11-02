
(in-package :mcffi)

(adp:write-in-file #P"README.md")

(adp:header "More-cffi")

(adp:text "This project defines some macros that help you to create wrappers to foreign functions, callbacks and structs. Also, it supports " @w("ADP" "https://github.com/Hectarea1996/adp") " to generate automatic documentation.")

(adp:table-of-contents)


(adp:subheader "Description")

(adp:text "Wrapping a foreign library using CFFI is difficult and the way how things must be done is not always clear. This project exports some utility macros to define such wrappers.")

(adp:table-of-functions)
