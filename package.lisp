

(defpackage :more-cffi
  (:use :cl :alexandria :iterate)
  (:nicknames :mcffi)
  (:export #:*export-symbols*
	   #:*doc-generation*
	   #:*doc-header-proc*
	   #:*doc-subheader-proc*
	   #:*doc-subsubheader-proc*
	   #:*doc-note-proc*
	   #:*doc-lisp-constant-proc*
	   #:*doc-lisp-function-proc*
	   #:*doc-lisp-macro-proc*
	   #:*doc-defwith-proc*
	   #:*doc-foreign-constant-proc*
	   #:*doc-foreign-enum-proc*
	   #:*doc-foreign-callback-definer-proc*
	   #:*doc-foreign-function-proc*
	   #:*doc-foreign-macro-proc*
	   #:*doc-foreign-struct-proc*
	   #:with-doc-file
	   #:doc-header
	   #:doc-subheader
	   #:doc-subsubheader
	   #:doc-note
	   #:memset
	   #:memcpy
	   #:def-lisp-constant
	   #:def-lisp-function
	   #:def-lisp-macro
	   #:defcfun
	   #:defwith
	   #:def-foreign-constant
	   #:def-foreign-enum
	   #:def-foreign-callback-definer
	   #:def-foreign-function
	   #:def-foreign-macro
	   #:def-foreign-struct
	   #:def-foreign-union))
