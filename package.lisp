

(defpackage :more-cffi
  (:use :cl :alexandria :iterate)
  (:nicknames :mcffi)
  (:export #:*export-symbols*
	   #:*doc-generation*
	   #:*doc-header-proc*
	   #:*doc-subheader-proc*
	   #:*doc-subsubheader-proc*
	   #:*doc-note-proc*
	   #:*doc-defwith-proc*
	   #:*doc-foreign-constant-proc*
	   #:*doc-foreign-constant-function-proc*
	   #:*doc-foreign-enum-proc*
	   #:*doc-foreign-callback-definer-proc*
	   #:*doc-foreign-function-proc*
	   #:*doc-foreign-struct-proc*
	   #:with-doc-file
	   #:doc-header
	   #:doc-subheader
	   #:doc-subsubheader
	   #:doc-note
	   #:copy
	   #:defwith
	   #:def-foreign-constant
	   #:def-foreign-constant-function
	   #:def-foreign-enum
	   #:def-foreign-callback-definer
	   #:def-foreign-function
	   #:def-foreign-struct
	   #:def-foreign-union))
