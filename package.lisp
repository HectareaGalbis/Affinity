

(defpackage "more-cffi"
  (:use :cl :alexandria :iterate)
  (:nicknames :mcffi)
  (:export #:*export-symbols*
	   #:*doc-generation*
	   #:*doc-header-proc*
	   #:*doc-subheader-proc*
	   #:*doc-subsubheader-proc*
	   #:*doc-note-proc*
	   #:*doc-defwith-proc*
	   #:*doc-foreign-callback-definer-proc*
	   #:*doc-foreign-function-proc*
	   #:*doc-foreign-struct-proc*
	   #:with-doc-file
	   #:doc-header
	   #:doc-subheader
	   #:doc-subsubheader
	   #:doc-note
	   #:defwith
	   #:def-foreign-callback-definer
	   #:def-foreign-function
	   #:def-foreign-struct))
