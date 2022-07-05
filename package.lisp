

(defpackage "more-cffi"
  (:use :cl :alexandria :iterate)
  (:nicknames :mcffi)
  (:export #:*enable-doc-generation*
	   #:doc-header
	   #:doc-subheader
	   #:doc-subsubheader
	   #:doc-note
	   #:defwith
	   #:def-foreign-function
	   #:def-foreign-struct))
