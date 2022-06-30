

(defpackage "more-cffi"
  (:use :cl :alexandria :iterate)
  (:nicknames :mcffi)
  (:export #:defwith
	   #:*enable-doc-generation*
	   #:doc-header
	   #:doc-subheader
	   #:doc-subsubheader
	   #:def-foreign-function
	   #:def-foreign-struct-functions
	   ;; #:def-foreign-accessors
	   ;; #:def-foreign-constructor-destructor
	   ))
