

(defpackage "more-cffi"
  (:use :cl :alexandria :iterate)
  (:nicknames :mcffi)
  (:export #:defwith
	   #:def-foreign-struct-functions
	   ;; #:def-foreign-accessors
	   ;; #:def-foreign-constructor-destructor
	   ))
