

(defpackage "more-cffi"
  (:use :cl :alexandria :iterate)
  (:nicknames :mcffi)
  (:export #:def-foreign-accessors
	   #:def-foreign-constructor-destructor))
