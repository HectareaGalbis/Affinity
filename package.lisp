

(defpackage "more-cffi"
  (:use :cl)
  (:nicknames :mcffi)
  (:export #:def-foreign-accessors
	   #:def-foreign-constructor-destructor))
