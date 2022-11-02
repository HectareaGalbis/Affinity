

(defpackage :more-cffi
  (:use :cl :alexandria :iterate)
  (:nicknames :mcffi)
  (:export #:defwith
	   #:define-foreign-callback-definer
	   #:defcfun
	   #:define-foreign-function
	   #:define-foreign-struct))
