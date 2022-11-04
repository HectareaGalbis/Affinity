

(defpackage :more-cffi
  (:use :cl :alexandria :iterate)
  (:nicknames :mcffi)
  (:export #:defwith
	   #:define-callback-definer
	   #:define-foreign-struct))
