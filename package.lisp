

(defpackage :more-cffi
  (:use :cl :alexandria :iterate :clith)
  (:nicknames :mcffi)
  (:shadowing-import-from :clith #:with)
  (:export #:define-callback-definer
	   #:define-foreign-struct))
