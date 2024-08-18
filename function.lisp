
(in-package #:affinity)


(exp:defexpander function-type define-function-type-expansion function-type-expansion-p)

(defmacro define-function-type (name ret-type &rest arg-types)
  `(define-function-type ,name ()
     `(,,ret-type ,@,arg-types)))

(defun function-type-p (name)
  (function-type-expansion-p name))

(defun get-function-type (name)
  (exp:expand `(,name)))
