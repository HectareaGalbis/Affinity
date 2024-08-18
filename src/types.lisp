
(in-package #:affinity)

;; Primitive types
;; Los tipos primitivos relacionan un tipo de affi con un tipo cffi.
;; Es una relacion 1 a 1. No se define ningun tipo de conversion.

;; Por tanto:
;; Los tipos primitivos devuelven:
;;  - El tipo cffi que representan
(exp:defexpander primitive-affi-types define-primitive-affi-type primitive-affi-type-p)

(define-primitive-affi-type :pointer (inner-affi-type) `(pointer ,inner-affi-type))
;; Lo mismo para las estructuras, callbacks, enums, ...


(defun affi-to-cffi (type)
  (let ((expander (car (ensure-list type))))
    (if (primitive-affi-type-p expander)
        (exp:expand type)
        type)))

(defmacro defctype (name (&rest args) &body body)
  `(define-primitive-affi-type ,name (,@args)
     (affi-to-cffi (progn ,@body))))


;; Las lentes permiten manipular varios elementos a la vez

;; El usuario deberá definir 4 metodos para un tipo affi:
;;  - Un constructor
;;  - Un destructor
;;  - Un getter
;;  - Un setter
