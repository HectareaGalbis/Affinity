
(in-package #:affinity)

;; Primitive types
;; Los tipos primitivos relacionan un tipo de affi con un tipo cffi.
;; Es una relacion 1 a 1. No se define ningun tipo de conversion.

;; Por tanto:
;; Los tipos primitivos devuelven:
;;  - El tipo cffi que representan
(exp:defexpander primitive-affi-types)

(exp:defexpansion primitive-affi-types :pointer (inner-affi-type)
  `(pointer ,inner-affi-type))

(exp:defexpansion primitive-affi-types :callback (callback-type)
  (declare (ignore callback-type))
  :pointer)
;; Lo mismo para las estructuras, callbacks, enums, ...


(defun affi-to-cffi (type)
  (let ((expander (car (ensure-list type))))
    (if (exp:expansionp 'primitive-affi-types expander)
        (exp:expand 'primitive-affi-types type)
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
