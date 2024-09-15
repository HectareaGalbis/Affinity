
(in-package #:affinity)

;; Primitive types
;; Los tipos primitivos relacionan un tipo de affi con un tipo cffi.
;; Es una relacion 1 a 1. No se define ningun tipo de conversion.

;; Por tanto:
;; Los tipos primitivos devuelven:
;;  - El tipo cffi que representan
(exp:defexpander primitive-affi-types)

(defmacro define-primitive-affi-type (name (&rest args) &body body)
  `(exp:defexpansion primitive-affi-types ,name ,args
     ,@body))

(define-primitive-affi-type :pointer (inner-affi-type)
  `(pointer ,inner-affi-type))

(define-primitive-affi-type :callback (callback-type)
  (declare (ignore callback-type))
  :pointer)
;; Lo mismo para las estructuras, callbacks, enums, ...

(defun affi-to-cffi (type)
  (let ((expander (car (ensure-list type))))
    (if (exp:expansionp 'primitive-affi-types expander)
        (exp:expand 'primitive-affi-types type)
        type)))

(defun primitive-affi-type-p (type)
  (exp:expansionp 'primitive-affi-types (car (ensure-list type))))

(defun check-primitive-affi-type (type)
  (unless (primitive-affi-type-p type)
    (error "This is not a valid primitive affi type: ~s" type)))

;; (defmacro defctype (name (&rest args) &body body)
;;   `(define-primitive-affi-type ,name (,@args)
;;      (affi-to-cffi (progn ,@body))))


;; ----------------------------------------------------------------


(exp:defexpander affi-types)
(exp:defexpander actual-affi-types)

(defmacro define-affi-type (name (&rest args) actual-type &body body)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (check-primitive-affi-type ,actual-type))
     (exp:defexpansion actual-affi-types ,name ,args
       ,actual-type)
     (exp:defexpansion affi-types ,name ,args
       ,@body)))

(defun affi-type-p (type)
  (exp:expansionp 'affi-types (car (ensure-list type))))

(defun check-affi-type (type)
  (unless (affi-type-p type)
    (error "This is not a valid affi type: ~s" type)))


(defgeneric expand-getter (slot obj-type)
  (:documentation
   "Must return a getter expression for a SLOT.")
  (:method (obj-type slot &rest args)
    (declare (ignore obj-type))
    slot))

(defgeneric expand-setter (slot value obj-type)
  (:documentation
   "Must return a setter expression for a SLOT.")
  (:method (obj-type slot &rest args)
    (declare (ignore obj-type))
    `(setf ,slot ,value)))


(defun expand-affi-getter (slot type)
  (cond
    ((primitive-affi-type-p type)
      slot)
    ((affi-type-p type)
     (let ((obj-type (exp:expand 'affi-types type)))
       (apply #'expand-getter slot obj-type)))))

(defun expand-affi-setter (slot value type)
  (cond
    ((primitive-affi-type-p type)
      `(setf ,slot ,value))
    ((affi-type-p type)
     (let ((obj-type (exp:expand 'affi-types type)))
       (apply #'expand-setter slot value obj-type)))))
