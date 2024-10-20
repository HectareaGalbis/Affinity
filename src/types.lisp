
(in-package #:affinity)

;; Primitive types
;; Los tipos primitivos relacionan un tipo de affi con un tipo cffi.
;; Es una relacion 1 a 1. No se define ningun tipo de conversion.

(exp:defexpander primitive-affi-types)

(defclass primitive-affi-type ()
  ((cffi-type :initarg :cffi-type)))

(defmacro define-primitive-affi-type (name (&rest args) &body body)
  `(exp:defexpansion primitive-affi-types ,name ,args
     (make-instance 'primitive-affi-type :cffi-type (progn ,@body))))

(define-primitive-affi-type :pointer (inner-affi-type)
  `(pointer ,inner-affi-type))

(define-primitive-affi-type :callback (callback-type)
  (declare (ignore callback-type))
  :pointer)
;; Lo mismo para las estructuras, callbacks, enums, ...

(defun primitive-affi-type-p (type)
  (exp:expansionp 'primitive-affi-types (car (ensure-list type))))

(defun check-primitive-affi-type (type)
  (unless (primitive-affi-type-p type)
    (error "This is not a valid primitive affi type: ~s" type)))

(defun parse-primitive-affi-type (type)
  (check-primitive-affi-type type)
  (exp:expand 'primitive-affi-types type))

(defun primitive-affi-to-cffi (type)
  (slot-value (parse-primitive-affi-type type) 'cffi-type))

;; ----------------------------------------------------------------

(exp:defexpander user-affi-types)

(defclass user-affi-type ()
  ((primitive-affi-type :initarg :primitive-affi-type)
   (object-type :initarg :object-type)))

(defmacro define-affi-type (name (&rest args) &body body)
  (assert (not (keywordp name)) "The name of an user affi type cannot be a keyword.")
  (with-gensyms (objetc-type primitive-type)
    `(exp:defexpansion user-affi-types ,name ,args
       (multiple-value-bind (,object-type ,primitive-type) (progn ,@body)
         (make-instance 'user-affi-type
                        :primitive-affi-type ,primitive-type
                        :object-type ,object-type)))))

(defmacro defctype (name (&rest args) &body body)
  (with-gensyms (actual-type-sym)
    `(exp:defexpansion user-affi-types ,name ,args
       (exp:expand affi-types (progn ,@body)))))

(defun user-affi-type-p (type)
  (exp:expansionp 'user-affi-types (car (ensure-list type))))

(defun check-user-affi-type (type)
  (unless (user-affi-type-p type)
    (error "This is not a valid user affi type: ~s" type)))

(defun parse-user-affi-type (type)
  (check-user-affi-type type)
  (slot-value (exp:expand 'user-affi-types type) 'object-type))

(defun user-affi-to-primitive-affi (type)
  (check-user-affi-type type)
  (slot-value (exp:expand 'user-affi-types type) 'primitive-affi-type))

;; --------------------------------------------------------------------------------

(defun affi-type-p (type)
  (or (primitive-affi-type-p type)
      (user-affi-type-p type)))

(defun check-affi-type (type)
  (unless (affi-type-p type)
    (error "This is not a valid affi type: ~s" type)))

(defun parse-affi-type (type)
  (cond
    ((primitive-affi-type-p type)
     (parse-primitive-affi-type type))
    ((user-affi-type-p type)
     (parse-user-affi-type type))
    (t (error "This is not a valid affi type: ~s" type))))
