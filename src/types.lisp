
(in-package #:affinity)
(in-readtable affinity)



;; Alias types
(exp:defexpander alias-affi-types)

(defmacro defctype (name (&rest args) &body body)
  `(exp:defexpansion alias-affi-types ,name ,args
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun affi-type-alias-p (type)
    (exp:expansionp 'alias-affi-types (car (ensure-list type))))

  (defun canonicalize-affi-type (type)
    (let ((list-type (ensure-list type)))
      (if (affi-type-alias-p type)
          (ensure-list (exp:expand 'alias-affi-types list-type))
          (values list-type))))

  (defun affi-type-name (type)
    (car (canonicalize-affi-type type)))

  (defun affi-type-to-string (type)
    (let ((actual-type (and (affi-type-alias-p type)
                            (canonicalize-affi-type type))))
      (format nil "~s~@[ [aka: ~s]~]" type actual-type))))

;; Primitive types
;; Los tipos primitivos relacionan un tipo de affi con un tipo cffi.
;; Es una relacion 1 a 1. No se define ningun tipo de conversion.

(exp:defexpander primitive-affi-types)

(defclass primitive-affi-type ()
  ((cffi-type :initarg :cffi-type)))

(defmacro define-primitive-affi-type (name (&rest args) &body body)
  (with-gensyms (args-sym)
    `(exp:defexpansion primitive-affi-types ,name (&rest ,args-sym)
       (let ((cffi-type (destructuring-bind ,args ,args-sym
                          ,@body)))
         (make-instance 'primitive-affi-type :cffi-type cffi-type)))))

(defmacro define-basic-primitive-types (&rest types)
  `(progn
     ,@(mapcar #¿`(define-primitive-affi-type ,?type () ,?type) types)))

(define-basic-primitive-types
    :char :unsigned-char :short :unsigned-short :int :unsigned-int :long :unsigned-long :long-long
  :unsigned-long-long :uchar :ushort :uint :ulong :llong :ullong :int8 :uint8 :int16 :uint16 :int32 :uint32
  :int64 :uint64 :size :ssize :intptr :uintptr :ptrdiff :offset :float :double :long-double :void)

(define-primitive-affi-type :bool (&optional (base-type :int))
  `(:boolean ,(affi-to-cffi base-type)))

(define-primitive-affi-type :pointer (inner-affi-type)
  `(pointer ,inner-affi-type))

(define-primitive-affi-type :struct (name)
  `(:struct ,name))

(define-primitive-affi-type :string-ptr (&optional (encoding :utf-8))
  `(string-ptr encoding))

(define-primitive-affi-type :string-array (size &optional (encoding :utf-8))
  `(string-array size encoding))

(define-primitive-affi-type :callback (callback-type)
  (declare (ignore callback-type))
  :pointer)
;; Lo mismo para las estructuras, callbacks, enums, ...


(defun primitive-affi-type-p (type)
  (exp:expansionp 'primitive-affi-types (affi-type-name type)))

(defun check-primitive-affi-type (type)
  (unless (primitive-affi-type-p type)
    (error "This is not a valid primitive affi type: ~a" (affi-type-to-string type))))

(defun parse-primitive-affi-type (type)
  (check-primitive-affi-type type)
  (exp:expand 'primitive-affi-types (canonicalize-affi-type type)))

(defun primitive-affi-to-cffi (type)
  (slot-value (parse-primitive-affi-type (canonicalize-affi-type type)) 'cffi-type))

;; ----------------------------------------------------------------

(exp:defexpander user-affi-types)

(defclass user-affi-type ()
  ((primitive-affi-type :initarg :primitive-affi-type)
   (object-type :initarg :object-type)))

(defmacro define-affi-type (name (&rest args) &body body)
  (assert (not (keywordp name)) (name) "The name of an user affi type cannot be a keyword.")
  (with-gensyms (object-type primitive-type)
    `(exp:defexpansion user-affi-types ,name ,args
       (multiple-value-bind (,object-type ,primitive-type) (progn ,@body)
         (assert (or (null ,primitive-type) (primitive-affi-type-p ,primitive-type))
                 "Expected a primitive type.")
         (make-instance 'user-affi-type
                        :primitive-affi-type ,primitive-type
                        :object-type ,object-type)))))

(defun user-affi-type-p (type)
  (exp:expansionp 'user-affi-types (affi-type-name type)))

(defun check-user-affi-type (type)
  (unless (user-affi-type-p type)
    (error "This is not a valid user affi type: ~a" (affi-type-to-string type))))

(defun parse-user-affi-type (type)
  (check-user-affi-type type)
  (slot-value (exp:expand 'user-affi-types (canonicalize-affi-type type)) 'object-type))

(defun user-affi-to-primitive-affi (type)
  (check-user-affi-type type)
  (slot-value (exp:expand 'user-affi-types (canonicalize-affi-type type)) 'primitive-affi-type))

(defun user-affi-to-cffi (type)
  (check-user-affi-type type)
  (let ((primitive-affi (user-affi-to-primitive-affi type)))
    (assert primitive-affi (primitive-affi)
            "The type ~s does not have an associated primitive type." (affi-type-to-string type)))
  (primitive-affi-to-cffi primitive-affi))

;; --------------------------------------------------------------------------------

(defun affi-type-p (type)
  (or (primitive-affi-type-p type)
      (user-affi-type-p type)))

(defun check-affi-type (type)
  (unless (affi-type-p type)
    (error "This is not a valid affi type: ~s" (affi-type-to-string type))))

(defun parse-affi-type (type)
  (cond
    ((primitive-affi-type-p type)
     (parse-primitive-affi-type type))
    ((user-affi-type-p type)
     (parse-user-affi-type type))
    (t (error "This is not a valid affi type: ~s" (affi-type-to-string type)))))

(defun affi-to-primitive-affi (type)
  (cond
    ((primitive-affi-type-p type)
     type)
    ((user-affi-type-p type)
     (user-affi-to-primitive-affi type))
    (t (error "This is not a valid affi type: ~s" (affi-type-to-string type)))))

(defun affi-to-cffi (type)
  (cond
    ((primitive-affi-type-p type)
     (primitive-affi-to-cffi type))
    ((user-affi-type-p type)
     (user-affi-to-cffi type))
    (t (error "This is not a valid affi type: ~s" (affi-type-to-string type)))))

(defgeneric expand-getter (slot-name obj-type)
  (:documentation
   "Expands to a getter expression.")
  (:method (slot-name obj-type)
    (declare (ignore slot-name))
    (error "The method expand-getter is not implemented for the type ~s." (type-of obj-type))))

(defmethod expand-getter (slot-name (obj-type primitive-affi-type))
  slot-name)

(defgeneric expand-setter (new-value slot-name obj-type)
  (:documentation
   "Expands to a setter expression.")
  (:method (new-value slot-name obj-type)
    (declare (ignore slot-name new-value))
    (error "The method expand-setter is not implemented for the type ~s." (type-of obj-type))))

(defmethod expand-setter (new-value slot-name (obj-type primitive-affi-type))
  `(setf ,slot-name ,new-value))