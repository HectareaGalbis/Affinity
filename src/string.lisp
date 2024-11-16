
(in-package #:affinity)


(defun foreign-string-alloc (str &key encoding null-terminated-p start end)
  ;; Por ahora es lo mismo, hasta que meta los owners.
  (cffi:foreign-string-alloc str :encoding encoding
                                 :null-terminated-p null-terminated-p
                                 :start start
                                 :end end))

;; string-ptr
(cffi:define-foreign-type string-ptr-type ()
  ((encoding :initarg :encoding)))

(cffi:define-parse-method string-ptr (&optional (encoding :utf-8))
  (make-instance 'string-ptr-type :encoding encoding :actual-type :pointer))

(defmethod cffi:translate-to-foreign ((str string) (obj-type string-ptr-type))
  (foreign-string-alloc str :encoding (slot-value obj-type 'encoding)))

(defmethod cffi:translate-from-foreign (pointer (obj-type string-ptr-type))
  (cffi:foreign-string-to-lisp pointer :encoding (slot-value obj-type 'encoding)))


;; string-array
(cffi:define-foreign-type string-array-type ()
  ((size :initarg :size)
   (encoding :initarg :encoding)))

(cffi:define-parse-method string-array (size &optional encoding)
  (make-instance 'string-array-tyepe :size size :encoding encoding :actual-type `(:array :char ,size)))

(defmethod cffi:translate-to-foreign ((str string) (obj-type string-ptr-type))
  (foreign-string-alloc str :encoding (slot-value obj-type 'encoding)))

(defmethod cffi:translate-from-foreign (pointer (obj-type string-ptr-type))
  (cffi:foreign-string-to-lisp pointer :encoding (slot-value obj-type 'encoding)))

(defmethod cffi:translate-into-foreign-memory ((str string) (obj-type string-array-type) pointer)
  (with-slots (size encoding) obj-type
    (cffi:lisp-string-to-foreign str pointer size :encoding encoding)))

(defmethod cffi::translate-aggregate-to-foreign (pointer (str string) (obj-type string-array-type))
  (cffi:translate-into-foreign-memory str obj-type pointer))
