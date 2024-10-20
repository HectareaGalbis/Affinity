
(in-package #:affi)

(defvar *user-data-counter* 1)
(defvar *user-data-table* (make-hash-table))

(cffi:define-foreign-type user-data-type ()
  ((default :initarg :default)))

(cffi:define-parse-method user-data (&key default)
  (make-instance 'user-data-type :default default :actual-type :pointer))

(defmethod cffi:translate-to-foreign (object (obj-type user-data-type))
  (let ((ptr (cffi:make-pointer *user-data-counter*)))
    (setf (gethash *user-data-counter* *user-data-table*) object)
    (incf *user-data-counter*)
    (values ptr)))

(defmethod cffi:translate-from-foreign (ptr (obj-type user-data-type))
  (with-slots (default) obj-type
    (gethash (cffi:pointer-address ptr) *user-data-table* default)))
