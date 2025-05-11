
(in-package #:affinity)


(defclass pool (owner)
  ((pointers :initform (make-hash-table))))

(defun make-pool ()
  (make-instance 'pool))

(defun poolp (pool)
  (typep pool 'pool))

(defun free-pool (pool)
  (with-slots (pointers) pool
    (loop for pointer being the hash-value of pointers using hash-key key
          do (foreign-free pointer)
             (remhash key pointers))))

(defmethod own-pointer ((owner pool) ptr)
  (setf (gethash (pointer-address ptr) owner) ptr))

(defmethod release-pointer ((owner pool) ptr)
  (remhash (pointer-address ptr) owner))

(defmethod owns-pointer-p ((owner pool) ptr)
  (and (gethash (pointer-address ptr) owner) t))
