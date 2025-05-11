
(in-package #:affi)


(defclass list-ptr ()
  ((size :initarg :size)))


(define-affi-type% :list-ptr (type size)
  (values
   `(:pointer ,type)
   (make-instance 'list-ptr :size size)))


(defmethod expand-getter (slot-name (type list-ptr))
  (with-slots (size) type
    (with-gensyms (i)
      `(loop for ,i from 0 below ,size
             collect (mem-aref slot-name ,i)))))

(defmethod expand-setter (new-value slot-name (type list-ptr))
  (with-slots (size) type
    (with-gensyms (i elem)
      `(loop for ,i from 0 below (min ,size (length ,new-value))
             for ,elem in ,new-value
             do (setf (mem-aref ,slot-name ,i) ,elem)))))
