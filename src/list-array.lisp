
(in-package #:affi)


(cffi:define-foreign-type list-array ()
  ((type :initarg :type)
   (count :initarg :count)))

(cffi:define-parse-method list-array (type count)
  (make-instance 'list-array :type type
                             :count count
                             :actual-type `(:array ,type ,count)))

(defmethod cffi:translate-to-foreign ((object list) (obj-type list-array))
  (with-slots (type count) obj-type
    (let ((pointer (cffi:foreign-alloc type :count count)))
      (cffi:translate-into-foreign-memory object obj-type pointer)
      (values pointer))))

(defmethod cffi:translate-from-foreign (pointer (obj-type list-array))
  (with-slots (type count) obj-type
    (loop for i from 0 below count
          collect (cffi:mem-aref pointer type i))))

(defmethod cffi:free-translated-object (pointer (obj-type list-array) param)
  (declare (ignore param))
  (free-into-foreign-memory obj-type pointer)
  (cffi:foreign-free pointer))

(defmethod cffi:translate-into-foreign-memory ((object list) (obj-type list-array) pointer)
  (with-slots (type count) obj-type
    (loop for i from 0 below count
          for elem in object
          do (setf (cffi:mem-aref pointer type i) elem))))

(defmethod cffi::translate-aggregate-to-foreign (pointer (object list) (obj-type list-array))
  (cffi:translate-into-foreign-memory object obj-type pointer))

(define-primitive-affi-type :list-array (type count)
  `(list-array ,type ,count))



;; Este estaria terminado (por ahora)

;; Con la definicion de pointer, se podra controlar que ocurre con las asignaciones de un elemento en concreto
;; del array.


;; NOTA
;; El tipo pointer podra tiene una lista de subpunteros de los que será dueño. Al tener que liberar el
;; puntero principal se liberarán antes los subpunteros.
