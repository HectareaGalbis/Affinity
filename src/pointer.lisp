
(in-package #:mcffi)


(cffi:define-foreign-type pointer-type ()
  ((type :initarg :type)))

(cffi:define-parse-method pointer (type)
  (make-instance 'pointer-type :type type :actual-type :pointer))

(defclass pointer ()
  ((cpointer :initarg :cpointer)
   (type :initarg :type)
   (subpointers :initarg :subpointers
                :initform nil)))

(defmethod cffi:translate-to-foreign ((object pointer) (obj-type pointer-type))
  (declare (ignore obj-type))
  (with ((cpointer (slots object)))
    (values cpointer)))

(defmethod cffi:translate-from-foreign (pointer (obj-type pointer-type))
  (with ((type (slots obj-type)))
    (make-instance 'pointer :cpointer pointer :type type)))

(defmethod cffi:free-translated-object (pointer (obj-type pointer-type) param)
  (declare (ignore param))
  (values))


;; ------ public functions ------

(defvar *owner* nil
  "The pointer that will own the next allocated memory")

(defun foreign-free (ptr)
  (check-type ptr pointer)
  (with (((cpointer subpointers) (slots ptr)))
    (loop for subpointer in subpointers
          do (cffi:foreign-free subpointer))
    (cffi:foreign-free cpointer)))

(defun foreign-alloc (type &rest args &key initial-element initial-contents (count 1) null-terminated-p)
  (let ((cpointer (apply #'cffi:foreign-alloc type args)))
    (when *owner*
      (with ((subpointers (slots *owner*)))
        (push cpointer subpointers)))
    (make-instance 'pointer
                   :cpointer cpointer
                   :type type)))

(defun foreign-symbol-pointer (foreign-name type &rest args &key library)
  (let ((cpointer (apply #'cffi:foreign-symbol-pointer foreign-name args)))
    (and cpointer
         (make-instance 'pointer :cpointer cpointer :type type))))

(defun inc-pointer (ptr offset)
  (check-type ptr pointer)
  (with ((cpointer (slots ptr)))
    (cffi:inc-pointer cpointer offset)))

(defmacro incf-pointer (place &optional (offset 1))
  (with-gensyms (cpointer-sym)
    `(with-slots ((,cpointer-sym cpointer)) ,place
       (cffi:incf-pointer ,cpointer-sym ,offset))))

(defun make-pointer (address type)
  (make-instance 'pointer
                 :cpointer (cffi:make-pointer address)
                 :type type))

(defun mem-aptr (ptr &optional (index 0))
  (check-type ptr pointer)
  (with (((cpointer type) (slots ptr)))
    (cffi:mem-aptr cpointer type index)))

(defun mem-aref (ptr &optional (index 0))
  (check-type ptr pointer)
  (with (((cpointer type) (slots ptr)))
    (cffi:mem-aref cpointer type index)))

(defun (setf mem-aref) (new-value ptr &optional (index 0))
  (check-type ptr pointer)
  (with ((*owner* ptr)
         ((cpointer type) (slots ptr)))
    (setf (cffi:mem-aref cpointer type index) new-value)))

(defun mem-ref (ptr &optional offset)
  (check-type ptr pointer)
  (with (((cpointer type) (slots ptr)))
    (cffi:mem-ref cpointer type offset)))

(defun (setf mem-ref) (new-value ptr &optional offset)
  (check-type ptr pointer)
  (with ((*owner* ptr)
         ((cpointer type) (slots ptr)))
    (setf (cffi:mem-ref cpointer type offset) new-value)))

(defun null-pointer ()
  (make-instance 'pointer
                 :cpointer (cffi:null-pointer)
                 :type :void))

(defun null-pointer-p (ptr)
  (check-type ptr pointer)
  (with ((cpointer (slots ptr)))
    (cffi:null-pointer-p cpointer)))

(defun pointerp (ptr)
  (typep ptr 'pointer))

(defun pointer-address (ptr)
  (check-type ptr pointer)
  (with ((cpointer (slots ptr)))
    (cffi:pointer-address ptr)))

(defun pointer-eq (ptr1 ptr2)
  (with ((((cpointer1 cpointer)) (slots ptr1))
         (((cpointer2 cpointer)) (slots ptr2)))
    (cffi:pointer-eq cpointer1 cpointer2)))

(defmacro with-foreign-pointer ((var size &optional size-var) &body body)
  (with-gensyms (cvar)
    `(cffi:with-foreign-pointer (,cvar ,size ,size-var)
       (let ((,var (make-instance 'pointer
                                  :cpointer ,cvar
                                  :type :void)))
         ,@body))))
