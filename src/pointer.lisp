
(in-package #:mcffi)
(in-readtable mcffi)


(cffi:define-foreign-type pointer-type ()
  ((type :initarg :type)))

(cffi:define-parse-method pointer (type)
  (make-instance 'pointer-type :type type :actual-type :pointer))

(defclass pointer ()
  ((cpointer :initarg :cpointer)
   (type :initarg :type)
   (subpointers :initarg :subpointers
                :initform nil)
   (owner :initarg :owner
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

(defmethod own ((owner pointer) (obj pointer))
  (with ((subpointers (slots owner)))
    (push obj subpointers)))

(defmethod disown ((owner pointer) (obj pointer))
  (with ((subpointers (slots owner)))
    (setf subpointers (delete obj subpointers :test 'eq))))

(defmethod get-owned ((owner pointer))
  (slot-value owner 'subpointers))

(defmethod get-owner ((obj pointer))
  (slot-value obj 'owner))

(defmethod set-owner ((obj pointer) owner)
  (setf (slot-value obj 'owner) owner))


;; ------ public functions ------
(defun foreign-free (ptr)
  (check-type ptr pointer)
  (with (((cpointer subpointers) (slots ptr)))
    (loop for subpointer in subpointers
          do (cffi:foreign-free subpointer))
    (cffi:foreign-free cpointer)))

(defun foreign-alloc (type &rest args &key initial-element initial-contents (count 1) null-terminated-p)
  (let* ((cpointer (apply #'cffi:foreign-alloc type args))
         (pointer-instance (make-instance 'pointer
                                          :cpointer cpointer
                                          :type type)))
    (when *owner*
      (establish-ownership *owner* pointer-instance))
    (values pointer-instance)))

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


;; struct related

(defun foreign-slot-pointer (ptr slot-name)
  (check-type ptr pointer)
  (with (((cpointer type) (slots ptr)))
    (cffi:foreign-slot-pointer cpointer type slot-name)))

(defun foreign-slot-value (ptr slot-name)
  (check-type ptr pointer)
  (with (((cpointer type) (slots ptr)))
    (cffi:foreign-slot-value cpointer type slot-name)))

(defun (setf foreign-slot-value) (new-value ptr slot-name)
  (check-type ptr pointer)
  (with (((cpointer type) (slots ptr)))
    (setf (cffi:foreign-slot-value cpointer type slot-name) new-value)))

(defmacro with-foreign-object ((var type &optional count) &body body)
  (with-gensyms (cvar ev-type)
    `(let ((,ev-type ,type))
       (cffi:with-foreign-object (,cvar ,ev-type ,count)
         (let ((,var (make-instance 'pointer
                                    :cpointer ,cvar
                                    :type ,ev-type)))
           ,@body)))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defun make-foreign-slot-binding (var ptr-sym type-sym)
    (cond
      ((symbolp var)
       `(,var (cffi:foreign-slot-value ,ptr-sym ,type-sym ',var)))
      ((listp var)
       (let ((num-elems (length var)))
         (when (not (null var))
           (let ((binding-var (car var)))
             (cond
               ((= num-elems 1)
                `(,binding-var (cffi:foreign-slot-value ,ptr-sym ,type-sym ',binding-var)))
               ((= num-elems 2)
                (let ((option (cadr var)))
                  (if (eq option :pointer)
                      `(,binding-var (cffi:foreign-slot-pointer ,ptr-sym ,type-sym ',binding-var))
                      `(,binding-var (cffi:foreign-slot-value ,ptr-sym ,type-sym ',option)))))
               ((and (= num-elems 3) (member :pointer var))
                (let ((slot (if (eq (cadr var) :pointer) (caddr var) (cadr var))))
                  `(,binding-var (cffi:foreign-slot-pointer ,ptr-sym ,type-sym ',slot))))))))))))

(defmacro with-foreign-slots ((vars ptr) &body body)
  (with-gensyms (ptr-sym type-sym)
    (let ((macrolet-bindings (mapcar #¿(make-foreign-slot-binding ? ptr-sym type-sym) (ensure-list vars))))
      `(with ((((,ptr-sym cpointer) (,type-sym type)) (slots ,ptr)))
         (symbol-macrolet ,macrolet-bindings
           ,@body)))))
