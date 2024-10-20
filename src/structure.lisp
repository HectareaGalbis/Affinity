
(in-package #:affinity)
(in-readtable #:affinity)



(defclass affinity-class (standard-class) ())

(defmethod c2mop:validate-superclass ((class affinity-class) (super-class standard-class))
  t)

(defclass affinity-slot-definition ()
  ((visibility :initarg :visibility :initform :public)
   (ctype :initarg :ctype)))

(defclass affinity-direct-slot-definition
    (affinity-slot-definition c2mop:standard-direct-slot-definition)
  ())

(defclass affinity-effective-slot-definition
    (affinity-slot-definition c2mop:standard-effective-slot-definition)
  ())

(defmethod c2mop:direct-slot-definition-class ((class affinity-class) &rest initargs)
  (find-class 'affinity-direct-slot-definition))

(defmethod c2mop:effective-slot-definition-class ((class affinity-class) &rest initargs)
  (find-class 'affinity-effective-slot-definition))

(defmethod c2mop:compute-effective-slot-definition ((class affinity-class) name direct-slots)
  (let ((slot (call-next-method)))
    (setf (slot-value slot 'visibility) (slot-value (car direct-slots) 'visibility)
          (slot-value slot 'ctype) (parse-affi-type (slot-value (car direct-slots) 'ctype)))
    slot))

(defmethod c2mop:compute-slots ((class affinity-class))
  (let ((slots (call-next-method))
        (private-access-slot (make-instance 'c2cl:standard-effective-slot-definition
                                            :name 'private-access-symbol
                                            :initform (make-symbol "PRIVATE-ACCESS")
                                            :initfunction (lambda () (make-symbol "PRIVATE-ACCESS")))))
    (cons private-access-slot slots)))

(defgeneric foreign-slot-value (object slot type)
  (:documentation
   "Returns the value of a slot."))

(defmethod foreign-slot-value (object slot (object-type primitive-affi-type))
  (slot-value object slot))

(defgeneric (setf foreign-slot-value) (value object slot type)
  (:documentation
   "Sets the value of a slot."))

(defmethod (setf foreign-slot-value) (value object slot (object-type primitive-affi-type))
  (setf (slot-value object slot) value))

(defmethod c2mop:slot-value-using-class ((class affinity-class) object
                                         (slot affinity-effective-slot-definition))
  (let ((private-access-symbol (slot-value object 'private-access-symbol))
        (private-access-p (symbol-value private-access-symbol)))
    (if private-access-p
        (call-next-method)
        (ecase (slot-value slot 'visibility)
          (:public
           (progv (list private-access-symbol) '(t)
             (foreign-slot-value object (c2mop:slot-definition-name slot) (slot-value slot 'ctype))))
          (:private
           (error "The member ~s is private." (c2mop:slot-definition-name slot)))))))

(defmethod (setf c2mop:slot-value-using-class) (new-value (class affinity-class) object
                                                (slot affinity-effective-slot-definition))
  (let ((private-access-symbol (slot-value object 'private-access-symbol))
        (private-access-p (symbol-value private-access-symbol)))
    (if private-access-p
        (call-next-method)
        (ecase (slot-value slot 'visibility)
          (:public
           (progv (list private-access-symbol) '(t)
             (setf (foreign-slot-value object (c2mop:slot-definition-name slot) (slot-value slot 'ctype))
                   new-value)))
          (:private
           (error "The member ~s is private." (c2mop:slot-definition-name slot)))))))

;; (defclass prueba ()
;;   ((x :initarg :x)
;;    (y :visibility :private :hola 4 :initarg :y)
;;    (z :visibility :public :initarg :z))
;;   (:metaclass affinity-class))

;; (defvar *mi-prueba* (make-instance 'prueba :x 1 :y 2 :z 3))




(eval-when (:compile-toplevel :load-toplevel :execute)

  (defstruct slot-info
    name
    type
    lens
    visible)

  (defun process-slot (slot)
    (let ((name (car slot))
          (type (cadr slot))
          (lens (getf (cddr slot) :lens))
          (visible (getf (cddr slot)) :visible))
      (make-slot-info :name name :type type :lens lens :visible visible)))
  
  (defun process-slots (slots)
    (mapcar #'process-slot slots))

  (defun make-c-slot (slot-info)
    (with-slots (name type) c-slot
      `(,name ,(affi-to-cffi type))))
  
  (defun make-c-slots (slot-infos)
    (loop for slot-info in slot-infos
          collect (make-c-slot slot-info)))

  (defun make-defcstruct (name class-name slot-infos)
    `(cffi:defcstruct (,name :class ,class-name)
       ,@(make-c-slots slot-infos)))

  (defun make-struct-slot (slot-info)
    (slot-value slot-info 'name))
  
  (defun make-struct-slots (slot-infos)
    (loop for slot-info in slot-infos
          if (slot-value slot-info 'visible)
            collect (make-struct-slot slot-info)))

  (defun make-defstruct (name slot-infos)
    `(defstruct ,name
       ,@(make-struct-slots slot-infos)))

  (defun make-pointer-getter (class-name slot-info)
    (with-slots (name lens) slot-info
      (let ((getter-name (symbolicate class-name "-" name)))
        (with-gensyms (ptr name-sym slot-sym)
          (let ((getter-body (if lens
                                 (exp:expand 'lens-getter `(,(car lens) ',name ,@(cdr lens)))
                                 `(foreign-slot-value ,ptr ',name))))
            `(defmethod get-structure-value (,ptr (,name-sym (eql ',class-name)) (,slot-sym (eql ',name)))
               (declare (ignore ,name-sym ,slot-sym))
               ,getter-body))))))

  (defun make-pointer-getters (class-name slot-infos)
    (loop for slot-info in slot-infos
          if (slot-value slot-info 'visible)
            collect (make-pointer-getter class-name slot-info)))

  (defun make-pointer-setter (class-name slot-info)
    (with-slots (name lens) slot-info
      (let ((getter-name (symbolicate class-name "-" name)))
        (with-gensyms (new-value ptr)
          (let ((setter-body (if lens
                                 (exp:expand 'lens-setter `(,(car lens) ',name ,@(cdr lens)))
                                 `(setf (foreign-slot-value ,ptr ',name) ,new-value))))
            `(defmethod (setf get-structure-value) (,new-value ,ptr (,name-sym (eql ',class-name)) (,slot-sym (eql ',name)))
               (declare (ignore ,name-sym ,slot-sym))
               ,setter-body))))))

  (defun make-pointer-setters (class-name slot-infos)
    (loop for slot-info in slot-infos
          if (slot-value slot-info 'visible)
            collect (make-pointer-setter class-name slot-info)))

  (defun make-from-foreign-setters (object-sym pointer-sym slot-infos)
    (loop for slot-info in slot-infos
          if (slot-value slot-info 'visible)
            collect (with-slots (name) slot-info
                      `((slot-value ,object-sym ',name)
                        (slot-value ,pointer-sym ',name)))))
  
  (defun make-translate-from-foreign (name class-name slot-infos)
    (with-gensyms (ptr-sym type-sym object-sym)
      `(defmethod translate-from-foreign (,ptr-sym (,type-sym ,class-name))
         (let ((,object-sym (make-instance ',name))
               (,pointer-sym (make-instance 'pointer
                                            :cpointer ,ptr-sym
                                            :ctype '(:struct ,name))))
           (setf ,@(make-from-foreign-setters object-sym pointer-sym slot-infos))))))

  (defun make-into-foreign-setters (object-sym pointer-sym slot-infos)
    (loop for slot-info in slot-infos
          if (slot-value slot-info 'visible)
            collect (with-slots (name) slot-info
                      `((slot-value ,pointer-sym ',name)
                        (slot-value ,object-sym ',name)))))
  
  (defun make-translate-into-foreign (name class-name slot-infos)
    (with-gensyms (type-sym ptr-sym object-sym)
      `(defmethod translate-into-foreign-memory ((,object-sym ,name) (,type-sym ,class-name) ,ptr-sym)
         (let ((,pointer-sym (make-instance 'pointer
                                            :cpointer ,ptr-sym
                                            :ctype '(:struct ,name))))
           (setf ,@(make-into-foreign-setters object-sym pointer-sym slot-infos))))))

  (defun make-translate-pointer-into-foreign (name class-name slot-infos)
    (with-gensyms (type-sym ptr-sym object-sym i-sym obj-ptr-sym)
      `(defmethod translate-into-foreign-memory ((,object-sym pointer) (,type-sym ,class-name) ,ptr-sym)
         (with-slots ((,obj-ptr-sym cpointer)) ,object-sym
             (loop for ,i-sym from 0 below (cffi:foreign-type-size '(:struct ,name))
                   do (setf (cffi:mem-aref ,ptr-sym :char ,i-sym) (cffi:mem-aref ,obj-ptr-sym :char ,i-sym)))))))

  (defun make-translate-into-foreign-pointer (name slot-infos)
    (with-gensyms (object-sym type-sym pointer-sym)
      `(defmethod cffi:translate-to-foreign ((,object-sym ,name) (,type-sym pointer))
         (let ((,pointer-sym (foreign-alloc '(:struct ,name))))
           (setf ,@(make-into-foreign-setter object-sym pointer-sym slot-infos))))))

  (defun make-translate-aggregate-to-foreign (class-name)
    (with-gensyms (pointer-sym object-sym type-sym)
      `(defmethod cffi::translate-aggregate-to-foreign (,pointer-sym ,object-sym (,type-sym ,class-name))
         (cffi:translate-into-foreign-memory ,object-sym ,type-sym ,pointer-sym)))))

(defmacro defcstruct (name slots)
  (let ((slot-infos (process-slots slots))
        (class-name (gensym "CLASS-NAME")))
    `(progn
       ,(make-defcstruct name class-name slot-infos)
       ,(make-defstruct name slot-infos)
       ,@(make-pointer-getters name slot-infos)
       ,@(make-pointer-setters name slot-infos)
       ,(make-translate-from-foreign name class-name slot-infos)
       ,(make-translate-info-foreign name class-name slot-infos)
       ,(make-translate-pointer-into-foreign name class-name slot-infos)
       ,(make-translate-into-foreign-pointer name slot-infos)
       ,(make-translate-aggregate-to-foreign class-name))))
