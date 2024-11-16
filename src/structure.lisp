
(in-package #:affinity)
(in-readtable affinity)



(defclass affinity-class (standard-class) ())

(defmethod c2mop:validate-superclass ((class affinity-class) (super-class standard-class))
  t)

(defclass affinity-slot-definition ()
  ((private :initarg :private :initform nil)))

(defclass affinity-direct-slot-definition
    (affinity-slot-definition c2mop:standard-direct-slot-definition)
  ())

(defclass affinity-effective-slot-definition
    (affinity-slot-definition c2mop:standard-effective-slot-definition)
  ())

(defmethod c2mop:direct-slot-definition-class ((class affinity-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'affinity-direct-slot-definition))

(defmethod c2mop:effective-slot-definition-class ((class affinity-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'affinity-effective-slot-definition))

(defmethod c2mop:compute-effective-slot-definition ((class affinity-class) name direct-slots)
  (let ((slot (call-next-method)))
    (setf (slot-value slot 'private) (slot-value (car direct-slots) 'private))
    slot))

(defmethod c2mop:compute-slots ((class affinity-class))
  (let ((slots (call-next-method))
        (private-access-slot (make-instance 'c2cl:standard-effective-slot-definition
                                            :name 'private-access-symbol
                                            :initform (make-symbol "PRIVATE-ACCESS")
                                            :initfunction (lambda () (make-symbol "PRIVATE-ACCESS")))))
    (cons private-access-slot slots)))

(defgeneric object-foreign-slot-value (object slot)
  (:documentation
   "Returns the value of a slot.")
  (:method (object slot)
    (slot-value object slot)))

(defgeneric (setf object-foreign-slot-value) (value object slot)
  (:documentation
   "Sets the value of a slot.")
  (:method (value object slot)
    (setf (slot-value object slot) value)))

(defgeneric pointer-foreign-slot-value (ptr slot class-name)
  (:documentation
   "
Same as foreign-slot-value but for pointers.
The dispatch is done by class-name.
")
  (:method (ptr slot class-name)
    (declare (ignore class-name))
    (slot-value ptr slot)))

(defgeneric (setf pointer-foreign-slot-value) (value ptr slot class-name)
  (:documentation
   "
Same as (setf foreign-slot-value) but for pointers.
The dispatch is done by class-name.
")
  (:method (value ptr slot class-name)
    (declare (ignore class-name))
    (setf (slot-value ptr slot) value)))

(defmethod c2mop:slot-value-using-class ((class affinity-class) object
                                         (slot affinity-effective-slot-definition))
  (let ((private-access-symbol (slot-value object 'private-access-symbol))
        (private-access-p (symbol-value private-access-symbol)))
    (if private-access-p
        (object-foreign-slot-value object (c2mop:slot-definition-name slot))
        (if (slot-value slot 'private)
            (error "The member ~s is private." (c2mop:slot-definition-name slot))
            (progv (list private-access-symbol) '(t)
              (slot-value object (c2mop:slot-definition-name slot)))))))

(defmethod (setf c2mop:slot-value-using-class) (new-value (class affinity-class) object
                                                (slot affinity-effective-slot-definition))
  (let ((private-access-symbol (slot-value object 'private-access-symbol))
        (private-access-p (symbol-value private-access-symbol)))
    (if private-access-p
        (setf (object-foreign-slot-value object (c2mop:slot-definition-name slot)) new-value)
        (if (slot-value slot 'private)
            (error "The member ~s is private." (c2mop:slot-definition-name slot))
            (progv (list private-access-symbol) '(t)
              (setf (slot-value object (c2mop:slot-definition-name slot)) new-value))))))

(defun public-slot-name-p (class slot-name)
  (let ((effective-slot (find slot-name (c2mop:class-slots class) :key #'c2mop:slot-definition-name)))
    (and effective-slot
         (not (slot-value effective-slot 'private)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defun make-defclass (class-name slots)
    `(defclass ,class-name ()
       (,@(loop for slot in slots
                collect `(,(slot-name slot) :private ,(slot-private slot)
                                            :initform ,(slot-init slot))))
       (:metaclass affinity-class)))

  (defun make-defcstruct (class-name class-type slots)
    (let ((foreign-slots (loop for slot in slots
                               if (slot-foreign-p slot)
                                 collect `(,(slot-name slot) ,(slot-cffi-type slot)))))
      `(cffi:defcstruct (,class-name :class ,class-type)
         ,@foreign-slots)))

  (defun make-object-accessor-context (object-sym foreign-public-names body)
    `(with-slots ,foreign-public-names ,object-sym
       ,body))

  (defun make-pointer-accessor-context (ptr-sym foreign-public-names body)
    (with-gensyms (cpointer-sym type-sym)
      `(with-slots ((,cpointer-sym cpointer) (,type-sym ctype)) ,ptr-sym
         (symbol-macrolet (,@(loop for foreign-public-name in foreign-public-names
                                   collect `(,foreign-public-name
                                             (cffi:foreign-slot-value ,ptr-sym ,type-sym
                                                                      ',foreign-public-name))))
           ,body))))
  
  (defun make-accessors (class-name slots)
    (let* ((foreign-public-slots (remove-if-not #'slot-public-p slots))
           (foreign-public-names (mapcar #'slot-name foreign-public-slots)))
      (with-gensyms (object-sym slot-sym value-sym class-name-sym)
        (loop for slot in foreign-public-slots
              if (user-affi-type-p (slot-affi-type slot))
                collect `(defmethod object-foreign-slot-value
                             ((,object-sym ,class-name)
                              (,slot-sym (eql ',(slot-name slot))))
                           ,(make-object-accessor-context
                             object-sym
                             foreign-public-names
                             (expand-getter slot-sym (parse-affi-type (slot-affi-type slot)))))
                and collect `(defmethod (setf object-foreign-slot-value)
                                 (,value-sym 
                                  (,object-sym ,class-name)
                                  (,slot-sym (eql ',(slot-name slot))))
                               ,(make-object-accessor-context
                                 object-sym
                                 foreign-public-names
                                 (expand-setter value-sym slot-sym (parse-affi-type (slot-affi-type slot)))))
                and collect `(defmethod pointer-foreign-slot-value
                                 (,object-sym
                                  (,slot-sym (eql ',(slot-name slot)))
                                  (,class-name-sym ,class-name))
                               ,(make-pointer-accessor-context
                                 object-sym
                                 foreign-public-names
                                 (expand-getter slot-sym (parse-affi-type (slot-affi-type slot)))))
                and collect `(defmethod (setf pointer-foreign-slot-value)
                                 (,value-sym
                                  ,object-sym
                                  (,slot-sym (eql ',(slot-name slot)))
                                  (,class-name-sym ,class-name))
                               ,(make-pointer-accessor-context
                                 object-sym
                                 foreign-public-names
                                 (expand-setter value-sym slot-sym (parse-affi-type (slot-affi-type slot)))))))))

  (defun make-translators (class-name class-type)
    (with-gensyms (ptr-sym type-sym value-sym)
      (list

       `(defmethod cffi:translate-from-foreign (,ptr-sym (,type-sym ,class-type))
          (cstruct-from-cffi-memory ',class-name ,ptr-sym))

       `(defmethod cffi:translate-into-foreign-memory (,value-sym (,type-sym ,class-type) ,ptr-sym)
          (cstruct-into-cffi-memory ,value-sym ,ptr-sym))

       `(defmethod cffi::translate-aggregate-to-foreign (,ptr-sym ,value-sym (,type-sym ,class-type))
          (cffi:translate-into-foreign-memory ,value-sym ,type-sym ,ptr-sym))))))

(defmacro defcstruct (class-name &body object-slots)
  (check-type class-name symbol)
  (let ((slots (mapcar #¿(parse-slot ?) object-slots)))
    (with-gensyms (class-type)
      `(progn
         ,(make-defclass class-name slots)
         ,(make-defcstruct class-name class-type slots)
         ,@(make-accessors class-name slots)
         ,@(make-translators class-name class-type)))))


(defun cstruct-into-cffi-memory (cstruct ptr)
  (let* ((struct-class (class-of cstruct))
         (struct-name (class-name struct-class))
         (affi-ptr (make-instance 'pointer :cpointer ptr :ctype `(:struct ,struct-name))))
    (loop for slot in (c2mop:class-slots struct-class)
          for slot-name = (c2mop:slot-definition-name slot)
          if (not (slot-value slot 'private))
            do (setf (slot-value affi-ptr slot-name) (slot-value cstruct slot-name)))))

(defun cstruct-from-cffi-memory (struct-name ptr)
  (let* ((cstruct (make-instance struct-name))
         (struct-class (class-of cstruct))
         (affi-ptr (make-instance 'pointer :cpointer ptr :ctype `(:struct ,struct-name))))
    (loop for slot in (c2mop:class-slots struct-class)
          for slot-name = (c2mop:slot-definition-name slot)
          if (not (slot-value slot 'private))
            do (setf (slot-value cstruct slot-name) (slot-value affi-ptr slot-name)))))
