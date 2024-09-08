
(in-package #:affinity)
(in-readtable #:affinity)


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
