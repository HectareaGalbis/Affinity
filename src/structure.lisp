
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
      `(,name ,type))) ;; TODO: affi-to-cffi??
  
  (defun make-c-slots (slot-infos)
    (loop for slot-info in slot-infos
          collect (make-c-slot slot-info)))

  (defun make-defcstruct (name slot-infos)
    `(cffi:defcstruct ,name
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
            collect (make-pointer-setter class-name slot-info))))

(defmacro defcstruct (name slots)
  (let ((slot-infos (process-slots slots)))
    `(progn
       ,(make-defcstruct name slot-infos)
       ,@(make-pointer-getters name slot-infos)
       ,@(make-pointer-setters name slot-infos))))
