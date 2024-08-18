
(in-package #:affinity)
(in-readtable #:affinity)


;; TODO: get-structure-slot-names


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
      `(,name ,type)))
  
  (defun make-c-slots (slot-infos)
    (let ((c-slots (remove-if-not #¿(with-slots (type) ? type) slot-infos)))
      (loop for c-slot in c-slots
            collect (make-c-slot c-slot))))

  (defun make-getter (class-name slot-info)
    (with-slots (name lens) slot-info
      (let ((getter-name (symbolicate class-name "-" name)))
        (with-gensyms (ptr name-sym slot-sym)
          (let ((getter-body (if lens
                                 (exp:expand 'lens-getter `(,(car lens) ',name ,@(cdr lens)))
                                 `(foreign-slot-value ,ptr ',name))))
            `((defun ,getter-name (,ptr)
                ,getter-body)
              (defmethod get-structure-value (,ptr (,name-sym (eql ',class-name)) (,slot-sym (eql ',name)))
                (declare (ignore ,name-sym ,slot-sym))
                ,getter-body)))))))

  (defun make-getters (class-name slot-infos)
    (let ((public-slots (remove-if-not #¿(with-slots (visible) ? visible) slot-infos)))
      (mapcan #'make-getter class-name public-slots)))

  (defun make-setter (class-name slot-info)
    (with-slots (name lens) slot-info
      (let ((getter-name (symbolicate class-name "-" name)))
        (with-gensyms (new-value ptr)
          (let ((setter-body (if lens
                                 (exp:expand 'lens-setter `(,(car lens) ',name ,@(cdr lens)))
                                 `(setf (foreign-slot-value ,ptr ',name) ,new-value))))
            `((defun (setf ,getter-name) (,new-value ,ptr)
                ,setter-body)
              (defmethod (setf get-structure-value) (,new-value ,ptr (,name-sym (eql ',class-name)) (,slot-sym (eql ',name)))
                (declare (ignore ,name-sym ,slot-sym))
                ,setter-body)))))))

  (defun make-setters (class-name slot-infos)
    (let ((public-slots (remove-if-not #¿(with-slots (visible) ? visible) slot-infos)))
      (mapcan #¿(make-setter class-name ?) public-slots))))

(defmacro defcstruct (name slots)
    (let ((slot-infos (process-slots slots)))
      `(progn
         (cffi:defcstruct ,name
           ,@(make-c-slots slot-infos))
         ,@(make-getters name slot-infos)
         ,@(make-setters name slot-infos))))
