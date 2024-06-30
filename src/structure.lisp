
(in-package #:mcffi)


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun make-defclass-class-name (class-name)
    (car (ensure-list class-name)))
  
  (defun make-defclass-slot-specifier (slot-specifier)
    (with ((slot-copy (copy-list slot-specifier)))
      (remf slot-copy :count)
      (remf slot-copy :offset)
      (values slot-copy)))

  (defun make-defclass (class-name super-classes slot-specifiers class-options)
    `(defclass ,class-name ,super-classes
       ,(mapcar #'make-defclass-slot-specifier slot-specifiers)
       ,@class-options))

  (defun make-defcstruct-class-name (class-name type-class)
    (with ((defcstruct-class-name (ensure-list class-name)))
      `(,@defcstruct-class-name :class ,type-class)))

  (defun make-defcstruct-slot-specifier (slot-specifier)
    (with ((slot-name (car slot-specifier))
           (slot-options (cdr slot-specifier))
           (ctype (getf slot-options :ctype))
           (count (getf slot-options :count))
           (offset (getf slot-options :offset)))
      `(,slot-name ,ctype ,@(when count `(:count ,count)) ,@(when offset `(:offset ,offset)))))

  (defun make-defcstruct (class-name type-class slot-specifiers)
    `(cffi:defcstruct ,(make-defcstruct-class-name class-name type-class)
       ,@(mapcar #'make-defcstruct-slot-specifier slot-specifiers)))

  (defun make-translate-to-foreign (class-name type-class)
    (with-gensyms (pointer obj-type instance)
      `(defmethod cffi:translate-from-foreign (,pointer (,obj-type ,type-class))
         (declare (ignore ,obj-type))
         (let ((,instance (make-instance ',class-name)))
           ,@(let ((struct-type `(:struct ,class-name)))
               (loop for slot in (cffi:foreign-slot-names struct-type)
                     collect `(setf (slot-value ,instance ',slot)
                                    (cffi:foreign-slot-value ,pointer ',struct-type ',slot))))))))

  (defun make-translate-into-foreign-memory (class-name type-class)
    (with-gensyms (object obj-type pointer)
      `(defmethod cffi:translate-into-foreign-memory ((,object ,class-name) (,obj-type ,type-class) ,pointer)
         (declare (ignore ,obj-type))
         ,@(let ((struct-type `(:struct ,class-name)))
             (loop for slot in (cffi:foreign-slot-names struct-type)
                   collect `(setf (cffi:foreign-slot-value ,pointer ',struct-type ',slot)
                                  (slot-value ,object ',slot)))))))

  (defun make-translate-aggregate-to-foreign (class-name type-class)
    (with-gensyms (object obj-type pointer)
      `(defmethod cffi::translate-aggregate-to-foreign ((,object ,class-name) (,obj-type ,type-class) ,pointer)
         (cffi:translate-into-foreign-memory ,object ,obj-type ,pointer)))))


(defmacro defcclass (class-name (&rest superclass-names) (&rest slot-specifiers) &rest class-options)
  (with-gensyms (type-class)
    (let ((actual-class-name (make-defclass-class-name class-name)))
      `(progn

         ,(make-defclass class-name superclass-names slot-specifiers class-options)

         ,(make-defcstruct class-name type-class slot-specifiers)

         ,(make-translate-to-foreign actual-class-name type-class)

         ,(make-translate-into-foreign-memory actual-class-name type-class)

         ,(make-translate-aggregate-to-foreign actual-class-name type-class)))))
