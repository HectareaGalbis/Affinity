
(in-package #:mcffi)


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun make-defclass-class-name (class-name)
    (car (ensure-list class-name)))
  
  (defun make-defclass-slot-specifier (slot-specifier)
    (with ((slot-copy (copy-list slot-specifier)))
      (remf slot-copy :count)
      (remf slot-copy :offset)
      (values slot-copy)))

  (defun make-defcstruct-class-name (class-name type-class)
    (with ((defcstruct-class-name (ensure-list class-name)))
      `(,@defcstruct-class-name :class ,type-class)))

  (defun make-defcstruct-slot-specifier (slot-specifier)
    (with ((slot-name (car slot-specifier))
           (slot-options (cdr slot-specifier))
           (ctype (getf slot-options :ctype))
           (count (getf slot-options :count))
           (offset (getf slot-options :offset)))
      `(,slot-name ,ctype ,@(when count `(:count ,count)) ,@(when offset `(:offset ,offset))))))


(defmacro defcclass (class-name (&rest superclass-names) (&rest slot-specifiers) &rest class-options)
  (with-gensyms (type-class)
    (with ((actual-class-name (make-defclass-class-name class-name)))
      `(progn

         (defclass ,actual-class-name ,superclass-names
           ,(mapcar #'make-defclass-slot-specifier slot-specifiers)
           ,@class-options)

         (cffi:defcstruct ,(make-defcstruct-class-name class-name type-class)
           ,@(mapcar #'make-defcstruct-slot-specifier slot-specifiers))

         (defmethod cffi:translate-to-foreign ((object ,actual-class-name) (obj-type ,type-class))
           )

         (defmethod cffi:translate-from-foreign (pointer (obj-type ,type-class))
           )

         (defmethod cffi:free-translated-object (pointer (obj-type ,type-class) param)
           )

         (defmethod cffi:translate-into-foreign-memory ((object list) (obj-type ,type-class) pointer)
           )

         (defmethod cffi::translate-aggregate-to-foreign ((object list) (obj-type ,type-class) pointer)
           )

         ))))
