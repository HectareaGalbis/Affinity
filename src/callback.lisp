
(in-package #:affinity)
(in-named-readtable #:affinity)


(exp:defexpander callback-expander define-callback-expander callback-expander-p)


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defstruct callback-argument
    name
    type
    lens
    visible)

  (defun make-callback-argument (name type &key lens (visible t))
    (make-callback-argument :name name :type type :lens lens :visible visible))

  (defun make-callback-arguments (arguments)
    (mapcar #¿(apply #'make-callback-argument ?) arguments))

  (defun make-defcstruct (class-name arguments)
    `(defcstruct ,class-name
       ,@arguments))

  (defun make-cffi-callback-arguments (arguments)
    (loop for argument in arguments
          if (callback-argument-type argument)
            collect (with-slots (name type) argument
                      `'(,name ,(affi-to-cffi type)))))

  (defun make-struct-setters (class-name arguments ptr)
    (loop for argument in arguments
          if (callback-argument-type argument)
            collect (with-slots (name type) argument
                      `'(setf (cffi:foreign-slot-value ,ptr ,class-name ',name) ,name))))
  
  (defun make-struct-getters (arguments ptr)
    (loop for argument in arguments
          for name = (callback-argument-name argument)
          for vis = (callback-argument-visible argument)
          if vis
            collect `',(slot-value ptr name)))
  
  (defun make-define-callback-expander (name class-name ret-type arguments)
    (with-gensyms (user-name user-args user-body ptr)
      (let ((user-args-syms (mapcar #¿`',(gensym) arguments)))
        `(define-callback-expander ,name (,user-name ,user-args ,user-body)
           `(cffi:defcallback ,,user-name ,',(affi-to-cffi ret-type) (,@,(make-cffi-callback-arguments arguments))
              (with-foreign-object (,',ptr '(:struct ,',class-name))
                ,@,(make-struct-setters class-name arguments ptr)
                (destructuring-bind (,@,user-args) (list ,@,(make-struct-getters arguments ptr))
                  ,@,user-body))))))))


(defmacro define-callback-type (name ret-type (&rest arguments))
  (let ((callback-arguments (make-callback-arguments arguments))
        (class-name (gensym "CLASS-NAME")))
    `(progn
       ,(make-defcstruct class-name callback-arguments)
       ,(make-define-callback-expander name class-name ret-type callback-arguments))))

(defmacro define-callback (callback-type name (&rest arguments) &body body)
  (exp:expand 'callback-expander `(,callback-type ,name ,arguments ,body)))

;; TODO: Hacer callback type
