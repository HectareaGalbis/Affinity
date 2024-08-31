
(in-package #:affinity)
(in-named-readtable affinity)

















(exp:defexpander callback-expander)


(defgeneric call-callback-pointer (callback-type ptr &rest args)
  (:documentation
   "Calls a pointer to a callback."))


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defclass callback-argument ()
    ((name :initarg :name)
     (type :initarg :type)
     (lens :initarg :lens)
     (visible :initarg :visible)))

  (defun make-callback-argument (name type &key lens (visible t))
    (make-instance 'callback-argument :name name :type type :lens lens :visible visible))

  (defun make-callback-arguments (arguments)
    (mapcar #¿(apply #'make-callback-argument ?) arguments))

  (defun make-defcstruct (class-name arguments)
    `(defcstruct ,class-name
       ,@arguments))

  (defun make-cffi-callback-arguments (arguments)
    (loop for argument in arguments
          if (slot-value argument 'type)
            collect (with-slots (name type) argument
                      `(,name ,(affi-to-cffi type)))))

  (defun make-struct-setters (class-name arguments ptr)
    (loop for argument in arguments
          if (slot-value argument 'type)
            collect (with-slots (name type) argument
                      `(setf (cffi:foreign-slot-value ,ptr '(:struct ,class-name) ',name) ,name))))
  
  (defun make-struct-getters (arguments ptr)
    (loop for argument in arguments
          for name = (slot-value argument 'name)
          for vis = (slot-value argument 'visible)
          if vis
            collect `(slot-value ,ptr ',name)))
  
  (defun make-callback-expansion (name class-name ret-type arguments)
    (with-gensyms (user-name user-args user-body ptr)
      (let ((user-args-syms (mapcar #¿`',(gensym (symbol-name (slot-value ? 'name))) arguments)))
        `(exp:defexpansion callback-expander ,name (,user-name ,user-args ,user-body)
           `(cffi:defcallback ,(exp:expand ',name (list ,user-name)) ,',(affi-to-cffi ret-type) (,@',(make-cffi-callback-arguments arguments))
              (with-foreign-object (,',ptr '(:struct ,',class-name))
                ,@',(make-struct-setters class-name arguments ptr)
                (destructuring-bind (,@,user-args) (list ,@',(make-struct-getters arguments ptr))
                  ,@,user-body)))))))

  (defun make-object-setters (object-sym arguments args-syms)
    (let* ((getters (make-struct-getters arguments object-sym))
           (args-gensyms (mapcar #¿`,(gensym (symbol-name (slot-value ? 'name))) arguments))
           (setter-pairs (mapcan #¿(list ? ?) getters args-gensyms)))
      `(destructuring-bind (,@args-gensyms) ,args-syms
         (setf ,@setter-pairs))))

  (defun make-foreign-arg-pairs (class-name arguments ptr)
    (loop for argument in arguments
          if (slot-value argument 'type)
            collect (with-slots (type) argument
                      type)
            and collect (with-slots (name) argument
                          `(cffi:foreign-slot-value ,ptr '(:struct ,class-name) ',name))))

  (defun make-foreign-funcall (ptr-sym class-name arguments ret-type object-sym)
    `(cffi:foreign-funcall-pointer ,ptr-sym () ,@(make-foreign-arg-pairs class-name arguments object-sym) ,ret-type))

  (defun make-defmethod (callback-type class-name ret-type arguments)
    (with-gensyms (callback-type-sym ptr-sym args-sym object-sym)
      `(defmethod call-callback-pointer ((,callback-type-sym (eql ',callback-type)) ,ptr-sym &rest ,args-sym)
         (with-foreign-object (,object-sym '(:struct ,class-name))
           ,(make-object-setters object-sym arguments args-sym)
           ,(make-foreign-funcall ptr-sym class-name arguments ret-type object-sym))))))


(defmacro define-callback-type (name ret-type (&rest arguments))
  (let ((callback-arguments (make-callback-arguments arguments))
        (class-name (gensym "CLASS-NAME")))
    `(progn
       ,(make-defcstruct class-name arguments)
       (exp:defexpander ,name)
       ,(make-callback-expansion name class-name ret-type callback-arguments)
       ,(make-defmethod name class-name ret-type callback-arguments))))

(defmacro define-callback (callback-type name (&rest arguments) &body body)
  (exp:expand 'callback-expander `(,callback-type ,name ,arguments ,body)))

(defmacro define-callback-definer (name callback-type)
  (with-gensyms (callback-name-sym arguments-sym body-sym callback-sym)
    `(defmacro ,name (,callback-name-sym (&rest ,arguments-sym) &body ,body-sym)
       `(progn
          (exp:defexpansion ,',callback-type ,,callback-name-sym () ',',callback-sym)
          (define-callback ,',callback-type ,,callback-name-sym (,@,arguments-sym)
            ,@,body-sym)))))


;; (define-callback-type window-func-type :boolean ((x :int) (y :float)))

;; (define-callback-definer define-window-callback window-func-type)

;; (define-window-callback my-callback (a b)
;;   (+ a b))



(cffi:define-foreign-type callback-type ()
  ((callback-type :initarg :callback-type)))

(cffi:define-parse-method callback (callback-type)
  (make-instance 'callback-type :callback-type callback-type :actual-type :pointer))

(defmethod cffi:translate-to-foreign ((object symbol) (obj-type callback-type))
  (declare (ignore obj-type))
  (with ((callback-type (slots object)))
    (cffi:get-callback (exp:expand callback-type (list object)))))

(defmethod cffi:translate-to-foreign ((object pointer) (obj-type pointer-type))
  (declare (ignore obj-type))
  (with (((cpointer ctype) (slots object)))
    (assert (eq (car ctype) :callback))
    cpointer))

(defmethod cffi:translate-from-foreign (pointer (obj-type callback-type))
  (with ((callback-type (slots obj-type)))
    (make-instance 'pointer :cpointer pointer :ctype `(:callback ,callback-type))))

(defmethod cffi:translate-from-foreign (pointer (obj-type pointer-type))
  (with ((ctype (slots obj-type)))
    (assert (eq (car ctype) :callback))
    (make-instance 'pointer :cpointer pointer :ctype ctype)))
