
(in-package #:affinity)


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defstruct func-arg
    name
    public-name
    affi-type
    cffi-type
    publicp
    privatep)

  (defun parse-defcfun-arg (slot-info)
    (with-slots (name type private) slot-info
      (make-func-arg :name name
                     :public-name (gensym (symbol-name name))
                     :affi-type type
                     :cffi-type (affi-to-cffi type)
                     :publicp (slot-public-p slot-info)
                     :privatep (slot-private-p slot-info))))
  
  (defun make-defun-args (func-args)
    (loop for func-arg in func-args
          if (slot-value func-arg 'publicp)
            collect (slot-value func-arg 'public-name)))

  (defun make-private-names (func-args)
    (loop for func-arg in func-args
          if (slot-value func-arg 'privatep)
            collect (slot-value func-arg 'name)))

  (defun make-setters (func-args)
    (loop for func-arg in func-args
          if (slot-value func-arg 'publicp)
            collect (with-slots (affi-type name public-name) func-arg
                      (expand-affi-setter name public-name affi-type))))

  (defun make-foreign-args (func-args)
    (loop for func-arg in func-args
          if (slot-value func-arg 'privatep)
            append (with-slots (name cffi-type) func-arg
                     `(,cffi-type ,name))))
  
  (defun make-return (func-name ret-arg func-args body)
    (with-slots ((ret-name name) (ret-cffi-type cffi-type) (ret-affi-type affi-type)) ret-arg
      (with-gensyms (ret-sym)
        `(let ((,ret-sym (cffi:foreign-funcall ,func-name ,@(make-foreign-args func-args) ret-cffi-type)))
           (let ((,ret-name ,(expand-affi-getter ret-sym ret-affi-type)))
             ,@(if body
                   body
                   `(,ret-name))))))))


(defmacro defcfun (name-and-options return-slot (&rest arg-slots) &body body)
  (check-slot-syntax return-slot)
  (loop for arg-slot in arg-slots
        do (check-slot-syntax arg-slot))
  (let* ((arg-slot-infos (mapcar #'parse-slot arg-slots))
         (func-arg-infos (mapcar #'parse-defcfun-arg arg-slot-infos))
         (ret-arg-info (parse-defcfun-arg (parse-slot return-slot)))
         (private-names (make-private-names func-arg-infos)))
    `(defun ,(car name-and-options) ,(make-defun-args func-arg-infos)
       (let ,private-names
         (declare (ignorable ,@private-names))
         ,@(make-setters func-arg-infos)
         ,(make-return (cadr name-and-options) ret-arg-info func-arg-infos body)))))
