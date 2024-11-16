
(in-package #:affinity)
(in-readtable affinity)


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun split-docstring-body (body)
    (if (stringp (car body))
        (values (car body) (cdr body))
        (values nil body)))

  (defun make-bindings (foreign-slots)
    (mapcar #¿`(,(slot-name ?slot) ,(slot-init ?slot)) foreign-slots))

  (defun make-setters (arg-names public-slots)
    (mapcar #¿(slot-expand-setter ? ?) arg-names public-slots))
  
  (defun make-foreign-funcall (foreign-name foreign-slots return-slot)
    `(cffi:foreign-funcall
      ,foreign-name
      ,@(mapcan #¿(list (slot-cffi-type ?slot) (slot-name ?slot)) foreign-slots)
      ,(slot-cffi-type return-slot)))

  (defun make-result-form (return-slot foreign-funcall body)
    (let ((return-name (slot-name return-slot))
          (return-type (slot-affi-type return-slot)))
      (with-gensyms (result-sym)
        `(let* ((,result-sym ,foreign-funcall)
                (,return-name ,(expand-getter result-sym (parse-affi-type return-type))))
           (declare (ignorable ,return-name))
           ,@body)))))

(defmacro define-c-function ((name foreign-name) return-form (&rest args) &body docstring-body)
  (let* ((return-slot (parse-slot return-form))
         (arg-slots (mapcar #¿(parse-slot ?) args))
         (public-slots (remove-if-not #'slot-public-p arg-slots))
         (foreign-slots (remove-if-not #'slot-foreign-p arg-slots))
         (arg-names (mapcar #¿(make-symbol (symbol-name (slot-name ?))) public-slots)))
    (multiple-value-bind (docstring body) (split-docstring-body docstring-body)
      (let* ((foreign-funcall (make-foreign-funcall foreign-name foreign-slots return-slot))
             (result-form (make-result-form return-slot foreign-funcall body)))
        `(defun ,name ,arg-names
           ,@(when docstring `(,docstring))
           (let ,(make-bindings foreign-slots)
             ,@(make-setters arg-names public-slots)
             ,result-form))))))

(defmacro defcfun ((name foreign-name) return-type (&rest arg-slots) &optional docstring)
  (with-gensyms (return-sym)
    `(define-c-function (,name ,foreign-name) (,return-sym ,return-type) ,arg-slots
       ,@(when docstring `(,docstring))
       ,(if (equal (canonicalize-affi-type return-type) '(:void))
            '(values)
            return-sym))))


