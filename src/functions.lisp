
(in-package #:affinity)


(defmacro defcfun ((name foreign-name) return-type (&rest arg-slots) &optional docstring)
  (let* ((slots (mapcar #'parse-slot arg-slots))
         (public-slots (remove-if-not #'slot-public-p slots))
         (foreign-slots (remove-if-not #'slot-foreign-p slots))
         ;; (public-names (mapcar #¿(slot-name ?) public-slots))
         (foreign-names (mapcar #¿(slot-name ?) foreign-slots))
         (arg-names (mapcar #¿(make-symbol (symbol-name (slot-name ?))) public-slots)))
    (flet ((make-result-forms (foreign-funcall)
             (if (equal (canonicalize return-type) '(:void))
                 `(,foreign-funcall (values))
                 (with-gensyms (result-sym)
                   `((let ((,result-sym ,foreign-funcall))
                       ,(expand-getter result-sym (parse-affi-type return-type))))))))
      `(defun ,name ,arg-names
         ,@(when docstring
             `(,docstring))
         ,@(if foreign-names
               `((let ,foreign-names
                   (declare (ignorable ,@foreign-names))
                   ,@(mapcar #¿(slot-expand-setter ? ?) arg-names public-slots)
                   ,@(make-result-forms `(cffi:foreign-funcall
                                          ,foreign-name
                                          ,@(mapcan #¿(list (slot-cffi-type ?) ?)
                                                      foreign-slots foreign-names)
                                          ,(affi-to-cffi return-type)))))
               (make-result-forms `(cffi:foreign-funcall
                                    ,foreign-name
                                    ,(affi-to-cffi return-type))))))))

(defmacro define-c-function ((name foreign-name) return-slot (&rest arg-slots) &body body)
  (let ((return-slot-object (parse-slot return-slot))
        (public-names (mapcar #¿(slot-name ?) (remove-if-not #'slot-public-p arg-slots))))
    (with-gensyms (func-sym)
      `(progn
         (defcfun (,func-sym ,foreign-name) ,(slot-affi-type resturn-slot-object) ,@arg-slots)
         (defun ,name ,public-names
           (let ((,(slot-name return-slot-object) (,func-sym ,@public-names)))
             ,@body))))))
