
(in-package #:affinity)
(in-readtable affinity)


;; Hacer el tipo (:function ret-type args) una lente. De este forma el paso de puntero a una instancia
;; de tipo funcallable-class es posible. A la instancia de tipo funcallable-class hay que asignarle la
;, la funcion y esta solo se puede deducir en tiempo de compilacion.























(cffi:define-foreign-type function-type ()
  ((name :initarg :name)))

(cffi:define-parse-method function (name)
  (make-instance 'function-type :name name :actual-type :pointer))

(defclass function-obj ()
  ((function-ref :initarg :function-ref))
  (:metaclass c2mop:funcallable-standard-class))

(defun function-ref-to-pointer (fref)
    "Turns a function reference to a function pointer."
    (etypecase fref
      (string (cffi:foreign-symbol-pointer fref))
      (symbol (cffi:get-callback fref))
      ((satisfies cffi:pointerp) fref)))


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun make-bindings (foreign-slots)
    (mapcar #¿`(,(slot-name ?slot) ,(slot-init ?slot)) foreign-slots))

  (defun make-setters (arg-names public-slots)
    (mapcar #¿(slot-expand-setter ? ?) arg-names public-slots))
  
  (defun make-foreign-funcall (function-ref foreign-slots return-slot)
    `(cffi:foreign-funcall-pointer
      (function-ref-to-pointer ,function-ref)
      ,@(mapcan #¿(list (slot-cffi-type ?slot) (slot-name ?slot)) foreign-slots)
      ,(slot-cffi-type return-slot)))

  (defun make-result-form (return-slot foreign-funcall body)
    (let ((return-name (slot-name return-slot))
          (return-type (slot-affi-type return-slot)))
      (with-gensyms (result-sym)
        `(let* ((,result-sym ,foreign-funcall)
                (,return-name ,(expand-getter result-sym (parse-affi-type return-type))))
           (declare (ignorable ,return-name))
           ,@body))))

  (defun make-function-form (function-ref return-form args body)
    "Returns a list with the lambda list and body of a foreign function call."
    (let* ((return-slot (parse-slot return-form))
           (arg-slots (mapcar #¿(parse-slot ?) args))
           (public-slots (remove-if-not #'slot-public-p arg-slots))
           (foreign-slots (remove-if-not #'slot-foreign-p arg-slots))
           (arg-names (mapcar #¿(make-symbol (symbol-name (slot-name ?))) public-slots)))
      (multiple-value-bind (actual-body declarations docstring) (parse-body body :documentation t)
        (let* ((foreign-funcall (make-foreign-funcall function-ref foreign-slots return-slot))
               (result-form (make-result-form return-slot foreign-funcall (append declarations actual-body))))
          `(,arg-names
            ,@(when docstring `(,docstring))
            (let ,(make-bindings foreign-slots)
              ,@(make-setters arg-names public-slots)
              ,result-form)))))))



(defmacro define-function-type (name return-form &rest args)
  )



(defmacro define-c-function ((name foreign-name) return-form (&rest args) &body body)
  `(progn
     (when (symbol-function ',name)
       (warn "affi:define-c-function : Redefining %s" ',name))
     (setf (symbol-function ',name) (make-instance 'function-obj :function-ref ,foreign-name))
     (c2mop:set-funcallable-instance-function
      #',name (lambda ,@(make-function-form foreign-name return-form args body)))))

(defmacro defcfun ((name foreign-name) return-type (&rest arg-slots) &optional docstring)
  (with-gensyms (return-sym)
    `(define-c-function (,name ,foreign-name) (,return-sym ,return-type) ,arg-slots
       ,@(when docstring `(,docstring))
       ,(if (equal (canonicalize-affi-type return-type) '(:void))
            '(values)
            return-sym))))


