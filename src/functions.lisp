
(in-package #:affinity)
(in-readtable affinity)


;; Hacer el tipo (:function ret-type args) una lente. De este forma el paso de puntero a una instancia
;; de tipo funcallable-class es posible. A la instancia de tipo funcallable-class hay que asignarle la
;, la funcion y esta solo se puede deducir en tiempo de compilacion.



(cffi:define-foreign-type function-type ()
  ((name :initarg :name)))

(cffi:define-parse-method function-instance (name)
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




(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun defcallback-function-form (name args body)
    `(defun ,name ,args
       ,@body))

  (defun defcallback-bindings (arg-names public-slots)
    (let ((getters (mapcar #¿(slot-expand-getter ?) public-slots)))
      (mapcar #¿(list ? ?) arg-names getters)))
  
  (defun defcallback-callback (name function-name return-type foreign-slots public-slots)
    (let* ((foreign-args (mapcar #¿(list (slot-name ?slot)
                                         (slot-cffi-type ?slot))
                                   foreign-slots))
           (public-args (mapcar #¿(make-symbol (symbol-name (slot-name ?))) public-slots)))
      `(cffi:defcallback ,name ,(affi-to-cffi return-type) ,foreign-args
         (let ,(defcallback-bindings public-args public-slots)
           (,function-name ,@public-args))))))

(defmacro defcallback (name return-type (&rest args) &body body)
  (let* ((arg-slots (mapcar #¿(parse-slot ?) args))
         (public-slots (remove-if-not #'slot-public-p arg-slots))
         (foreign-slots (remove-if-not #'slot-foreign-p arg-slots))
         (arg-names (mapcar #¿(slot-name ?) public-slots)))
    (with-gensyms (func-name callback-name)
      `(progn
         ,(defcallback-function-form func-name arg-names body)
         ,(defcallback-callback callback-name func-name return-type foreign-slots public-slots)
         (when (symbol-function ',name)
           (warn "affi:define-c-function : Redefining %s" ',name))
         (setf (symbol-function ',name) (make-instance 'function-obj :function-ref ',callback-name))
         (c2mop:set-funcallable-instance-function #',name #',func-name)))))

(defmacro define-callback-definer (name return-type (&rest args))
  (check-type name symbol)
  (check-affi-type return-type)
  (mapcar #'check-slot-syntax args)
  (let* ((arg-slots (mapcar #¿(parse-slot ?) args))
         (public-slots (remove-if-not #'slot-public-p arg-slots))
         (arg-names (mapcar #¿(make-symbol (symbol-name (slot-name ?))) public-slots)))
    (with-gensyms (func-name callback-name body)
      `(defmacro ,name (,callback-name ,arg-names &body ,body)
         (check-type ,callback-name symbol)
         (mapcar #¿(check-type ? symbol) ',arg-names)
         `(progn
            (defun ,',func-name ,(list ,@arg-names)
              ,@,body)
            (defcallback ,,callback-name ,',return-type ,',args
              (,',func-name ,@(mapcar #¿(car ?) ',args))))))))


;; (define-callback-definer define-super-callback :int ((a :float) (b :float)))

;; (define-super-callback hey (a b)
;;   (+ a b))
