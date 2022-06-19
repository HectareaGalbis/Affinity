

(in-package :mcffi)


;; Returns the list of symbols from slot-names that appear in expr
(defun find-slot-names (slot-names expr)
  (flet ((find-slot-names-aux (names l names-found)
	   (cond
             ((and (symbolp l) (member l names)) (adjoin l names-found))
             ((consp l) (find-slot-names-aux names (cdr l) (find-slot-names-aux names (car l) names-found)))
             (t names-found))))
    (find-slot-names-aux slot-names expr nil)))


;; Substitute every ocurrence of the symbol old with the symbol neweach symbol in assoc-symbol by the
;; its associated symbol
(defun rec-substitute (l assoc-symbols)
  (cond
    ((and (symbolp l) (member l assoc-symbols)) (getf assoc-symbols l))
    ((consp l) (cons (rec-substitute (car l) assoc-symbols) (rec-substitute (cdr l) assoc-symbols)))
    (t l)))


;; Defines a getter and asetter for a foreign struct slot
;; If no getter provided, the returned value is the slot value itself
;; If no setter provided, the stored value is the value passed to setf
;; If getter is provided, this must be a list of two elements. The former must be
;; another list with additional parameters that the getter will receive. The latter
;; must be an expression using the additional paramaters and the slots from the structure.
;; The resulting value from evaluating that expression is returned.
;; If setter is provided, this must be a list of two elements. The former must be another
;; list with a new-value parameter and additional parameters if needed. The latter must
;; be an expression using this parameters and the slots from the structure. The resulting value
;; is stored in the specified slot.
(defmacro def-foreign-slot-accessors (getter-name slot type
				      &key (slot-names nil) (getter nil getter-p) (setter nil setter-p))
  (let* ((arg (gensym))
	 (slot-names-c (or slot-names (cffi:foreign-slot-names type)))
	 (used-getter-slots (find-slot-names slot-names-c (cadr getter)))
	 (getter-args (if (null getter) (list arg) `(,arg ,@(car getter))))
	 (used-setter-slots (find-slot-names slot-names-c (cadr setter)))
	 (new-value-arg (if (null setter) (gensym "new-value")))
	 (setter-args (if (null setter) (list new-value-arg arg) (if (null (car setter))
								     (error "At least one parameter is needed")
								     `(,(caar setter) ,arg ,@(cdar setter))))))
    (progn
      (if (or (not getter-p) (not (null getter)))
	  `(defun ,getter-name ,getter-args
             (cffi:with-foreign-slots (,used-getter-slots ,arg ,type)
	       ,(if (null getter)
	            slot
	            (cadr getter)))))
      (if (or (not setter-p) (not (null setter)))
	  `(defun (setf ,getter-name) ,setter-args
	     (cffi:with-foreign-slots (,used-setter-slots ,arg ,type)
	       (setf ,slot ,(if (null setter) new-value-arg (cadr setter)))))))))


;; Defines a bunch of foreign getters and setters.
;; Each expression in slot-accessors is a symbol denoting a slot member or a list
;; (slot :getter (getter-expression nil) :setter (setter-expression nil))
;; The getter-expressions and setter expressions are explained in the above macro (def-foreign-slot-accessors)
;; If no getter-expr or setter-expr are provided, then the correspondly function is not defined.
(defmacro def-foreign-accessors (prefix type &rest slot-accessors)
  (let ((slot-names (cffi:foreign-slot-names type)))
    `(progn ,@(loop for slot-accessor in slot-accessors
		    collect (if (symbol slot-accessor)
			        `(def-foreign-slot-accessor
				   ,(intern (concatenate 'string (string prefix) "-" slot-accessor))
				   ,slot-accessor
				   ,type
				   :slot-names ,slot-names)
			        `(def-foreign-slot-accessor
				   ,(intern (concatenate 'string (string prefix) "-" (first slot-accessor)))
				   ,(first slot-accessor)
				   ,type
				   :slot-names ,slot-names
				   :getter nil
				   :setter nil
				   ,@(let ((getter-expr (getf slot-accessor :getter)))
				       (if getter-expr
					   (list :getter getter-expr)))
				   ,@(let ((setter-expr (get-slot-accessor :setter)))
				       (if setter-expr
					   (list :setter setter-expr)))))))))


;; Defines a constructor, a destructor and a with macro for some struct type.
;; suffix is the suffix of the constructor, destructor and with macro. The added prefixes are
;; "create-", "destroy-", and "with-" respectively.
;; Each slot-descriptor must one of the following options:
;;  - slot-name: The constructor receives an argument with the keyword ":slot-name".
;;  - (slot-name): Same as the previous option.
;;  - ((slot-name init-value)): Same as the previous, but with an initial value init-value.
;;  - (slot-name constructor-expr): The constructor-expr is a lisp to foreign traslation for the slot
;;                                  slot-name. You can use all the slot-names used in the macro, but
;;                                  only that slot-names (The structure may have more slots than the
;;                                  used in the macro).
;;  - (slot-name constructor-expr destructor-expr): The destructor-expr should perform some operations
;;                                                  on the slot to assure that the destructor dealloc correctly
;;                                                  the structure. This expression can use all the slots from
;;                                                  the structure.
(defmacro def-foreign-constructor-destructor (suffix type &rest slot-descriptors)
  (let* ((slot-names (cffi:foreign-slot-names type))
	 (slot-keys  (loop for name in slot-names collect (intern (concatenate 'string ":" (string name)))))
	 (slot-name-keys (apply #'append (mapcar #'list slot-names slot-keys)))
	 (slot-args  (mapcar (lambda (x) (gensym (string x))) slot-names))
	 (slot-name-args (apply #'append (mapcar #'list slot-names slot-args))))
    (iter (for descriptor in slot-descriptors)
          (print descriptor)
	  (if (symbolp descriptor)
	      (progn
		(collect descriptor into used-slots)
		(collect (getf descriptor slot-name-args) into c-slots))
	      (progn
		(if (symbolp (car descriptor))
		    (progn
		      (collect (car descriptor) into used-slots)
		      (collect (list (list (getf (car descriptor) slot-name-keys)
					   (getf (car descriptor) slot-name-args))) into constructor-args))
		    (progn
		      (print (caar descriptor))		      
		      (print (list (list (getf slot-name-keys (caar descriptor))
					 (getf slot-name-args (caar descriptor))) (cadar descriptor)))
		      (collect (caar descriptor) into used-slots)
		      (collect (list (list (getf slot-name-keys (caar descriptor))
					   (getf slot-name-args (caar descriptor))) (cadar descriptor))
			into constructor-args)))
		(if (not (null (cdr descriptor)))
		  (progn
		    (let ((c-symbol (gensym)))
		      (collect (list c-symbol
				     (rec-substitute (cadr descriptor) slot-name-args)) into let-bindings)
		      (collect c-symbol into c-slots))
		    (if (not (null (cddr descriptor)))
			(progn
			  (collect (caddr descriptor) into destructor-expressions)
			  (unioning (find-slot-names slot-names (caddr descriptor))
				    into destructor-used-slots))))
		  (collect (getf (car descriptor) slot-name-args) into c-slots))))
	  (finally (let ((struct-object (gensym)))
		     `(progn
		        (defun ,(intern (concatenate 'string "create-" (string suffix)))
			    (&key ,@constructor-args)
			  (let (,@let-bindings
			        (,struct-object (alloc-vulkan-object ',type)))
			    (cffi:with-foreign-slots (,used-slots ,struct-object ,type)
			      (setf ,@(append (mapcar #'list used-slots c-slots))))
			    (values ,struct-object)))
			(defun ,(intern (concatenate 'string "destroy-" (string suffix))) (,struct-object)
			  (cffi:with-foreign-slots (,destructor-used-slots ,struct-object ,type)
			    ,@destructor-expressions)
			  (free-vulkan-object ,struct-object))
			(defwith ,(intern (concatenate 'string "with-" (string suffix)))
			    ,(intern (concatenate 'string "create-" (string suffix)))
			    ,(intern (concatenate 'string "destroy-" (string suffix))))))))))

