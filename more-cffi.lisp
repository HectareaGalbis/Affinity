

(in-package :mcffi)


;; memset from C standard library (parece ser que puede dar problemas, mejor usar zero-struct)
(cffi:defcfun "memset" :pointer
  (str :pointer) (c :int) (n :size))


;; Defines a with macro named name, using a constructor and a destructor
;; The constructor can receive zero or more arguments and can return one or more values
;; The destructor must receive 'destructor-arity' arguments. These arguments are the first values the
;; constructor returns
;; The resulting macro binds some vars to the results from the constructor. These vars can be fewer than the returned values
(defmacro defwith (name create destroy &key (destructor-arity 1))
  (with-gensyms ((var "var") (var-list "var-list") (args "args") (ret-list "ret-list") (body "body"))
    `(defmacro ,name (,var ,args &body ,body)
       (with-gensyms ((,ret-list "ret-list"))
         (let ((,var-list (if (listp ,var)
                              ,var
                              (list ,var))))
           `(let ((,,ret-list (multiple-value-list (,',create ,@,args))))
              (unwind-protect
                (multiple-value-bind ,,var-list (values-list ,,ret-list)
                  ,@,body)
                (apply #',',destroy (subseq ,,ret-list 0 ,',destructor-arity)))))))))


;; Returns the list of symbols from slot-names that appear in expr
(defun find-slot-names (slot-names expr)
  (labels ((find-slot-names-aux (names l names-found)
	         (cond
                   ((and (symbolp l) (member l names)) (adjoin l names-found))
                   ((consp l) (find-slot-names-aux names (cdr l)
						   (find-slot-names-aux names (car l) names-found)))
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
	 (used-getter-slots (if (null getter) (list slot) (find-slot-names slot-names-c (cadr getter))))
	 (getter-args (if (null getter) (list arg) `(,arg ,@(car getter))))
	 (used-setter-slots (if (null setter) (list slot) (find-slot-names slot-names-c (cadr setter))))
	 (new-value-arg (if (null setter) (gensym "new-value")))
	 (setter-args (if (null setter) (list new-value-arg arg) (if (null (car setter))
								     (error "At least one parameter is needed")
								     `(,(caar setter) ,arg ,@(cdar setter))))))
    `(progn
       ,(if (or (not getter-p) (not (null getter)))
	    `(defun ,getter-name ,getter-args
               (cffi:with-foreign-slots (,used-getter-slots ,arg ,type)
	         ,(if (null getter)
	              slot
	              (cadr getter)))))
       ,(if (or (not setter-p) (not (null setter)))
	    `(defun (setf ,getter-name) ,setter-args
	       (cffi:with-foreign-slots (,used-setter-slots ,arg ,type)
	         ,(if (null setter)
		      `(setf ,slot ,new-value-arg)
		      (cadr setter))))))))


;; Defines a bunch of foreign getters and setters.
;; Each expression in slot-accessors is a symbol denoting a slot member or a list
;; (slot :getter (getter-expression nil) :setter (setter-expression nil))
;; The getter-expressions and setter expressions are explained in the above macro (def-foreign-slot-accessors)
;; If no getter-expr or setter-expr are provided, then the correspondly function is not defined.
(defmacro def-foreign-accessors (prefix type &rest slot-accessors)
  (let ((slot-names (cffi:foreign-slot-names type)))
    `(progn ,@(loop for slot-accessor in slot-accessors
		    collect (if (symbolp slot-accessor)
			        `(def-foreign-slot-accessors
				   ,(intern (concatenate 'string (string prefix) "-" (string slot-accessor)))
				   ,slot-accessor
				   ,type
				   :slot-names ,slot-names)
			        `(def-foreign-slot-accessors
				   ,(intern (concatenate 'string (string prefix) "-"
							 (string (first slot-accessor))))
				   ,(first slot-accessor)
				   ,type
				   :slot-names ,slot-names
				   ,@(cdr slot-accessor)))))))


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
	 (slot-keys  (loop for name in slot-names collect (intern (string name) "KEYWORD")))
	 (slot-name-keys (apply #'append (mapcar #'list slot-names slot-keys)))
	 (slot-args  (mapcar (lambda (x) (gensym (string x))) slot-names))
	 (slot-name-args (apply #'append (mapcar #'list slot-names slot-args)))
	 (name-with        (intern (concatenate 'string "WITH-" (string suffix))))
	 (constructor-name (intern (concatenate 'string "CREATE-" (string suffix))))
	 (destructor-name  (intern (concatenate 'string "DESTROY-" (string suffix)))))
    (iter (for descriptor in slot-descriptors)
      (if (symbolp descriptor)
	  (progn
	    (collect descriptor into used-slots)
	    (collect (getf slot-name-args descriptor) into c-slots))
	  (progn
	    (if (symbolp (car descriptor))
		(progn
		  (collect (car descriptor) into used-slots)
		  (collect (list (list (getf slot-name-keys (car descriptor))
				       (getf slot-name-args (car descriptor)))) into constructor-args))
		(progn
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
		(collect (getf slot-name-args (caar descriptor)) into c-slots))))
      (finally (let ((struct-object (gensym)))
		 (return
		   `(progn
		      (defun ,constructor-name (&key ,@constructor-args)
			(let (,@let-bindings
			      (,struct-object (cffi:foreign-alloc ',type)))
			  (memset ,struct-object 0 (cffi:foreign-type-size ',type))
			  (cffi:with-foreign-slots (,used-slots ,struct-object ,type)
			    (setf ,@(apply #'append (mapcar #'list used-slots c-slots))))
			  (values ,struct-object)))
		      (defun ,destructor-name (,struct-object)
			(cffi:with-foreign-slots (,destructor-used-slots ,struct-object ,type)
			  ,@destructor-expressions)
			(cffi:foreign-free ,struct-object))
		      (defwith ,name-with
			,constructor-name
			,destructor-name))))))))

