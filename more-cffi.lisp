

(in-package :mcffi)


;; memset from C standard library (parece ser que puede dar problemas, mejor usar zero-struct)
(cffi:defcfun "memset" :pointer
  (str :pointer) (c :int) (n :size))


;; Defines a with macro named name, using a constructor and a destructor
;; The constructor can receive zero or more arguments and can return one or more values
;; The destructor must receive 'destructor-arity' arguments. These arguments are the first values the
;; constructor returns
;; The resulting macro binds some vars to the results from the constructor. These vars can be fewer than the returned values
(defmacro defwith (name create destroy &key (destructor-arity 1) (destructor-arguments nil))
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
                (apply #',',destroy ,,(if destructor-arguments
					  ``(loop for index in ',',destructor-arguments
						  collect (nth index ,,ret-list))
					  ``(subseq ,,ret-list 0 ,',destructor-arity))))))))))


;; Returns the list of symbols from slot-names that appear in expr
(defun find-slot-names (slot-names expr)
  (labels ((find-slot-names-aux (names l names-found)
	         (cond
                   ((and (symbolp l) (member l names)) (adjoin l names-found))
                   ((consp l) (find-slot-names-aux names (cdr l)
						   (find-slot-names-aux names (car l) names-found)))
                   (t names-found))))
    (find-slot-names-aux slot-names expr nil)))


;; Substitute every ocurrence of each symbol in assoc-symbol by
;; its associated symbol
(defun rec-substitute (l assoc-symbols)
  (cond
    ((and (symbolp l) (member l assoc-symbols)) (getf assoc-symbols l))
    ((consp l) (cons (rec-substitute (car l) assoc-symbols) (rec-substitute (cdr l) assoc-symbols)))
    (t l)))


;; Defines a getter and a setter for a foreign struct slot
;; If no getter provided, the returned value is the slot value itself
;; If no setter provided, the stored value is the value passed to setf
;; If getter is provided, this must be a list of two elements. The former must be
;; another list with additional parameters that the getter will receive. The latter
;; must be an expression using the additional paramaters and the slots from the structure.
;; The resulting value from evaluating that expression is returned.
;; If setter is provided, this must be a list of two elements. The former must be another
;; list with a new-value parameter and additional parameters if needed. The latter must
;; be an expression using this parameters and the slots from the structure. This expression
;; should specialize the slot assignment.
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
	              `(progn
			 ,@(cdr getter))))))
       ,(if (or (not setter-p) (not (null setter)))
	    `(defun (setf ,getter-name) ,setter-args
	       (cffi:with-foreign-slots (,used-setter-slots ,arg ,type)
	         ,(if (null setter)
		      `(setf ,slot ,new-value-arg)
		      `(progn
			,@(cdr setter)))))))))


;; Defines a bunch of foreign getters and setters.
;; Each expression in slot-accessors is a symbol denoting a slot member or a list
;; (slot :getter getter-expression :setter setter-expression)
;; The getter-expressions and setter expressions are explained in the above macro (def-foreign-slot-accessors)
;; If no getter-expr or setter-expr are provided, then the correspondly function is not defined.
(defmacro def-foreign-accessors (prefix type &body slot-accessors)
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
(defun check-suffix (suffix)
  (unless (symbolp suffix)
      (error "Expected a symbol.~%Found:~%   ~S" suffix)))

(defun check-foreign-type (type)
  (unless (and (listp type)
	       (eq (car type) :struct)
	       (symbolp (cadr type)))
    (error "Expected a list (:struct type) where type is a cffi type.~%Found:~%   ~S" type)))

(defun check-slot-member (slot-member slot-names struct-type)
  (unless (member slot-member slot-names)
    (error "Expected a slot member from ~S~%Found:~%   ~S" struct-type slot-member)))

(defun check-slot-descriptors (slot-descriptors slot-names struct-type)
  (iter (for slot-descriptor in slot-descriptors)
    (if (listp slot-descriptor)
	(progn
	  (check-slot-member (car slot-descriptor) slot-names struct-type)
	  (if (not (null (cdr slot-descriptor)))
	      (iter (for rest-descriptor on (cdr slot-descriptor) by (lambda (x) (cdr (cdr x))))
		(unless (member (car rest-descriptor) '(:init-form :create :destroy))
		  (error "Expected :init-form, :create or :destroy in:~%   ~S~%Found:~%   ~S"
			 slot-descriptor (car rest-descriptor))))))
	(check-slot-member slot-descriptor slot-names struct-type))))

(defmacro def-foreign-constructor-destructor (suffix type &body slot-descriptors)
  (check-suffix suffix)
  (check-foreign-type type)
  (check-slot-descriptors slot-descriptors (cffi:foreign-slot-names type) type)
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
	    (collect (list (list (getf slot-name-keys descriptor)
				 (getf slot-name-args descriptor))
			   0)
	      into constructor-args)
	    (collect descriptor into constructor-used-slots)
	    (collect (getf slot-name-args descriptor) into final-args))
	  (let ((init-form (member :init-form descriptor))
		(create    (member :create descriptor))
		(destroy   (member :destroy descriptor)))
	    (collect (list (list (getf slot-name-keys (car descriptor))
				 (getf slot-name-args (car descriptor)))
			   (if init-form
			       (cadr init-form)
			       nil))
	      into constructor-args)
	    (if (cadr create)
		(let ((final-arg (gensym)))
		  (collect (list final-arg (rec-substitute (cadr create) slot-name-args))
		    into let-bindings)
		  (collect final-arg into final-args))
		(collect (getf slot-name-args (car descriptor)) into final-args))
	    (collect (car descriptor) into constructor-used-slots)
	    (if (cadr destroy)
		(progn
		  (unioning (find-slot-names slot-names (cadr destroy)) into destructor-used-slots)
		  (collect (cadr destroy) into destructor-expressions)))))
      (finally (let ((struct-object (gensym)))
		 (return
		   `(progn
		      (defun ,constructor-name (&key ,@constructor-args)
			(let (,@let-bindings
			      (,struct-object (cffi:foreign-alloc ',type)))
			  (memset ,struct-object 0 (cffi:foreign-type-size ',type))
			  (cffi:with-foreign-slots (,constructor-used-slots ,struct-object ,type)
			    (setf ,@(apply #'append (mapcar #'list constructor-used-slots final-args))))
			  (values ,struct-object)))
		      (defun ,destructor-name (,struct-object)
			,@(if destructor-used-slots
			      `((cffi:with-foreign-slots (,destructor-used-slots ,struct-object ,type)
				  ,@destructor-expressions))
			      nil)
			(cffi:foreign-free ,struct-object))
		      (defwith ,name-with
			,constructor-name
			,destructor-name))))))))

