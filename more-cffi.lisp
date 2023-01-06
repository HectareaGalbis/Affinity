

(in-package :mcffi)

(adp:in-file #P"docs/mcffi-api")

(adp:header "More-cffi API")

;; ----------------------------
;; ----- Helper functions -----
;; ----------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun find-slot-names (slot-names expr)
    "Return the list of symbols from slot-names that appear in expr"
    (labels ((find-slot-names-aux (names l names-found)
	       (cond
		 ((and (symbolp l) (member l names)) (adjoin l names-found))
		 ((consp l) (find-slot-names-aux names (cdr l) (find-slot-names-aux names (car l) names-found)))
		 (t names-found))))
      (find-slot-names-aux slot-names expr nil)))

  (defun exists-rec (syms l)
    "Return a non-nil value if there is some symbol from syms in l"
    (if (consp l)
	(or (exists-rec syms (car l)) (exists-rec syms (cdr l)))
	(member l syms)))

  (defun rec-substitute (assoc-symbols l)
    "Substitute every ocurrence of each symbol in assoc-symbol by its associated symbol. assoc-symbols is a property list."
    (cond
      ((and (symbolp l) (member l assoc-symbols)) (getf assoc-symbols l))
      ((consp l) (cons (rec-substitute assoc-symbols (car l)) (rec-substitute assoc-symbols (cdr l))))
      (t l)))

  (cffi:defcfun "memset" :pointer
    (str :pointer) (c :int) (n :size)))


;; -----------------------------------
;; ----- define-callback-definer -----
;; -----------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun check-callback-definer-arg-descriptor (arg-descriptor)
    (unless (and (listp arg-descriptor)
		 (not (null arg-descriptor)))
      (error "MCFFI error: Expected a non-null list as an argument descriptor but ~s was found."
	     arg-descriptor))
    (unless (typep (car arg-descriptor) 'symbol)
      (error "MCFFI error: Expected a symbol as a callback argument but ~s was found."
	     (car arg-descriptor)))
    (let ((slot-name (car arg-descriptor))
	  (createp (member :receiver arg-descriptor))
	  (returnp (member :returner arg-descriptor))
	  (typep (member :type arg-descriptor))
	  (virtualp (cadr (member :virtual arg-descriptor))))
      (unless (not (and virtualp returnp))
	(error "MCFFI error: The options :virtual and :returner cannot be used simultaneously but they were found in the following descriptor:~%  ~s"
	       arg-descriptor))
      (unless (not (and createp returnp))
	(error "MCFFI error: The options :receiver and :returner cannot be used simultaneously but they were found in the following descriptor:~%  ~s"
	       arg-descriptor))
      (unless (not (and virtualp typep))
	(error "MCFFI error: The options :virtual and :type cannot be used simultaneously but they were found in the following descriptor:~%  ~s"
	       arg-descriptor))
      (when (not virtualp)
	(unless typep
	  (error "MCFFI error: Expected the option :virtual or :type in the following descriptor:~%  ~s"
		 arg-descriptor)))
      (loop for rest-descriptor on (cdr arg-descriptor) by #'cddr
	    for option-type = (car rest-descriptor)
	    for option-value = (cadr rest-descriptor)
	    do (unless (member option-type '(:type :virtual :receiver :returner))
		 (error "MCFFI error: Expected :type, :virtual, :receiver or :returner but ~s was found in the following descriptor:~%  ~s"
			option-type arg-descriptor))
	       (case option-type
		 (:type
		  (unless (or (symbolp option-value)
			      (and (listp option-value)
				   (or (eq (car option-value) :struct)
				       (eq (car option-value) :union))))
		    (error "MCFFI error: Expected a CFFI type but ~s was found in the following descriptor:~%  ~s"
			   option-value arg-descriptor)))
		 (:receiver
		  (unless (or (not virtualp) (not (null option-value)))
		    (error "MCFFI error: Expected non-NIL :receiver expression because :virtual is being used in the following descriptor:~%  ~s"
			   arg-descriptor))
		  (unless (or (null option-value) virtualp (exists-rec (list slot-name) option-value))
		    (error "MCFFI error: Expected the use of the symbol ~s in the :receiver expression in the following descriptor:~%  ~s"
			   slot-name arg-descriptor)))
		 (:returner
		  (unless (exists-rec (list slot-name) option-value)
		    (error "MCFFI error: Expected the use of the symbol ~s in the :returner expression in the following descriptor:~%  ~s"
			   slot-name arg-descriptor)))))))

  (defun check-callback-definer-arg-descriptors (arg-descriptors)
    (loop for arg-descriptor in arg-descriptors
	  do (check-callback-definer-arg-descriptor arg-descriptor)
	  count (member :returner arg-descriptor) into return-descriptors
	  finally (unless (<= return-descriptors 1)
		    (error "MCFFI error: Expected zero or one descriptor using the :returner option but the following descriptors are found:~%~{  ~s~%~}"
			   return-descriptors))))

  (defun extract-create-arguments (arg-descriptors)
    "Return a list of lists with 5 elements: The slot name, the create expression, the type,
whether is a foreign argument and whether is a lisp argument."
    (loop for arg-descriptor in arg-descriptors
	  for slot-name = (car arg-descriptor)
	  for type = (cadr (member :type arg-descriptor))
	  for createp = (member :receiver arg-descriptor)
	  for returnp = (member :returner arg-descriptor)
	  for virtualp = (member :virtual arg-descriptor)
	  when (or createp (not returnp))
	    collect (list slot-name
			  (if createp (cadr createp) slot-name)
			  type
			  (not virtualp)
			  (or virtualp (not createp) (cadr createp)))))

  (defun extract-return-argument (arg-descriptors)
    "Return a list with three elements: The slot name, the return expression and the type."
    (loop for arg-descriptor in arg-descriptors
	  for slot-name = (car arg-descriptor)
	  for returnp = (member :returner arg-descriptor)
	  for type = (cadr (member :type arg-descriptor))
	  when returnp
	    return (list slot-name
			 (cadr returnp)
			 type)))

  (defun create-definer-code (name docstring create-arguments return-argument)
    (loop for (slot-name create-expr type foreign-arg lisp-arg) in create-arguments
	  when lisp-arg
	    collect slot-name into lisp-args
	  when create-expr
	    collect create-expr into lisp-create-exprs
	  when foreign-arg
	    collect slot-name into foreign-args
	    and collect type into foreign-types
	  finally (let ((callback-name (make-symbol "NAME"))
			(callback-body (make-symbol "BODY")))
		    (with-gensyms (user-lisp-args callback-let-create-exprs)
		      (let* ((callback-args (mapcar (lambda (x) (gensym (symbol-name x))) foreign-args))
			     (lisp-args-callback-args (mapcan #'list lisp-args callback-args))
			     (callback-create-exprs (mapcar (lambda (x) (rec-substitute lisp-args-callback-args x)) lisp-create-exprs))
			     (callback-args-types (mapcar #'list callback-args foreign-types)))
			(return `(adp:defmacro ,name (,callback-name ,lisp-args &body ,callback-body)
				   ,@(when docstring
				       `(,docstring))
				   (let* ((,user-lisp-args ,(cons 'list lisp-args))
					  (,callback-let-create-exprs (mapcar #'list ,user-lisp-args ',callback-create-exprs)))
				     ,(if return-argument
					  (multiple-value-bind (slot-name return-expr ret-type) return-argument
					    (with-gensyms (callback-return-sym)
					      (let ((callback-return-expr (rec-substitute (list slot-name callback-return-sym) return-expr)))
						``(cffi:defcallback ,,callback-name ,',ret-type ,',callback-args-types
						    (let ((,',callback-return-sym (let ,,callback-let-create-exprs
										    ,@,callback-body)))
						      ,',callback-return-expr)))))
					  ``(cffi:defcallback ,,callback-name :void ,',callback-args-types
					      (let ,,callback-let-create-exprs
						,@,callback-body))))))))))))

(adp:defmacro define-callback-definer (name &body arg-descriptors)
  "Define a macro named NAME to define callbacks. This macro has the following syntax:

  (DEFINE-CALLBACK-DEFINER name [docstring] arg-descriptor*)
  docstring      ::= string
  arg-descriptor ::= (slot-name slot-option*)
  slot-name      ::= symbol
  slot-oprion    ::= { :RECEIVER expr | :RETURNER expr }1 | { :TYPE type }1 | { :VIRTUAL expr }

To understand how this macro works we need to talk about C-arguments and Lisp-arguments. If you define a callback using CFFI:DEFCALLBACK
the resulting function will receive C-args. That arguments should be translated to the Lisp world resulting on Lisp-arguments. Regarding
the result value it will be first on the Lisp world and it should be translated to C. With this macro we can establish what are these
arguments. 

If you add an arg-descriptor you are indicating that your callback will have a C-arg named slot-name. Using :TYPE gives to that arg the
specified foreign type (CFFI type). Right now a Lisp-argument is created but no translation will be done. To do a custom translation you must use the :RECEIVER
option. The associated expression must use slot-name and return the new Lisp-arg. But, if the expression is NIL no Lisp-argument will be created.
In this case you should use this C-arg in the :RECEIVER expression of another arg-descriptor. In other words, the resulting callbacks will have
one less argument. 

You can use :RETURNER instead of :RECEIVER to indicate slot-name is not a callback argument but a return value. The expression must use
the symbol slot-name to return the new C returned value. 

The last available option is :VIRTUAL. Using this option indicates that slot-name is not a C-arg but will be a Lisp-arg. You should use
:RECEIVER and an expression using the rest of slot-names to initialize this Lisp-arg."
  (check-type name symbol)
  (let ((docstring (if (stringp (car arg-descriptors))
		       (car arg-descriptors)
		       nil))
	(real-arg-descriptors (if (stringp (car arg-descriptors))
				  (cdr arg-descriptors)
				  arg-descriptors)))
    (check-callback-definer-arg-descriptors real-arg-descriptors)
    (create-definer-code name
			 docstring
			 (extract-create-arguments real-arg-descriptors)
			 (extract-return-argument real-arg-descriptors))))


;; ---------------------------------
;; ----- define-foreign-struct -----
;; ---------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun check-foreign-struct-struct-type (struct-type)
    (unless (and (typep struct-type 'list)
		 (member (car struct-type) '(:struct :union)))
      (error "MCFFI error: Expected a list starting with :struct or :union but ~s was found."
	     struct-type)))

  (defun check-foreign-struct-infix (infix)
    (unless (symbolp infix)
      (error "MCFFI error: Expected a symbol as an infix but ~s was found."
	     infix)))

  (defun check-foreign-struct-options (options)
    (unless (listp options)
      (error "MCFFI error: Expected a list of option but ~s was found."
	     options))
    (loop for option in options
	  do (unless (member option '(:no-constructor :no-destructor :default-readers :default-writers :default-constructors :include-invisibles))
	       (error "MCFFI error: Expected :no-constructor, :no-destructor, :default-readers, :default-writers, :default-constructors or :include-invisibles but ~s was found."
		      option))))

  (defun check-foreign-struct-constructor-documentation (documentation no-constructor-p)
    (unless (not no-constructor-p)
      (error "MCFFI error: Trying to add documentation to the constructor when :NO-CONSTRUCTOR is used. Found: ~s"
	     documentation))
    (unless (= (length documentation) 2)
      (error "MCFFI error: The constructor documentation form must be a list with two elements, but ~s was found."
	     documentation))
    (unless (stringp (cadr documentation))
      (error "MCFFI error: The argument agter :CONSTRUCTOR-DOCUMENTATION must be a string but ~s was found."
	     (cadr documentation))))

  (defun check-foreign-struct-destructor-documentation (documentation no-destructor-p)
    (unless (not no-destructor-p)
      (error "MCFFI error: Trying to add documentation to the destructor when :NO-DESTRUCTOR is used. Found: ~s"
	     documentation))
    (unless (= (length documentation) 2)
      (error "MCFFI error: The destructor documentation form must be a list with two elements, but ~s was found."
	     documentation))
    (unless (stringp (cadr documentation))
      (error "MCFFI error: The argument agter :DESTRUCTOR-DOCUMENTATION must be a string but ~s was found."
	     (cadr documentation))))
  
  (defun check-foreign-struct-slot-name (slot-name virtualp slot-names struct-type)
    (unless (symbolp slot-name)
      (error "MCFFI error: Expected a symbol as a slot descriptor name but ~s was found."
	     slot-name))
    (unless (or virtualp (member slot-name slot-names))
      (error "MCFFI error: Expected a member of ~s but ~s was found."
	     struct-type slot-name)))

  (defun check-foreign-struct-initform-option (constructorp constructor-value slot-name)
    (when constructorp
      (unless (car constructor-value)
	(error "MCFFI error: The option :initform is used when no parameter is specified in the :constructor expression. Found in the ~s despcriptor."
	       slot-name))))

  (defun check-foreign-struct-constructor-option (constructor-value virtual-value slot-name)
    (unless (or (null constructor-value)
		(and (listp constructor-value)
		     (listp (car constructor-value))
		     (<= (length (car constructor-value)) 1)
		     (symbolp (caar constructor-value))
		     (not (null (cadr constructor-value)))))
      (error "MCFFI error: Expected an expression with the syntax (([arg]) &body body) in the ~s descriptor but ~s was found."
	     slot-name constructor-value))
    (unless (or (null (car constructor-value))
		(not (eq slot-name (caar constructor-value))))
      (error "MCFFI error: The argument symbol ~s used in the :constructor expression must be different of the slot member name."
	     (car constructor-value)))
    (unless (or (null (car constructor-value))
		(exists-rec (list (caar constructor-value)) (cdr constructor-value)))
      (error "MCFFI error: Expected the use of the symbol ~s in the :constructor expression."
	     slot-name))
    (unless (or virtual-value (null constructor-value) (exists-rec (list slot-name) (cdr constructor-value)))
      (error "MCFFI error: Expected the use of the symbol ~s in the :constructor expression."
	     slot-name)))

  (defun check-foreign-struct-destructor-option (destructor-value slot-name)
    (unless (exists-rec (list slot-name) destructor-value)
      (error "MCFFI error: Expected the use of the symbol ~s in the :destructor expression."
	     slot-name)))

  (defun check-foreign-struct-reader-option (reader-value virtual-value slot-name)
    (unless (or (null reader-value)
		(and (listp reader-value)
		     (listp (car reader-value))
		     (not (null (cadr reader-value)))))
      (error "MCFFI error: Expected a :reader expression of the form ((&rest args) &body body) in the ~s descriptor but ~s was found."
	     slot-name reader-value))
    (unless (or virtual-value (null reader-value) (exists-rec (list slot-name) reader-value))
      (error "MCFFI error: Expected the use of the symbol ~s in the :reader expression."
	     slot-name)))

  (defun check-foreign-struct-writer-option (writer-value virtual-value slot-name)
    (unless (or (null writer-value)
		(and (listp writer-value)
		     (listp (car writer-value))
		     (not (null (car writer-value)))
		     (not (null (cadr writer-value)))
		     (not (member (caar writer-value) '(&optional &key &rest &aux &allow-other-keys)))))
      (error "MCFFI error: Expected a :writer expression with the form ((new-val &rest args) &body body) in the ~s descriptor but ~s was found."
	     slot-name writer-value))
    (unless (or virtual-value (null writer-value) (exists-rec (list slot-name) writer-value))
      (error "MCFFI error: Expected the use of the symbol ~s in the :writer expression."
	     slot-name)))

  (defun check-foreign-struct-slot-descriptor (descriptor slot-names struct-type no-constructor-p no-destructor-p)
    (unless descriptor
      (error "MCFFI error: Expected a non-NIL expression as a slot descriptor."))
    (if (listp descriptor)
	(let* ((slot-name (car descriptor))
	       (slot-options (cdr descriptor))
	       (virtualp (member :virtual descriptor))
	       (virtual-value (second virtualp)))
	  (check-foreign-struct-slot-name slot-name virtual-value slot-names struct-type)
	  (when slot-options
	    (let* ((destructorp (member :destructor descriptor))
		   (pointerp (member :pointer descriptor))
		   (constructorp (member :constructor descriptor))
		   (constructor-value (cadr constructorp)))
	      (unless (not (and virtual-value destructorp))
		(error "MCFFI error: The :destroy and :virtual options cannot be used simultaneously but they were found in the ~s descriptor."
		       slot-name))
	      (unless (not (and virtual-value pointerp))
		(error "MCFFI error: The :pointer and :virtual options cannot be used simultaneously but they were found in the ~s descriptor."
		       slot-name))
	      (unless (not (and no-constructor-p constructorp))
		(error "MCFFI error: The option :constructor cannot be used when the global option :no-constructor is activated but it was found in the ~s descriptor."
		       slot-name))
	      (unless (not (and no-destructor-p destructorp))
		(error "MCFFI error: The option :destructor cannot be used when the global option :no-destructor is activated but it was found in the ~s descriptor."
		       slot-name))
	      (loop for rest-slot-options on slot-options by #'cddr
		    for option-type = (car rest-slot-options)
		    for option-value = (cadr rest-slot-options)
		    do (unless (member option-type '(:name :initform :pointer :virtual :constructor :destructor :reader :writer))
			 (error "MCFFI error: Expected :name, :initform, :pointer, :virtual, :constructor, :destructor, :reader or :writer in ~s descriptor but ~s was found."
				slot-name option-type)) 
		       (case option-type
			 (:name
			  (unless (typep option-value 'symbol)
			    (error "MCFFI error: Expected a symbol as a :name expression but ~s was found."
				   option-value)))
			 (:initform
			  (check-foreign-struct-initform-option constructorp constructor-value slot-name))
			 (:constructor
			     (check-foreign-struct-constructor-option option-value virtual-value slot-name))
			 (:destructor
			  (check-foreign-struct-destructor-option option-value slot-name))
			 (:reader
			  (check-foreign-struct-reader-option option-value virtual-value slot-name))
			 (:writer
			  (check-foreign-struct-writer-option option-value virtual-value slot-name)))))))
	(check-foreign-struct-slot-name descriptor nil slot-names struct-type)))

  (defun extract-constructor-documentation (descriptors doctype)
    (cond
      ((and (listp (car descriptors))
	    (eq (caar descriptors) doc-type))
       (car descriptors))
      ((and (listp (cadr descriptors))
	    (eq (caadr descriptors) doc-type))
       (cadr descriptors))
      (t nil)))

  (defun check-foreign-struct-slot-descriptors (descriptors slot-names struct-type no-constructor-p no-destructor-p)
    (let* ((constructor-doc (extract-constructor-documentation descriptors :constructor-documentation))
	   (destructor-doc  (extract-constrcutor-documentation descriptors :destructor-documentation))
	   (real-descriptors (cond
			       ((and constructor-doc destructor-doc)
				(cddr descriptors))
			       ((or constructor-doc destructor-doc)
				(cdr descriptors))
			       (t descriptors))))
      (check-foreign-struct-constructor-documentation constructor-doc no-constructor-p)
      (check-foreign-struct-destructor-documentation destructor-doc no-destructor-p)
      (loop for descriptor in real-descriptors
	    do (check-foreign-struct-slot-descriptor descriptor slot-names struct-type no-constructor-p no-destructor-p))))
  
  (defun extract-descriptor-info (slot-name descriptor keyword)
    "Return a list with four elements: The slot name, whether the slot is invisible, the option expr, whether the option is used."
    (cond
      ((null descriptor) (list slot-name t nil nil))
      ((symbolp descriptor) (list slot-name nil nil nil))
      (t (let ((key-expr (member keyword descriptor)))
	   (list slot-name nil (cadr key-expr) (and key-expr t))))))

  (defun create-constructor-code (constructor-infos constructor-doc pointer-slots name-infos initform-infos
				  struct-type enable-default-constructors enable-invisibles suffix)
    (let ((slot-names (cffi:foreign-slot-names struct-type)))
      (iter
       (for (slot-name invisiblep constructor constructorp) in constructor-infos)
       (when constructor
	 (unioning (find-slot-names slot-names (cdr constructor)) into used-slots))
       (when (and enable-default-constructors (not constructorp))
	 (collect slot-name into used-slots))
       (when (and (or enable-invisibles (not invisiblep))
		  (or constructor (and enable-default-constructors (not constructorp))))
	 (if constructorp
	     (when (not (null (car constructor)))
	       (let* ((namep (member slot-name name-infos :key #'car))
		      (keyword-name (if namep (cadar namep) slot-name))
		      (arg (caar constructor))
		      (initformp (member slot-name initform-infos :key #'car))
		      (initform (if initformp (cadar initformp) 0))
		      (supplied-var (when (eq (car struct-type) :union) (gensym))))
		 (collect `((,(intern (symbol-name keyword-name) "KEYWORD") ,arg) ,initform ,@(when (eq (car struct-type) :union) `(,supplied-var)))
		   into constructor-parameters)
		 (if (eq (car struct-type) :union)
		     (collect `(when ,supplied-var ,@(cdr constructor)) into constructor-expressions)
		     (appending (cdr constructor) into constructor-expressions))))
	     (let* ((namep (member slot-name name-infos :key #'car))
		    (keyword-name (if namep (cadar namep) slot-name))
		    (arg (make-symbol (symbol-name keyword-name)))
		    (initformp (member slot-name initform-infos :key #'car))
		    (initform (if initformp (cadar initformp) 0))
		    (supplied-var (when (eq (car struct-type) :union) (gensym))))
	       (collect `((,(intern (symbol-name keyword-name) "KEYWORD") ,arg) ,initform ,@(when (eq (car struct-type) :union) `(,supplied-var)))
		 into constructor-parameters)
	       (collect (if (eq (car struct-type) :union)
			    `(when ,supplied-var (setf ,slot-name ,arg))
			    `(setf ,slot-name ,arg))
		 into constructor-expressions))))
       (finally (return (let ((final-used-slots (mapcar (lambda (x) (if (member x pointer-slots)
									(list :pointer x)
									x))
							used-slots)))
			  (with-gensyms (object-sym)
			    `(adp:defun ,(intern (concatenate 'string "CREATE-" (string-upcase (symbol-name suffix))))
				 (&key ,@constructor-parameters)
			       ,@(when constructor-doc
				   `(,constructor-doc))
			       (let ((,object-sym (cffi:foreign-alloc ',struct-type)))
				 (memset ,object-sym 0 (cffi:foreign-type-size ',struct-type))
				 (cffi:with-foreign-slots (,final-used-slots ,object-sym ,struct-type)
				   ,@constructor-expressions)
				 (values ,object-sym))))))))))

  (defun create-destructor-code (destructor-infos destructor-doc pointer-slots struct-type suffix)
    (let ((slot-names (cffi:foreign-slot-names struct-type)))
      (iter (for (nil nil destructor destructorp) in destructor-infos)
	    (when destructorp
	      (collect destructor into destructor-exprs)
	      (unioning (find-slot-names slot-names destructor) into used-slots))
	    (finally (return (let ((final-used-slots (mapcar (lambda (x) (if (member x pointer-slots)
									     (list :pointer x)
									     x))
							     used-slots))
				   (arg (make-symbol "OBJECT")))
			       `(adp:defun ,(intern (concatenate 'string "DESTROY-" (symbol-name suffix))) (,arg)
				  ,@(when destructor-doc
				   `(,destructor-doc))
				  ,(when final-used-slots
				     `(cffi:with-foreign-slots (,final-used-slots ,arg ,struct-type)
					,@destructor-exprs))
				  (cffi:foreign-free ,arg))))))))

  (defun create-with-code (suffix)
    (let ((create-name (intern (concatenate 'string "CREATE-" (symbol-name suffix))))
	  (destroy-name (intern (concatenate 'string "DESTROY-" (symbol-name suffix)))))
      `(defwith ,create-name #',create-name #',destroy-name)))

  (defun create-readers-code (reader-infos pointer-slots name-infos struct-type
			      enable-default-readers enable-invisibles prefix)
    (let ((slot-names (cffi:foreign-slot-names struct-type)))
      (iter (for (slot-name invisiblep reader readerp) in reader-infos)
	    (when (and (or enable-invisibles (not invisiblep))
		       (or reader (and enable-default-readers (not readerp))))
	      (let* ((object-arg (make-symbol "OBJECT"))
		     (args (cons object-arg (if reader (car reader) nil)))
		     (reader-doc   (when (and reader (stringp (cadr reader)))
				     (cadr reader)))
		     (final-reader (if reader
				       (cons 'progn (if reader-doc
							(cddr reader)
							(cdr reader)))
				       slot-name))
		     (used-slots (find-slot-names slot-names final-reader))
		     (final-used-slots (mapcar (lambda (x) (if (member x pointer-slots)
							       (list :pointer x)
							       x))
					       used-slots))
		     (namep (member slot-name name-infos :key #'car))
		     (name (if namep (cadar namep) slot-name)))
		(collect `(adp:defun ,(intern (concatenate 'string (symbol-name prefix) "-" (symbol-name name)))
			    ,args
			    ,@(when reader-doc
				`(,reader-doc))
			    (cffi:with-foreign-slots (,final-used-slots ,(car args) ,struct-type)
			      ,final-reader))))))))

  (defun create-writers-code (writer-infos pointer-slots name-infos struct-type
			      enable-default-writers enable-invisibles prefix)
    (let ((slot-names (cffi:foreign-slot-names struct-type)))
      (iter (for (slot-name invisiblep writer writerp) in writer-infos)
	    (when (and (or enable-invisibles (not invisiblep))
		       (or writer (and enable-default-writers (not writerp))))
	      (let* ((object-arg (make-symbol "OBJECT"))
		     (new-value-arg (if writer (caar writer) (gensym)))
		     (args (if writer
			       `(,new-value-arg ,object-arg ,@(cdar writer))
			       `(,new-value-arg ,object-arg)))
		     (writer-doc   (when (and writer (stringp (cadr writer)))
				     (cadr writer)))
		     (final-writer (if writer
				       (cons 'progn (if writer-doc
							(cddr writer)
							(cdr writer)))
				       `(setf ,slot-name ,new-value-arg)))
		     (used-slots (find-slot-names slot-names final-writer))
		     (final-used-slots (mapcar (lambda (x) (if (member x pointer-slots)
							       (list :pointer x)
							       x))
					       used-slots))
		     (namep (member slot-name name-infos :key #'car))
		     (name (if namep (cadar namep) slot-name)))
		(collect `(adp:defun (setf ,(intern (concatenate 'string (symbol-name prefix) "-" (symbol-name name))))
			    ,args
			    ,@(when writer-doc
				`(,writer-doc))
			    (cffi:with-foreign-slots (,final-used-slots ,object-arg ,struct-type)
			      ,final-writer))))))))

  (defun define-foreign-struct-aux (struct-type infix options &rest slot-descriptors)
    (let* ((no-constructorp      (member :no-constructor options))
	   (no-destructorp       (member :no-destructor options))
	   (default-constructorp (member :default-constructors options))
	   (default-readerp      (member :default-readers options))
	   (default-writerp      (member :default-writers options))
	   (invisiblesp          (member :include-invisibles options))
	   (constructor-doc      (when (not no-constructorp)
				   (extract-constructor-documentation slot-descriptors :constructor-documentation)))
	   (destructor-doc       (when (not no-destructorp)
				   (extract-constructor-documentation slot-descriptors :destructor-documentation))))
      (loop for slot-name in (cffi:foreign-slot-names struct-type)
	    for pre-slot-descriptor = (car (member slot-name slot-descriptors :key (lambda (x) (if (listp x) (car x) x))))
	    for slot-descriptor =     (if (listp pre-slot-descriptor) pre-slot-descriptor (list pre-slot-descriptor))
	    for pointerp =       (member :pointer slot-descriptor)
	    for pointer-value =  (cadr pointerp)
	    for namep =          (member :name slot-descriptor)
	    for name-value =     (cadr namep)
	    for initformp =      (member :initform slot-descriptor)
	    for initform-value = (cadr initformp)
	    when pointer-value
	      collect slot-name                                                         into pointer-slots
	    when namep
	      collect (list slot-name name-value)                                       into name-infos
	    when initformp
	      collect (list slot-name initform-value)                                   into initform-infos
	    when (not no-constructorp)
	      collect (extract-descriptor-info slot-name slot-descriptor :constructor)  into constructor-infos
	    when (not no-destructorp)
	      collect (extract-descriptor-info slot-name slot-descriptor :destructor)   into destructor-infos
	    collect (extract-descriptor-info slot-name slot-descriptor :reader)         into reader-infos
	    collect (extract-descriptor-info slot-name slot-descriptor :writer)         into writer-infos
	    finally (return `(progn
			       ,@(unless no-constructorp
				   (list (create-constructor-code constructor-infos constructor-doc
								  pointer-slots name-infos
								  initform-infos struct-type
								  default-constructorp
								  invisiblesp
								  infix)))
			       ,@(unless no-destructorp
				   (list (create-destructor-code destructor-infos destructor-doc
								 pointer-slots struct-type infix)))
			       ,@(unless (or no-constructorp
					     no-destructorp)
				   (list (create-with-code infix)))
			       ,@(create-readers-code reader-infos pointer-slots name-infos struct-type
						      default-readerp invisiblesp infix)
			       ,@(create-writers-code writer-infos pointer-slots name-infos struct-type
						      default-writerp invisiblesp infix)))))))

(adp:defmacro define-foreign-struct (struct-type infix options &body slot-descriptors)
  "This macro has the following syntax:

  (DEFINE-FOREIGN-STRUCT struct-type infix-names options [constructor-docstring] [destructor-docstring] slot-descriptor*)

  struct-type           ::= ( { :STRUCT | :UNION } cffi-type )
  cffi-type             ::= 'symbol'
  infix-names           ::= infix-name | (infix-name+)
  infix-name            ::= 'symbol'
  options               ::= option*
  option                ::= { :NO-CONSTRUCTOR | :NO-DESTRUCTOR | :DEFAULT-CONSTRUCTORS | :DEFAULT-READERS | :DEFAULT-WRITERS | :INCLUDE-INVISIBLES }
  constructor-docstring ::= (:CONSTRUCTOR-DOCUMENTATION docstring)
  destructor-docstring  ::= (:DESTRUCTOR-DOCUMENTATION docstring)
  slot-descriptor       ::= slot-name | (slot-name slot-option*)
  slot-name             ::= 'symbol'
  slot-option           ::= { :NAME name } | { :POINTER pointer } | { :VIRTUAL virtual } | { :INITFORM initform } | { :CONSTRUCTOR constructor } | { :DESTRUCTOR destructor } | { :READER reader } | { :WRITER writer }
  name                  ::= 'symbol'
  pointer               ::= 'form'
  virtual               ::= 'form'
  initform              ::= 'form'
  constructor           ::= NIL | (([constructor-arg]) constructor-body*)
  constructor-arg       ::= 'symbol'
  constructor-body      ::= 'form'
  destructor            ::= 'form'
  reader                ::= NIL | (reader-lambda-list [docstring] reader-body*)
  reader-lambda-list    ::= 'lambda-list'
  reader-body           ::= 'form'
  writer                ::= (writer-lambda-list [docstring] writer-body*)
  writer-lambda-list    ::= (new-value . rest-lambda-list)
  new-value             ::= 'symbol'
  rest-lambda-list      ::= 'lambda-list'
  docstring             ::= 'string'
  
Define a wraper around the foreign struct (or union) STRUCT-TYPE. Each INFIX-NAME is used to generate the function names.
In fact, for each INFIX-NAME, a constructor, destructor and set of accessors are defined. You should use this if multiple C types
refer to the same struct type.

OPTIONS is a list of keywords that modify the way this macro works. If :NO-CONSTRUCTOR is specified, then this macro will 
not define a constructor. In the same way, if :no-destructor is specified no destructor will be defined. If :DEFAULT-CONSTRUCTORS
 is used each slot member has no need to define an explicit constructor. Instead, a default initialization of the foreign slot is 
done (no translation). The same occurs with :DEFAULT-READERS and :DEFAULT-WRITERS when defining the accessors. You will need 
to write every slot you want to define a constructor, destructor, reader or writer for. However, if :INCLUDE-INVISIBLES is used 
that functions are defined even if you don't specify the existence of that slot.

Using CONSTRUCTOR-DOCSTRING and DESTRUCTOR-DOCSTRING adds a docstring to the constructor and destructor respectively.

Each SLOT-DESCRIPTOR specify the translation of a foreign struct member. The SLOT-NAME is a symbol denoting a foreign member of 
the struct STRUCT-TYPE (unless :VIRTUAL is used). The different options allow you to specify the translations of each member:

  - NAME:        If used, that symbol will be used to create the accessors names.

  - INITFORM:    Specifies the default value of SLOT-NAME member.
 
  - CONSTRUCTOR: This option is used to specify a translation when receiving a parameter in the constructor. If the expression after :CONSTRUCTOR
                 is NIL or neither this option nor :DEFAULT-CONSTRUCTORS are used a constructor will not be defined. If this option is not used
                 but :DEFAULT-CONSTRUCTORS is, a parameter is still received to initialize the foreign member but no translation will be performed.
                 The expression after :CONSTRUCTOR, if not NIL, must be a list where the first element is NIL or a list with a symbol. If NIL,
                 no argument will be received in the constructor. Otherwise, an argument with name the specified symbol will be received. In both cases,
                 the rest of elements of the list after :CONSTRUCTOR must be expressions to initialize the member SLOT-NAME (using setf, for example).
                 It is possible to use the arguments specified from other :CONSTRUCTOR options. Also, every SLOT-NAME is accessible from here.

  - DESTRUCTOR:  This must be an expression doing something with SLOT-NAME, like CFFI:FREE-ing it.
  - READER:      This option works the same as :CONSTRUCTOR about when an accessor is defined or not. Depending on the expression after :READER and
                 :DEFAULT-READERS the accessor may be defined or not. If the expression after :READER is not NIL, then it must be a list where the first
                 element is the lambda list of the accessor. The rest of elements are forms that must return the desired value. Every SLOT-NAME is accessible.
  
  - WRITER:      Same as :READER but indicates that the accessor is SETF-able. The same rules apply here when using or not :DEFAULT-WRITERS.
                 The expression after :WRITER, if not NIL, must be a list where its first element is the lambda list of the SETF function. So, a first
                 symbol is required always denoting the NEW-VALUE. The rest of elements are forms that should modify SLOT-NAME. Every SLOT-NAME is accessible.
  
  - POINTER:     Some structs have as a member another struct. You may want to use its pointer rather than the struct itself. To do that you only need to
                 use a non-NIL expression after :POINTER.
  
  - VIRTUAL:     Using :VIRTUAL you can receive an additional parameter in the constructor, or create additional accessors.

An additional note: If :NO-CONSTRUCTOR and :NO-DESTRUCTOR are NOT used, then a 'with-constructor' is defined. See the Clith project for more information."
  (check-foreign-struct-struct-type struct-type)
  (check-foreign-struct-infix infix)
  (check-foreign-struct-options options)
  (check-foreign-struct-slot-descriptors slot-descriptors (cffi:foreign-slot-names struct-type)
					 struct-type (member :no-constructor options) (member :no-destructor options))
  (if (listp infix)
      `(progn
	 ,@(loop for subinfix in infix
		 collect (apply #'define-foreign-struct-aux struct-type subinfix options slot-descriptors)))
      (apply #'define-foreign-struct-aux struct-type infix options slot-descriptors)))
