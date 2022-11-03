

(in-package :mcffi)

(adp:write-in-file #P"docs/mcffi-api")

(adp:header "More-cffi API")

;; ----------------------------
;; ----- Helper functions -----
;; ----------------------------

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


;; -------------------
;; ----- defwith -----  
;; -------------------

(adp:defmacro defwith (name create destroy &optional (arity 1) docstring)
  "Define a macro named NAME. This new macro has the following syntax:

  (NAME var-or-vars (&rest args) &body body)

When using this new macro CREATE is called with ARGS and the results are stored in VAR-OR-VARS. If VAR-OR-VARS
is a symbol, the rest of returned values are ignored. Afterwards, the BODY forms are evaluated. Finally, 
DESTROY is called. The arguments used by DESTROY depends on ARITY. If ARITY is a list of non-negative integers
then they denote the arguments returned by CREATE to be used by DESTROY in the order they appear. For example, 
using the list (3 0 2) indicates that DESTROY will receive the fourth, first and third values returned by CREATE 
and in that order. If ARITY is just a non-negative integer then it indicates the number of arguments
to be used by DESTROY. For example, if 2 is specified, then DESTROY will receive the first 2 values returned by CREATE."  
  (check-type name symbol)
  (check-type create symbol)
  (check-type destroy symbol)
  (let ((destructor-arity (when (numberp arity) arity))
	(destructor-arguments (when (listp arity) arity)))
    (when destructor-arity
      (check-type destructor-arity unsigned-byte "a non-negative integer"))
    (when destructor-arguments
      (check-type destructor-arguments list)
      (loop for dest-arg in destructor-arguments
	    do (check-type dest-arg unsigned-byte "a non-negative integer")))
    (with-gensyms (var args body ret-list-sym var-list)
      `(adp:defmacro ,name (,var ,args &body ,body)
	 ,@(when docstring
	     `(,docstring))
	 (with-gensyms ((,ret-list-sym "ret-list"))
           (let ((,var-list (if (listp ,var)
				,var
				(list ,var))))
             `(let ((,,ret-list-sym (multiple-value-list (,',create ,@,args))))
		(unwind-protect
                     (destructuring-bind ,,var-list ,,ret-list-sym
		       ,@,body)
                  (apply #',',destroy ,,(if destructor-arguments
					    ``(loop for index in ',',destructor-arguments
						    collect (nth index ,,ret-list-sym))
					    ``(subseq ,,ret-list-sym 0 ,',destructor-arity)))))))))))


;; -------------------------------------------
;; ----- define-foreign-callback-definer -----
;; -------------------------------------------

(defun check-callback-definer-arg-descriptor (arg-descriptor)
  (assert (and (listp arg-descriptor)
	       (not (null arg-descriptor)))
	  () "Expected a non-null list.~%Found:~%   ~S" arg-descriptor)
  (check-type (car arg-descriptor) symbol)
  (let ((slot-name (car arg-descriptor))
	(createp (member :create arg-descriptor))
	(returnp (member :return arg-descriptor))
	(typep (member :type arg-descriptor))
	(virtualp (cadr (member :virtual arg-descriptor))))
    (assert (not (and virtualp returnp)) () "If :virtual is used, :return is forbidden. Found them in ~s descriptor"
	    slot-name)
    (assert (not (and createp returnp)) () "Just one of :create or :return can appear in an argument descriptor.~%Found in ~s descriptor:~%   ~S~%   ~S"
	    slot-name (subseq createp 0 2) (subseq returnp 0 2))
    (assert (not (and virtualp typep)) () "If :virtual is used, :type is forbidden. Found them in ~s descriptor."
	    slot-name)
    (when (not virtualp)
      (assert typep () "Expected :type and a cffi type in ~s descriptor." slot-name))
    (loop for rest-descriptor on (cdr arg-descriptor) by #'cddr
	  for option-type = (car rest-descriptor)
	  for option-value = (cadr rest-descriptor)
	  do (assert (member option-type '(:type :virtual :create :return)) ()
		     "Expected :type, :create or :return in ~s descriptor.~%Found:~%   ~S"
		     slot-name option-type)
	     (case option-type
	       (:type
		(assert (or (symbolp option-value)
			    (and (listp option-value)
				 (or (eq (car option-value) :struct)
				     (eq (car option-value) :union))))
			() "Expected a cffi type after :type in ~s descriptor.~%Found:~%   ~s" slot-name option-value))
	       (:create
		(assert (or (not virtualp) (not (null option-value))) ()
			"Virtual slots must have a non-nil create expression. Found in ~a argument." slot-name)
		(assert (or (null option-value) virtualp (exists-rec (list slot-name) option-value)) ()
			"Create expression must use the ~s argument." slot-name))
	       (:return
		 (assert (exists-rec (list slot-name) option-value) ()
			 "Return expression must use the ~s argument." option-value))))))

(defun check-callback-definer-arg-descriptors (arg-descriptors)
  (loop for arg-descriptor in arg-descriptors
	do (check-callback-definer-arg-descriptor arg-descriptor)
	count (member :return arg-descriptor) into return-descriptors
	finally (assert (<= return-descriptors 1) ()
			"Expected zero or one return argument. Found ~a" return-descriptors)))

(defun extract-create-arguments (arg-descriptors)
  "Return a list of lists with 5 elements: The slot name, the create expression, the type,
whether is a foreign argument and whether is a lisp argument."
  (loop for arg-descriptor in arg-descriptors
	for slot-name = (car arg-descriptor)
	for type = (cadr (member :type arg-descriptor))
	for createp = (member :create arg-descriptor)
	for returnp = (member :return arg-descriptor)
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
	for returnp = (member :return arg-descriptor)
	for type = (cadr (member :type arg-descriptor))
	when returnp
	  return (list slot-name
		       (cadr returnp)
		       type)))

(defun create-definer-code (name create-arguments return-argument)
  (loop for (slot-name create-expr type foreign-arg lisp-arg) in create-arguments
	when lisp-arg
	  collect slot-name into lisp-args
	when create-expr
	  collect create-expr into lisp-create-exprs
	when foreign-arg
	  collect slot-name into foreign-args
	  and collect type into foreign-types
	finally (with-gensyms (callback-name callback-body user-lisp-args callback-let-create-exprs callback-return-sym)
		  (destructuring-bind (slot-name return-expr ret-type) return-argument
		    (let* ((callback-args (mapcar (lambda (x) (gensym (symbol-name x))) foreign-args))
			   (lisp-args-callback-args (mapcan #'list lisp-args callback-args))
			   (callback-create-exprs (mapcar (lambda (x) (rec-substitute lisp-args-callback-args x)) lisp-create-exprs))
			   (callback-args-types (mapcar #'list callback-args foreign-types))
			   (callback-return-expr (rec-substitute (list slot-name callback-return-sym) return-expr)))
		      (return `(adp:defmacro ,name (,callback-name ,lisp-args &body ,callback-body)
				 (let* ((,user-lisp-args ,(cons 'list lisp-args))
					(,callback-let-create-exprs (mapcar #'list ,user-lisp-args ',callback-create-exprs)))
				   `(cffi:defcallback ,,callback-name ,',ret-type ,',callback-args-types
				      (let ((,',callback-return-sym (let ,,callback-let-create-exprs
								      ,@,callback-body)))
					,',callback-return-expr))))))))))

(adp:defmacro define-foreign-callback-definer (name &body arg-descriptors)
  "Define a macro named NAME to define callbacks. Each ARG-DESCRIPTOR must have the following syntax:

  ARG-DESCRIPTOR ::= (SLOT-NAME [[slot-option]])
  SLOT-OPTION    ::= {:create expr | :return expr}1 | {:type type}1 | :virtual expr

To understand how this macro works we need to talk about C-arguments and Lisp-arguments. If you define a callback using CFFI:DEFCALLBACK
the resulting function will receive C-args. That arguments should be translated to the Lisp world resulting on Lisp-arguments. Regarding
the result value it will be first on the Lisp world and it should be translated to C. With this macro we can establish what are these
arguments. 

If you add an ARG-DESCRIPTOR you are indicating that your callback will have a C-arg named SLOT-NAME. Using :TYPE gives to that arg the
specified type. Right now a Lisp-argument is created but no translation will be done. To do a custom translation you must use the :CREATE
option. The associated expression must use SLOT-NAME and return the new value. But, if expression is NIL no Lisp-argument will be created.
In this case you should use this value in the :CREATE expression of another ARG-DESCRIPTOR. In other words, the resulting callbacks will have
one less argument. 

You can use :RETURN instead of :CREATE to indicate SLOT-NAME is not a callback argument but a return value. 

The last available option is :VIRTUAL. Using this option indicates that SLOT-NAME is not a C-arg but will be a Lisp arg. You should use
:CREATE and an expression using the rest of SLOT-NAMEs."
  (check-type name symbol)
  (check-callback-definer-arg-descriptors arg-descriptors)
  (create-definer-code name
		       (extract-create-arguments arg-descriptors)
		       (extract-return-argument arg-descriptors)))


;; ---------------------------------
;; ----- define-foreign-struct -----
;; ---------------------------------

(defun check-foreign-struct-struct-type (struct-type)
  (check-type struct-type list)
  (assert (member (car struct-type) '(:struct :union)) ()
	  "The struct type must be a list starting with :struct or :union. Found: ~s" struct-type))

(defun check-foreign-struct-infix (infix)
  (check-type infix (or symbol list))
  (when (listp infix)
    (loop for subinfix in infix
	  do (check-type subinfix symbol))))

(defun check-foreign-struct-options (options)
  (check-type options list)
  (loop for option in options
	do (assert (member option '(:no-constructor :no-destructor :default-readers :default-writers :default-constructors :include-invisibles)) ()
				    "Expected :no-constructor, :no-destructor, :default-readers, :default-writers, :default-constructors or :include-invisibles.~%Found:~%   ~S"
				    option)))

(defun check-foreign-struct-slot-name (slot-name virtualp slot-names struct-type)
  (check-type slot-name symbol)
  (assert (or virtualp (member slot-name slot-names)) ()
	  "Expected a slot name from ~s~%Found:~%   ~s" struct-type slot-name))

(defun check-foreign-struct-initform-option (constructorp constructor-value slot-name)
  (when constructorp
    (assert (car constructor-value) ()
	    "If the constructor does not specify a parameter in ~s descriptor, :initform is forbidden." slot-name)))

(defun check-foreign-struct-constructor-option (constructor-value virtual-value slot-name)
  (assert (or (null constructor-value)
	      (and (listp constructor-value)
		   (listp (car constructor-value))
		   (<= (length (car constructor-value)) 1)
		   (symbolp (caar constructor-value))
		   (not (null (cadr constructor-value)))))
	  () "Expected a constructor-value expression (([arg]) &body body) in ~s descriptor.~%Found:~%   ~S"
	  slot-name constructor-value)
  (assert (or (null (car constructor-value))
	      (not (eq slot-name (caar constructor-value))))
	  () "The argument ~s must be different of ~s."
	  (car constructor-value) slot-name)
  (assert (or (null (car constructor-value))
	      (exists-rec (list (caar constructor-value)) (cdr constructor-value)))
	  () "Expected the use of ~s in the constructor-value expression of ~s descriptor."
	  (caar constructor-value) slot-name)
  (assert (or virtual-value (null constructor-value) (exists-rec (list slot-name) (cdr constructor-value)))
	  () "Expected the use of ~s in its constructor-value expression." slot-name))

(defun check-foreign-struct-destructor-option (destructor-value slot-name)
  (assert (exists-rec (list slot-name) destructor-value) ()
	  "Expected the use of ~s in its destructor expression." slot-name))

(defun check-foreign-struct-reader-option (reader-value virtual-value slot-name)
  (assert (or (null reader-value)
	      (and (listp reader-value)
		   (listp (car reader-value))
		   (not (null (cadr reader-value)))))
	  () "Expected a reader-value expression ((&rest args) &body body) in the ~s descriptor.~%Found:~%   ~S"
	  slot-name reader-value)
  (assert (or virtual-value (null reader-value) (exists-rec (list slot-name) reader-value)) ()
	  "Expected the use of ~s in its reader-value expression." slot-name))

(defun check-foreign-struct-writer-option (writer-value virtual-value slot-name)
  (assert (or (null writer-value)
	      (and (listp writer-value)
		   (listp (car writer-value))
		   (not (null (car writer-value)))
		   (not (null (cadr writer-value)))
		   (not (member (caar writer-value) '(&optional &key &rest &aux &allow-other-keys)))))
	  () "Expected a writer-value expression ((new-val &rest args) &body body) in the ~s descriptor.~%Found:~%   ~S"
	  slot-name writer-value)
  (assert (or virtual-value (null writer-value) (exists-rec (list slot-name) writer-value)) ()
	  "Expected the use of ~s in its writer-value expression." slot-name))

(defun check-foreign-struct-slot-descriptor (descriptor slot-names struct-type no-constructor-p no-destructor-p)
  (assert descriptor () "Expected a non-nil expression.")
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
	    (assert (not (and virtual-value destructorp)) ()
		    "The :destroy keyword is forbidden for :virtual slots. Found :destroy in ~s descriptor." slot-name)
	    (assert (not (and virtual-value pointerp)) ()
		    "The :pointer keyword is forbidden for :virtual slots. Found :pointer in ~s descriptor." slot-name)
	    (assert (not (and no-constructor-p constructorp)) ()
		    "While :no-constructor is enabled, :constructor is forbidden. Found :constructor in ~S descriptor." slot-name)
	    (assert (not (and no-destructor-p destructorp)) ()
		    "While :no-destructor is enabled, :destructor is forbidden. Found :destructor in ~S descriptor." slot-name)
	    (loop for rest-slot-options on slot-options by #'cddr
		  for option-type = (car rest-slot-options)
		  for option-value = (cadr rest-slot-options)
		  do (assert (member option-type '(:name :type :initform :pointer :virtual :constructor :destructor :reader :writer)) ()
			     "Expected :name, :type, :initform, :pointer, :virtual, :constructor, :destructor, :reader or :writer in ~S descriptor.~%Found:~%   ~S"
			     slot-name option-type) 
		     (case option-type
		       (:name
			(check-type option-value symbol))
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


(defun check-foreign-struct-slot-descriptors (descriptors slot-names struct-type no-constructor-p no-destructor-p)
  (check-type descriptors list)
  (loop for descriptor in descriptors
	do (check-foreign-struct-slot-descriptor descriptor slot-names struct-type no-constructor-p no-destructor-p)))

(defun extract-descriptor-info (slot-name descriptor keyword)
  "Return a list with four elements: The slot name, whether the slot is invisible, the option expr, whether the option is used."
  (cond
    ((null descriptor) (list slot-name t nil nil))
    ((symbolp descriptor) (list slot-name nil nil nil))
    (t (let ((key-expr (member keyword descriptor)))
	 (list slot-name nil (cadr key-expr) (and key-expr t))))))

(defun create-constructor-code (constructor-infos pointer-slots name-infos initform-infos type-infos struct-type
				enable-default-constructors enable-invisibles suffix)
  (let ((slot-names (cffi:foreign-slot-names struct-type)))
    (iter
      (for (slot-name invisiblep constructor constructorp) in constructor-infos)
      (when constructor
	(unioning (find-slot-names slot-names (cdr constructor)) into used-slots))
      (when (and enable-default-constructors (not constructorp))
	(collect slot-name into used-slots))
      (when (and (or enable-invisibles (not invisiblep))
		 (or constructor (and enable-default-constructors (not constructorp))))
	(let ((typep (member slot-name type-infos :key #'car)))
	  (when typep 
	    (collect `(type (,(cadar typep) ,(caar typep))) into type-declarations)))
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
		   (arg (gensym))
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
			      ,(format nil "Constructor of ~s." suffix)
			      (declare ,@type-declarations)
			      (let ((,object-sym (cffi:foreign-alloc ',struct-type)))
				(memset ,object-sym 0 (cffi:foreign-type-size ',struct-type))
				(cffi:with-foreign-slots (,final-used-slots ,object-sym ,struct-type)
				  ,@constructor-expressions)
				(values ,object-sym))))))))))

(defun create-destructor-code (destructor-infos pointer-slots struct-type suffix)
  (let ((slot-names (cffi:foreign-slot-names struct-type)))
    (iter (for (nil nil destructor destructorp) in destructor-infos)
      (when destructorp
	(collect destructor into destructor-exprs)
	(unioning (find-slot-names slot-names destructor) into used-slots))
      (finally (return (let ((final-used-slots (mapcar (lambda (x) (if (member x pointer-slots)
								       (list :pointer x)
								       x))
						       used-slots)))
			 (with-gensyms (arg)
			   `(adp:defun ,(intern (concatenate 'string "DESTROY-" (symbol-name suffix))) (,arg)
			      ,(format nil "Destructor of ~s." suffix)
			      ,(when final-used-slots
				 `(cffi:with-foreign-slots (,final-used-slots ,arg ,struct-type)
				    ,@destructor-exprs))
			      (cffi:foreign-free ,arg)))))))))

(defun create-with-code (suffix)
  (let ((with-macro-name (intern (concatenate 'string "WITH-" (symbol-name suffix))))
	(create-name (intern (concatenate 'string "CREATE-" (symbol-name suffix))))
	(destroy-name (intern (concatenate 'string "DESTROY-" (symbol-name suffix)))))
    `(defwith ,with-macro-name ,create-name ,destroy-name 1
	      ,(format nil "Wrap the body forms with ~s and ~s." create-name destroy-name))))

(defun create-readers-code (reader-infos pointer-slots name-infos struct-type
			    enable-default-readers enable-invisibles prefix)
  (let ((slot-names (cffi:foreign-slot-names struct-type)))
    (iter (for (slot-name invisiblep reader readerp) in reader-infos)
      (when (and (or enable-invisibles (not invisiblep))
		 (or reader (and enable-default-readers (not readerp))))
	(let* ((object-arg (gensym))
	       (args (cons object-arg (if reader (car reader) nil)))
	       (final-reader (if reader (cons 'progn (cdr reader)) slot-name))
	       (used-slots (find-slot-names slot-names final-reader))
	       (final-used-slots (mapcar (lambda (x) (if (member x pointer-slots)
							 (list :pointer x)
							 x))
					 used-slots))
	       (namep (member slot-name name-infos :key #'car))
	       (name (if namep (cadar namep) slot-name)))
	  (collect `(adp:defun ,(intern (concatenate 'string (symbol-name prefix) "-" (symbol-name name)))
		      ,args
		      (cffi:with-foreign-slots (,final-used-slots ,(car args) ,struct-type)
			,final-reader))))))))

(defun create-writers-code (writer-infos pointer-slots name-infos type-infos struct-type
			    enable-default-writers enable-invisibles prefix)
  (let ((slot-names (cffi:foreign-slot-names struct-type)))
    (iter (for (slot-name invisiblep writer writerp) in writer-infos)
      (when (and (or enable-invisibles (not invisiblep))
		 (or writer (and enable-default-writers (not writerp))))
	(let* ((object-arg (gensym))
	       (new-value-arg (if writer (caar writer) (gensym)))
	       (args (if writer
			 `(,new-value-arg ,object-arg ,@(cdar writer))
			 `(,new-value-arg ,object-arg)))
	       (final-writer (if writer (cons 'progn (cdr writer)) `(setf ,slot-name ,new-value-arg)))
	       (used-slots (find-slot-names slot-names final-writer))
	       (final-used-slots (mapcar (lambda (x) (if (member x pointer-slots)
							 (list :pointer x)
							 x))
					 used-slots))
	       (namep (member slot-name name-infos :key #'car))
	       (name (if namep (cadar namep) slot-name))
	       (typep (member slot-name type-infos :key #'car))
	       (type-declaration `(type ,(cadar typep) ,(caar typep))))
	  (collect `(adp:defun (setf ,(intern (concatenate 'string (symbol-name prefix) "-" (symbol-name name))))
		      ,args
		      (declare ,type-declaration)
		      (cffi:with-foreign-slots (,final-used-slots ,object-arg ,struct-type)
			,final-writer))))))))

(defun define-foreign-struct-aux (struct-type infix options &rest slot-descriptors)
  (let ((no-constructorp      (member :no-constructor options))
	(no-destructorp       (member :no-destructor options))
	(default-constructorp (member :default-constructors options))
	(default-readerp      (member :default-readers options))
	(default-writerp      (member :default-writers options))
	(invisiblesp          (member :include-invisibles options)))
    (loop for slot-name in (cffi:foreign-slot-names struct-type)
	  for pre-slot-descriptor = (car (member slot-name slot-descriptors :key (lambda (x) (if (listp x) (car x) x))))
	  for slot-descriptor =     (if (listp pre-slot-descriptor) pre-slot-descriptor (list pre-slot-descriptor))
	  for pointerp =       (member :pointer slot-descriptor)
	  for pointer-value =  (cadr pointerp)
	  for namep =          (member :name slot-descriptor)
	  for name-value =     (cadr namep)
	  for typep =          (member :type slot-descriptor)
	  for type-value =     (cadr typep)
	  for initformp =      (member :initform slot-descriptor)
	  for initform-value = (cadr initformp)
	  when pointer-value
	    collect slot-name                                                         into pointer-slots
	  when namep
	    collect (list slot-name name-value)                                       into name-infos
	  when typep
	    collect (list slot-name type-value)                                       into type-infos
	  when initformp
	    collect (list slot-name initform-value)                                   into initform-infos
	  when (not no-constructorp)
	    collect (extract-descriptor-info slot-name slot-descriptor :constructor)  into constructor-infos
	  when (not no-destructorp)
	    collect (extract-descriptor-info slot-name slot-descriptor :destructor)   into destructor-infos
	  collect (extract-descriptor-info slot-name slot-descriptor :reader)     into reader-infos
	  collect (extract-descriptor-info slot-name slot-descriptor :writer)     into writer-infos
	  finally (return `(progn
			     ,@(unless no-constructorp
				 (list (create-constructor-code constructor-infos pointer-slots name-infos
								initform-infos type-infos struct-type
								default-constructorp
								invisiblesp
								infix)))
			     ,@(unless no-destructorp
				 (list (create-destructor-code destructor-infos pointer-slots struct-type infix)))
			     ,@(unless (or no-constructorp
					   no-destructorp)
				 (list (create-with-code infix)))
			     ,@(create-readers-code reader-infos pointer-slots name-infos struct-type
						    default-readerp invisiblesp infix)
			     ,@(create-writers-code writer-infos pointer-slots name-infos type-infos struct-type
						     default-writerp invisiblesp infix))))))

(adp:defmacro define-foreign-struct (struct-type infix options &body slot-descriptors)
  "Define a wraper around the foreign struct (or union) STRUCT-TYPE. The symbol INFIX is used to generate the function names.
OPTIONS is a list of keywords that modify the way this macro works. 

  OPTIONS ::= [[OPTION]]
  OPTION  ::= :no-constructor | :no-destructor | :default-constructors | :default-readers | :default-writers | :include-invisibles

If :no-constructor is specified, then this macro will not define a constructor. In the same way, if :no-destructor is specified
no destructor will be defined. If :default-constructors is used each slot member has no need to define an explicit constructor. Instead,
a default initialization of the foreign slot is done (no translation). The same occurs with :default-readers and :default-writers when
defining the accessors. You will need to write every slot you want to define a constructor, destructor, reader or writer for. However,
if :include-invisibles is used that functions are defined even if you don't specify the existence of that slot.

The SLOT-DESCRIPTIONS specify the translations of each foreign member. The systaxis is as follows:

  SLOT-DESCRIPTIONS ::= (SLOT-NAME [[SLOT-OPTION]])
  SLOT-OPTION       ::= :name NAME | :type TYPE | :pointer POINTER | :virtual VIRTUAL | :constructor CONSTRUCTOR | :destructor DESTRUCTOR | :reader READER | :writer WRITER

The SLOT-NAME is a symbol denoting a foreign member of the struct STRUCT-TYPE (unless :virtual is used). The different options allow you to 
specify the translations of each member:

  - NAME: If used, that symbol will be used to create the accessors names. 
  - TYPE: If used, specify the Lisp type of SLOT-NAME. 
  - CONSTRUCTOR: This option is used to specify a translation when receiving a parameter in the constructor. If the expression after :constructor
                 is NIL or neither this option nor :default-constructors are used a constructor will not be defined. If this option is not used
                 but :default-constructors is, a parameter is still received to initialize the foreign member but no translation will be performed.
                 The expression after :constructor, if not NIL, must be a list where the first element is NIL or a list with a symbol. If NIL,
                 no argument will be received in the constructor. Otherwise, an argument with name the specified symbol will be received. In both cases,
                 the rest of elements of the list after :constructor must be expressions to initialize the member SLOT-NAME (using setf, for example).
                 It is possible to use the arguments specified from other :constructor options. Also, every SLOT-NAME is accessible from here.
  - DESTRUCTOR: This must be an expression doing something with SLOT-NAME, like cffi:free-ing it.
  - READER: This option works the same as :constructor about when an accessor is defined or not. Depending on the expression after :reader and
            :default-readers the accessor may be defined or not. If the expression after :reader is not NIL, then it must be a list where the first
            element is the lambda list of the accessor. The rest of elements are forms that must return the desired value. Every SLOT-NAME is accessible.
  - WRITER: Same as :reader but indicates that the accessor is setf-able. The same rules apply here when using or not :default-writers.
            The expression after :writer, if not NIL, must be a list where its first element is the lambda list of the setf function. So, a first
            symbol is required always denoting the new value. The rest of elements are forms that should modify SLOT-NAME. Every SLOT-NAME is accessible.
  - POINTER: Some structs have as a member another struct. You may want to use its pointer rather than the struct itself. To do that you only need to
             use a non-NIL expression after :pointer.
  - VIRTUAL: Using :virtual you can receive an additional parameter in the constructor, or create additional accessors."
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
