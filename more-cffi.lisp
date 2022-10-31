

(in-package :mcffi)

(write-in-file #P"docs/mcffi-api")

;; ----------------------------
;; ----- Helper functions -----
;; ----------------------------

;; memset from C standard library
(cffi:defcfun "memset" :pointer
  (str :pointer) (c :int) (n :size))


;; memcpy from C standard library
(cffi:defcfun "memcpy" :pointer
  (dest :pointer) (src :pointer) (n :size))


;; Return the list of symbols from slot-names that appear in expr
(defun find-slot-names (slot-names expr)
  (labels ((find-slot-names-aux (names l names-found)
	     (cond
               ((and (symbolp l) (member l names)) (adjoin l names-found))
               ((consp l) (find-slot-names-aux names (cdr l) (find-slot-names-aux names (car l) names-found)))
               (t names-found))))
    (find-slot-names-aux slot-names expr nil)))


;; Return a non-nil value if there is some symbol from syms in l
(defun exists-rec (syms l)
  (if (consp l)
      (or (exists-rec syms (car l)) (exists-rec syms (cdr l)))
      (member l syms)))


;; Return a list of lists which their first element is the symbol sym
(defun find-many-rec (sym l)
  (if (listp l)
      (if (eq sym (car l))
	  (list l)
	  (iter (for elem in l)
	    (appending (find-many-rec sym elem))))
      nil))


;; Substitute every ocurrence of each symbol in assoc-symbol by
;; its associated symbol. assoc-symbols is a property list.
(defun rec-substitute (assoc-symbols l)
  (cond
    ((and (symbolp l) (member l assoc-symbols)) (getf assoc-symbols l))
    ((consp l) (cons (rec-substitute assoc-symbols (car l)) (rec-substitute assoc-symbols (cdr l))))
    (t l)))



;; -------------------
;; ----- defwith -----  
;; -------------------

(adp:defmacro defwith (name create destroy &key (destructor-arity 1) destructor-arguments)
  "Define a macro named NAME. This new macro has the following syntax:

  (NAME var-or-vars (&rest args) &body body)

When using this new macro CREATE is called with ARGS and the results are stored in VAR-OR-VARS. If VAR-OR-VARS
is a symbol, the rest of returned values are ignored. Afterwards, the BODY forms are evaluated. Finally, 
DESTROY is called. The arguments used by DESTROY depends on DESTRCUTOR-ARITY and DESTRUCTOR-ARGUMENTS. If
DESTRUCTOR-ARGUMENTS is specified, it must be a list of non-negative integers denoting the arguments returned
by CREATE to be used by DESTROY in the order they appear. For example, using the list (3 0 2) indicates that
DESTROY will receive the fourth, first and third values returned by CREATE and in that order. If this parameter
is not used but DESTRUCTOR-ARITY is, then it must be a non-negative integer indicating the number of arguments
to be used by DESTROY. For example, if 2 is specified as the DESTRUCTOR-ARITY, then
DESTROY will receive the first 2 values returned by CREATE."
  (check-type name symbol)
  (check-type create symbol)
  (check-type destroy symbol)
  (when destructor-arityp
    (check-type destructor-arityp unsigned-byte "a non-negative integer"))
  (when destructor-argumentsp
    (check-type destructor-argumentsp list)
    (loop for dest-arg in destructor-arguments
	  do (check-type dest-arg unsigned-byte "a non-negative integer")))
  (with-gensyms (var args body ret-list-sym var-list)
    `(adp:defmacro ,name (,var ,args &body ,body)
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
					  ``(subseq ,,ret-list-sym 0 ,',destructor-arity))))))))))


;; ----------------------------------------
;; ----- def-foreign-callback-definer -----
;; ----------------------------------------

(defun check-arg-descriptor (arg-descriptor)
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
    (assert typep () "Expected :type and a cffi type in ~s descriptor." slot-name)
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
		(assert (or (null option-value) virtualp (exists-rec (list slot-name) option-value))
			"Create expression must use the ~s argument." slot-name))
	       (:return
		 (assert (exists-rec (list slot-name) option-value)
		   (error "Return expression must use the ~s argument." option-value)))))))

(defun check-arg-descriptors (arg-descriptors)
  (loop for arg-descriptor in arg-descriptors
	do (check-arg-descriptor arg-descriptor)
	count (member :return arg-descriptor) into return-descriptors
	finally (assert (<= return-descriptors 1) ()
			"Expected zero or one return argument. Found ~a" return-descriptors)))

(defun extract-create-arguments (arg-descriptors)
  "Return a list of lists with 5 elements: The slot name, the create expression, the type,
whether is a foreign argument and whether is a lisp argument."
  (loop for arg-descriptor in arg-descriptors
	for slot-name = (car arg-descriptor)
	fot type = (cadr (member :type arg-descriptor))
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
		      (return `(defmacro ,(name-des-symbol name) (,callback-name ,lisp-args &body ,callback-body)
				 (let* ((,user-lisp-args ,(cons 'list lisp-args))
					(,callback-let-create-exprs (mapcar #'list ,user-lisp-args ',callback-create-exprs)))
				   `(cffi:defcallback ,,callback-name ,',return-ftype ,',callback-args-types
				      (let ((,',callback-return-sym (let ,,callback-let-create-exprs
								      ,@,callback-body)))
					,',callback-return-expr))))))))))

(adp:defmacro def-foreign-callback-definer (name &body arg-descriptors)
  "Define a macro named NAME to define callbacks. Each ARG-DESCRIPTOR must have the following syntax:

  ARG-DESCRIPTOR ::= (SLOT-NAME [[slot-option]])
  SLOT-OPTION    ::= {:create expr | :return expr}1 | {:type type}1 | :virtual expr

; Arreglar explicando conceptos de Lisp-args y C-args
The :CREATE option indicates the slot to be an argument in the callback or not. If an expression is non-nil
in :CREATE then it must be a C-to-Lisp translation of that argument. In contrast, if that expression is nil, then
the slot will not be an argument. On the other hand, if :RETURN is used, then the slot will
be the callback returned value. The expression from :RETURN is a Lisp-to-C translation of that value. There can only be
one slot using the :RETURN option. There must be as many slots using :CREATE as arguments the callback has. However, you can
add extra "
  (check-type name symbol)
  (check-arg-descriptors arg-descriptors)
  (create-definer-code name
		       (extract-create-arguments arg-descriptors)
		       (extract-return-argument arg-descriptors)))


;; -------------------
;; ----- defcfun -----
;; -------------------

(adp:defmacro defcfun ((foreign-name name &optional (funcall-name nil)) return-type &body arguments)
  "Define a function or functions that call the foreign function with name FOREIGN-NAME. If NAME is specified, a 
function that call the foreign function is defined. If FUNCALL-NAME is specified, another function is defined 
which receive an extra parameter at the beginning. That extra parameter must be a foreign pointer to the foreign 
function. "
  (check-type foreign-name string)
  (check-type name symbol)
  (check-type funcall-name symbol)
  (loop for argument in arguments
	unless (and (listp argument)
		    (equal (length argument) 2)
		    (symbolp (car argument)))
	  do (error "Expected a list with a symbol and a foreign type. Found: ~s" argument))
  (assert (or name funcall-name) () "MORE-CFFI:defcfun : name and funcall-name are both nil")
  (multiple-value-bind (docstring real-arguments) (if (stringp (car arguments))
						      (values (car arguments) (cdr arguments))
						      (values nil arguments))
    (let ((name-args (mapcar #'car real-arguments))
	  (ordered-args (mapcan (lambda (arg) (list (cadr arg) (car arg))) real-arguments)))
      (with-gensyms (func-ptr)
	`(progn
	   ,@(when name
	       `((defun ,name ,name-args
		   ,@(when docstring `(,docstring))
		   (cffi:foreign-funcall ,foreign-name ,@ordered-args ,return-type))))
	   ,@(when funcall-name
	       `((defun ,funcall-name ,(cons func-ptr name-args)
		   ,@(when docstring `(,docstring))
		   (cffi:foreign-funcall-pointer ,func-ptr () ,@ordered-args ,return-type)))))))))


;; --------------------------------
;; ----- def-foreign-function -----
;; --------------------------------

(defun check-foreign-function-foreign-name (foreign-name)
  (unless (name-desp foreign-name)
    (error "Expected a name designator.~%Found:~%   ~s" foreign-name)))

(defun check-foreign-function-name (name)
  (unless (or (not name) (name-desp name))
    (error "Expected a name designator.~%Found:~%   ~S" name)))

(defun check-foreign-function-funcall-name (name)
  (unless (or (not name) (name-desp name))
    (error "Expected a name designator.~%Found:~%   ~S" name)))

(defun check-foreign-function-type-declarations (decl name args)
  (unless decl
    (error "Expected a DECLARE-TYPES form in the body."))
  (unless (>= (length decl) 2)
    (error "Expected more elements in type declaration."))
  (iter (for rest-decl on (cdr decl))
    (let ((type-decl (car rest-decl)))
      (unless (or (symbolp type-decl) (listp type-decl))
	(error "Expected :return or a list in ~a.~%Found:~%   ~a" name type-decl))
      (when (symbolp type-decl)
	(unless (eq type-decl :return)
	  (error "The only available keyword is :return.~%Found: ~a" type-decl))
	(unless (not (null (cdr rest-decl)))
	  (error "Expected a list after :return in ~a" name))
	(counting type-decl into return-found))
      (when (listp type-decl)
	(unless (>= (length type-decl) 2)
	  (error "The lists must have two or more elements.~%Found in ~a:~%   ~S" name type-decl))
	(unless (type-desp (car type-decl))
	  (error "Expected a type designator at the start of a type declaration in ~a.~%Found:~%   ~a"
		 name type-decl))
	(iter (for sym in (cdr type-decl))
	  (unless (name-desp sym)
	    (error "Expected a name designator in ~a type declaration.~%Found:~%   ~a" name type-decl))
	  (when (= return-found 0)
	    (let ((sym-sym (name-des-symbol sym)))
	      (unless (exists-rec (list sym-sym) args)
		(error "There is no symbol ~a in arguments from ~a." sym-sym name)))))))))

(defun check-function-body (foreign-name body)
  (when foreign-name
    (let ((foreign-sym (name-des-symbol foreign-name)))
      (unless (exists-rec (list foreign-sym) body)
	(error "The foreign function ~s must be used in the body"
	       foreign-sym)))))

(defun extract-foreign-function-type-declarations (body)
  (iter (for type-decl in body)
    (when (and (listp type-decl) (string= (car type-decl) "DECLARE-TYPES"))
      (return type-decl))))

(defun remove-foreign-function-type-declarations (body)
  (remove "DECLARE-TYPES" body :test #'string= :key (lambda (decl)
						      (when (listp decl)
							(car decl)))))

(defun extract-foreign-function-docstring (body)
  (if (stringp (car body))
      (car body)
      nil))

(defun extract-foreign-function-arg-declarations (decl)
  (iter outer (for type-decl in (cdr decl))
    (until (eq type-decl :return))
    (iter (for var in (cdr type-decl))
      (in outer (collect (list var (car type-decl)))))))

(defun extract-foreign-function-return-declarations (decl)
  (let ((type-decls (cdr (member :return decl))))
    (iter outer (for type-decl in type-decls)
      (iter (for var in (cdr type-decl))
	(in outer (collect (list var (car type-decl))))))))

(defmacro def-foreign-function (file (foreign-name name &optional (funcall-name nil)) args &body exprs)
  (check-foreign-function-foreign-name foreign-name)
  (check-foreign-function-name name)
  (check-foreign-function-funcall-name funcall-name)
  (check-function-body foreign-name exprs)
  (let ((decl (extract-foreign-function-type-declarations exprs)))
    (check-foreign-function-type-declarations decl name args)
    (let* ((type-declarations (extract-foreign-function-arg-declarations decl))
	   (return-declarations (extract-foreign-function-return-declarations decl))
	   (docstring (extract-foreign-function-docstring exprs))
	   (name-sym (when name (name-des-symbol name)))
	   (foreign-name-sym (when foreign-name (name-des-symbol foreign-name)))
	   (funcall-name-sym (when funcall-name (name-des-symbol funcall-name)))
	   (foreign-funcall-name-sym (when foreign-name (intern (concatenate 'string "FUNCALL-"
									     (string-upcase (name-des-string foreign-name))))))
	   (func-pointer (gensym))
	   (macrolet-args (gensym))
	   (final-body (if decl
			   (remove-foreign-function-type-declarations exprs)
			   exprs)))
      `(progn
	 ,(when name
	    `(progn
	       (defun ,name-sym ,args ,@final-body)
	       ,(when *export-symbols*
		  `(export ',name-sym))))
	 ,(when funcall-name
	    `(progn
	       (defun ,funcall-name-sym ,(cons func-pointer args)
		 (labels ((,foreign-name-sym (&rest ,macrolet-args)
			    (apply #',foreign-funcall-name-sym ,func-pointer ,macrolet-args)))
		   ,@final-body))
	       ,(when *export-symbols*
		  `(export ',funcall-name-sym))))
	 ,(when (and *doc-generation* file)
	    `(doc-foreign-function ',foreign-name ',name ',docstring ',args ',type-declarations ',return-declarations ,file))))))


;; ------------------------------
;; ----- def-foreign-struct -----
;; ------------------------------

(defun check-struct-type (struct-type)
  (unless (name-desp struct-type)
    (error "Expected a name designator.~%Found:~%   ~S" struct-type)))

(defun check-infix (infix)
  (unless (or (name-desp infix)
	      (listp infix))
    (error "Expected a name designator or a list.~%Found:~%   ~S" infix))
  (when (listp infix)
    (iter (for subinfix in infix)
      (check-infix subinfix))))

(defun check-options (options)
  (unless (listp options)
    (error "Expected a list.~%Found:~%   ~S" options))
  (iter (for option in options)
    (unless (member option '(:no-constructor :no-destructor :default-get
			     :default-set :default-create :include-invisibles))
      (error "Expected :no-constructor, :no-destructor, :default-get, :default-set, :default-create or :include-invisibles.~%Found:~%   ~S"
	     option))))

(defun check-slot-name (slot-name virtualp slot-names struct-type)
  (unless (name-desp slot-name)
    (error "Expected a name designator.~%Found:~%   ~S" slot-name))
  (unless (or virtualp (member (name-des-symbol slot-name) slot-names))
    (error "Expected a slot name from ~a~%Found:~%   ~S" (name-des-string struct-type) slot-name)))

(defun check-name-option (name slot-name)
  (unless (name-desp name)
    (error "Expected a name designator in ~a descriptor.~%Found:~%   ~S"
	   (name-des-string slot-name) name)))

(defun check-type-option (type slot-name)
  (unless (type-desp type)
    (error "Expected a type designator in ~a descriptor.~%Found:~%   ~S"
	   (name-des-string slot-name) type)))

(defun check-init-form-option (create-optionp create-option name slot-name)
  (when create-optionp
    (unless (not (null (car create-option)))
      (error "If the name ~a is not used in ~a descriptor, init-form is forbidden."
	     (name-des-string name) (name-des-string slot-name)))))

(defun check-create-option (create virtualp slot-name)
  (unless (or (null create)
	      (and (listp create)
		   (listp (car create))
		   (<= (length (car create)) 1)
		   (symbolp (caar create))
		   (not (null (cadr create)))))
    (error "Expected a create expression (([arg]) &body body) in ~a descriptor.~%Found:~%   ~S"
	   (name-des-string slot-name) create))
  (unless (or (null (car create))
	      (not (string= (name-des-string slot-name) (caar create))))
    (error "The argument ~a must be different of ~a."
	   (string-downcase (string (car create))) (name-des-string slot-name)))
  (unless (or (null (car create))
	      (exists-rec (list (caar create)) (cdr create)))
    (error "Expected the use of ~a in the create expression of ~a descriptor."
	   (caar create) (name-des-string slot-name)))
  (unless (or virtualp (null create) (exists-rec (list (name-des-symbol slot-name)) (cdr create)))
    (error "Expected the use of ~a in its create expression." (name-des-string slot-name))))

(defun check-destroy-option (destroy slot-name)
  (unless (exists-rec (list (name-des-symbol slot-name)) destroy)
    (error "Expected the use of ~a in its destroy expression." (name-des-string slot-name))))

(defun check-get-option (get virtualp slot-name)
  (unless (or (null get)
	      (and (listp get)
		   (listp (car get))
		   (not (null (cadr get)))))
    (error "Expected a get expression ((&rest args) &body body) in the ~a descriptor.~%Found:~%   ~S"
	   (name-des-string slot-name) get))
  (unless (or virtualp (null get) (exists-rec (list (name-des-symbol slot-name)) get))
    (error "Expected the use of ~a in its get expression." (name-des-string slot-name))))

(defun check-set-option (set virtualp slot-name)
  (unless (or (null set)
	      (and (listp set)
		   (listp (car set))
		   (not (null (car set)))
		   (not (null (cadr set)))
		   (not (member (caar set) '(&optional &key &rest &aux &allow-other-keys)))))
    (error "Expected a set expression ((new-val &rest args) &body body) in the ~a descriptor.~%Found:~%   ~S"
	   (name-des-string slot-name) set))
  (unless (or virtualp (null set) (exists-rec (list (name-des-symbol slot-name)) set))
    (error "Expected the use of ~a in its set expression." (name-des-string slot-name))))

(defun check-slot-descriptor (descriptor slot-names struct-type no-constructor-p no-destructor-p)
  (unless (not (null descriptor))
    (error "Expected a non-nil expression."))
  (if (listp descriptor)
      (progn
	(check-slot-name (car descriptor) (cadr (member :virtual descriptor)) slot-names struct-type)
	(when (not (null (cdr descriptor)))
	  (when (and (cadr (member :virtual descriptor)) (member :destroy descriptor))
	    (error "The :destroy keyword is forbidden for :virtual slots. Found :destroy in ~s descriptor."
		   (car descriptor)))
	  (when (and (cadr (member :virtual descriptor)) (member :pointer descriptor))
	    (error "The :pointer keyword is forbidden for :virtual slots. Found :pointer in ~s descriptor."
		   (car descriptor)))
	  (when (and no-constructor-p (member :create descriptor))
	    (error "While :no-constructor is enabled, :create is forbidden. Found :create in ~S descriptor."
		   (car descriptor)))
	  (when (and no-destructor-p (member :destroy descriptor))
	    (error "While :no-destructor is enabled, :destroy is forbidden. Found :destroy in ~S descriptor."
		   (car descriptor)))
	  (iter (for rest-descriptor on (cdr descriptor) by #'cddr)
	    (unless (member (car rest-descriptor) '(:name :type :init-form :pointer :virtual :create :destroy :get :set))
	      (error "Expected :name, :type, :init-form, :pointer, :virtual, :create, :destroy, :get or :set in ~S descriptor.~%Found:~%   ~S"
		     (car descriptor) (car rest-descriptor)))
	    (cond
	      ((eq (car rest-descriptor) :name)
	       (check-name-option (cadr rest-descriptor) (car descriptor)))
	      ((eq (car rest-descriptor) :type)
	       (check-type-option (cadr rest-descriptor) (car descriptor)))
	      ((eq (car rest-descriptor) :init-form)
	       (let* ((create-optionp (member :create descriptor))
		      (namep (member :name descriptor))
		      (name (if namep (cadr namep) (car descriptor))))
		 (check-init-form-option create-optionp (cadr create-optionp) name (car descriptor))))
	      ((eq (car rest-descriptor) :create)
	       (check-create-option (cadr rest-descriptor) (cadr (member :virtual descriptor)) (car descriptor)))
	      ((eq (car rest-descriptor) :destroy)
	       (check-destroy-option (cadr rest-descriptor) (car descriptor)))
	      ((eq (car rest-descriptor) :get)
	       (check-get-option (cadr rest-descriptor) (cadr (member :virtual descriptor)) (car descriptor)))
	      ((eq (car rest-descriptor) :set)
	       (check-set-option (cadr rest-descriptor) (cadr (member :virtual descriptor)) (car descriptor)))))))
      (check-slot-name descriptor nil slot-names struct-type)))


(defun check-slot-descriptors (descriptors slot-names struct-type no-constructor-p no-destructor-p)
  (unless (listp descriptors)
    (error "Expected a list of slot descriptors.~%Found:~%   ~S" descriptors))
  (iter (for descriptor in descriptors)
    (check-slot-descriptor descriptor slot-names struct-type no-constructor-p no-destructor-p)))

;; Returns a list with four elements.
;; 1. The slot name of the descriptor.
;; 2. t if the slot is invisible. Otherwise nil.
;; 3. If the keyword is used, the expression after it. Otherwise nil.
;; 4. If the keyword is used, t. Otherwise nil.
(defun extract-descriptor-info (slot-name-sym descriptor keyword)
  (cond
    ((null descriptor) (list slot-name-sym t nil nil))
    ((name-desp descriptor) (list slot-name-sym nil nil nil))
    (t (let ((key-expr (member keyword descriptor)))
	 (list slot-name-sym nil (cadr key-expr) (and key-expr t))))))

;; Return a list with two values
;; 1. t if :no-constructor was used. nil otherwise.
;; 2. A list of pairs (keyword-sym init-form). The keyword-sym is symbol
;; denotes the keyword that will be used in a constructor parameter.
(defun doc-create-info (create-infos name-infos init-form-infos
			no-constructor-p enable-default-creates enable-invisibles)
  (list no-constructor-p
	(if (not no-constructor-p)
	    (iter (for create-info in create-infos)
	      (destructuring-bind (slot-name-sym invisiblep create createp) create-info
		(when (and (or enable-invisibles (not invisiblep))
			   (or enable-default-creates createp))
		  (when (or (and createp (not (null (car create))))
			    (not createp))
		    (let* ((namep (member slot-name-sym name-infos :key #'car))
			   (keyword-sym (if namep (cadar namep) slot-name-sym))
			   (init-formp (member slot-name-sym init-form-infos :key #'car))
			   (init-form (if init-formp (cadar init-formp) 0)))
		      (collect (list keyword-sym init-form) into constructor-parameters)))))
	      (finally (return constructor-parameters)))
	    nil)))

;; Retur t if :no-destructor was used. nil otherwise
(defun doc-destroy-info (no-destructor-p)
  no-destructor-p)

;; Return a list of lists of 3 elements
;; 1. The suffix of the getter (it is a name designator)
;; 2. The additional getter parameters
;; 3. A non-nil value if the getter is setf-able
(defun doc-accessors-info (get-infos set-infos name-infos default-get default-set enable-invisibles)
  (iter (for get-info in get-infos)
    (destructuring-bind (get-slot-name-sym get-invisiblep get-expr get-expr-p) get-info
      (when (and (or enable-invisibles (not get-invisiblep))
		 (or get-expr (and default-get (not get-expr-p))))
	(let* ((set-info (car (member get-slot-name-sym set-infos :key #'car))))
	  (destructuring-bind (set-slot-name-sym set-invisiblep set-expr set-expr-p) set-info
	    (declare (ignore set-slot-name-sym))
	    (let* ((setf-ablep (and (or enable-invisibles (not set-invisiblep))
				    (or set-expr (and default-set (not set-expr-p)))))
		   (namep (member get-slot-name-sym name-infos :key #'car))
		   (name (if namep (cadar namep) get-slot-name-sym)))
	      (collect (list name (if get-expr-p (car get-expr) nil) setf-ablep)))))))))

(defun create-constructor-code (struct-or-union create-infos pointer-slots name-infos init-form-infos struct-type
				enable-default-creates enable-invisibles suffix)
  (let ((slot-names (cffi:foreign-slot-names (list struct-or-union (name-des-symbol struct-type)))))
    (iter (for create-info in create-infos)
      (destructuring-bind (slot-name-sym invisiblep create createp) create-info
	(when create
	  (unioning (find-slot-names slot-names (cdr create)) into used-slots))
	(when (and enable-default-creates (not createp))
	  (collect slot-name-sym into used-slots))
	(when (and (or enable-invisibles (not invisiblep))
		   (or create (and enable-default-creates (not createp))))
	  (if createp
	      (progn
		(when (not (null (car create)))
		  (let* ((namep (member slot-name-sym name-infos :key #'car))
			 (keyword-name (if namep (cadar namep) slot-name-sym))
			 (arg (caar create))
			 (init-formp (member slot-name-sym init-form-infos :key #'car))
			 (init-form (if init-formp (cadar init-formp) 0))
			 (supplied-var (when (eq struct-or-union :union) (gensym))))
		    (collect `((,(name-des-keyword keyword-name) ,arg) ,init-form ,@(when (eq struct-or-union :union) `(,supplied-var)))
		      into constructor-parameters)
		    (if (eq struct-or-union :union)
			(collect `(when ,supplied-var ,@(cdr create)) into create-expressions)
			(appending (cdr create) into create-expressions)))))
	      (progn
		(let* ((namep (member slot-name-sym name-infos :key #'car))
		       (keyword-name (if namep (cadar namep) slot-name-sym))
		       (arg (gensym))
		       (init-formp (member slot-name-sym init-form-infos :key #'car))
		       (init-form (if init-formp (cadar init-formp) 0))
		       (supplied-var (when (eq struct-or-union :union) (gensym))))
		  (collect `((,(name-des-keyword keyword-name) ,arg) ,init-form ,@(when (eq struct-or-union :union) `(,supplied-var)))
		    into constructor-parameters)
		  (collect (if (eq struct-or-union :union)
			       `(when ,supplied-var (setf ,slot-name-sym ,arg))
			       `(setf ,slot-name-sym ,arg))
		    into create-expressions))))))
      (finally (return (let ((final-used-slots (mapcar (lambda (x) (if (member x pointer-slots)
								       (list :pointer x)
								       x))
						       used-slots)))
			 `(defun ,(intern (concatenate 'string "CREATE-" (string-upcase (name-des-string suffix))))
			      (&key ,@constructor-parameters)
			    ,(let ((object-sym (gensym)))
			       `(let ((,object-sym (cffi:foreign-alloc '(,struct-or-union ,(name-des-symbol struct-type)))))
				  (memset ,object-sym 0 (cffi:foreign-type-size '(,struct-or-union ,(name-des-symbol struct-type))))
				  (cffi:with-foreign-slots (,final-used-slots ,object-sym (,struct-or-union ,(name-des-symbol struct-type)))
				    ,@create-expressions)
				  (values ,object-sym))))))))))

(defun create-destructor-code (struct-or-union destroy-infos pointer-slots struct-type suffix)
  (let ((slot-names (cffi:foreign-slot-names (list struct-or-union (name-des-symbol struct-type)))))
    (iter (for destroy-info in destroy-infos)
      (destructuring-bind (slot-name-sym invisiblep destroy destroyp) destroy-info
	(declare (ignore slot-name-sym invisiblep))
	(when destroyp
	  (collect destroy into destroy-exprs)
	  (unioning (find-slot-names slot-names destroy) into used-slots)))
      (finally (return (let ((arg (gensym))
			     (final-used-slots (mapcar (lambda (x) (if (member x pointer-slots)
								       (list :pointer x)
								       x))
						       used-slots)))
			 `(defun ,(intern (concatenate 'string "DESTROY-" (string-upcase (name-des-string suffix)))) (,arg)
			    ,(when final-used-slots
			       `(cffi:with-foreign-slots (,final-used-slots ,arg (,struct-or-union ,(name-des-symbol struct-type)))
				  ,@destroy-exprs))
			    (cffi:foreign-free ,arg))))))))

(defun create-with-code (suffix)
  `(defwith nil ,(intern (concatenate 'string "WITH-" (string-upcase (name-des-string suffix))))
     ,(intern (concatenate 'string "CREATE-" (string-upcase (name-des-string suffix))))
     ,(intern (concatenate 'string "DESTROY-" (string-upcase (name-des-string suffix))))))

(defun create-get-codes (struct-or-union get-infos pointer-slots name-infos struct-type
			 enable-default-get enable-invisibles prefix)
  (let ((slot-names (cffi:foreign-slot-names (list struct-or-union (name-des-symbol struct-type)))))
    (iter (for get-info in get-infos)
      (destructuring-bind (slot-name-sym invisiblep get-expr get-expr-p) get-info
	(when (and (or enable-invisibles (not invisiblep))
		   (or get-expr (and enable-default-get (not get-expr-p))))
	  (let* ((object-arg (gensym))
		 (args (cons object-arg (if get-expr (car get-expr) nil)))
		 (final-get-expr (if get-expr (cons 'progn (cdr get-expr)) slot-name-sym))
		 (used-slots (find-slot-names slot-names final-get-expr))
		 (final-used-slots (mapcar (lambda (x) (if (member x pointer-slots)
							   (list :pointer x)
							   x))
					   used-slots))
		 (namep (member slot-name-sym name-infos :key #'car))
		 (name (if namep (cadar namep) slot-name-sym)))
	    (collect `(defun ,(intern (concatenate 'string (string-upcase (name-des-string prefix)) "-"
						   (string-upcase (name-des-string name))))
			,args
			(cffi:with-foreign-slots (,final-used-slots ,(car args) (,struct-or-union ,(name-des-symbol struct-type)))
			  ,final-get-expr)))))))))

(defun create-set-codes (struct-or-union set-infos pointer-slots name-infos struct-type
			 enable-default-set enable-invisibles prefix)
  (let ((slot-names (cffi:foreign-slot-names (list struct-or-union (name-des-symbol struct-type)))))
    (iter (for set-info in set-infos)
      (destructuring-bind (slot-name-sym invisiblep set-expr set-expr-p) set-info
	(when (and (or enable-invisibles (not invisiblep))
		   (or set-expr (and enable-default-set (not set-expr-p))))
	  (let* ((object-arg (gensym))
		 (new-value-arg (if set-expr (caar set-expr) (gensym)))
		 (args (if set-expr
			   `(,new-value-arg ,object-arg ,@(cdar set-expr))
			   `(,new-value-arg ,object-arg)))
		 (final-set-expr (if set-expr (cons 'progn (cdr set-expr)) `(setf ,slot-name-sym ,new-value-arg)))
		 (used-slots (find-slot-names slot-names final-set-expr))
		 (final-used-slots (mapcar (lambda (x) (if (member x pointer-slots)
							   (list :pointer x)
							   x))
					   used-slots))
		 (namep (member slot-name-sym name-infos :key #'car))
		 (name (if namep (cadar namep) slot-name-sym)))
	    (collect `(defun (setf ,(intern (concatenate 'string (string-upcase (name-des-string prefix)) "-"
							 (string-upcase (name-des-string name)))))
			,args
			(cffi:with-foreign-slots (,final-used-slots ,object-arg (,struct-or-union ,(name-des-symbol struct-type)))
			  ,final-set-expr)))))))))

(defun def-foreign-single-struct (file struct-or-union struct-type infix options &rest slot-descriptors)
  (let ((no-constructorp (member :no-constructor options))
	(no-destructorp  (member :no-destructor options))
	(default-createp (member :default-create options))
	(default-getp    (member :default-get options))
	(default-setp    (member :default-set options))
	(invisiblesp     (member :include-invisibles options)))
    (iter (for slot-name-sym in (cffi:foreign-slot-names (list struct-or-union (name-des-symbol struct-type))))
      (let ((slot-descriptor (car (member slot-name-sym slot-descriptors :key (lambda (x) (if (listp x) (car x) x))))))
	(when (and (listp slot-descriptor)
		   (not (null slot-descriptor))
		   (cadr (member :pointer slot-descriptor)))
	  (collect slot-name-sym                                                  into pointer-slots))
	(when (and (listp slot-descriptor)
		   (not (null slot-descriptor))
		   (cadr (member :virtual slot-descriptor)))
	  (collect slot-name-sym                                                  into virtual-slots))
	(when (and (listp slot-descriptor)
		   (not (null slot-descriptor)))
	  (let ((namep (member :name slot-descriptor)))
	    (when namep
	      (collect (list slot-name-sym (cadr namep))                          into name-infos))))
	(when (and (listp slot-descriptor)
		   (not (null slot-descriptor)))
	  (let ((typep (member :type slot-descriptor)))
	    (when typep
	      (collect (list slot-name-sym (cadr typep))                          into type-infos))))
	(when (and (listp slot-descriptor)
		   (not (null slot-descriptor)))
	  (let ((init-formp (member :init-form slot-descriptor)))
	    (when init-formp
	      (collect (list slot-name-sym (cadr init-formp))                     into init-form-infos))))
	(when (not no-constructorp)
	  (collect (extract-descriptor-info slot-name-sym slot-descriptor :create)  into create-infos))
	(when (not no-destructorp)
	  (collect (extract-descriptor-info slot-name-sym slot-descriptor :destroy) into destroy-infos))
	(collect (extract-descriptor-info slot-name-sym slot-descriptor :get)         into get-infos)
	(collect (extract-descriptor-info slot-name-sym slot-descriptor :set)         into set-infos))
      (finally (return `(progn
			  ,@(unless no-constructorp
			      (list (create-constructor-code struct-or-union create-infos pointer-slots name-infos
							     init-form-infos struct-type
							     default-createp
							     invisiblesp
							     infix)))
			  ,@(when (and *export-symbols* (not no-constructorp))
			      `((export ',(intern (concatenate 'string "CREATE-" (string-upcase (name-des-string infix)))))))
			  ,@(unless no-destructorp
			      (list (create-destructor-code struct-or-union destroy-infos pointer-slots
							    struct-type infix)))
			  ,@(when (and *export-symbols* (not no-destructorp))
			      `((export ',(intern (concatenate 'string "DESTROY-" (string-upcase (name-des-string infix)))))))
			  ,@(unless (or no-constructorp
					no-destructorp)
			      (list (create-with-code infix)))
			  ,@(create-get-codes struct-or-union get-infos pointer-slots name-infos struct-type
					      default-getp invisiblesp infix)
			  ,@(create-set-codes struct-or-union set-infos pointer-slots name-infos struct-type
					      default-setp invisiblesp infix)
			  ,@(when *export-symbols*
			      (let ((slot-names (cffi:foreign-slot-names (list struct-or-union (name-des-symbol struct-type)))))
				(iter (for slot-name in slot-names)
				  (let* ((slot-name-sym (name-des-symbol slot-name))
					 (namep (member slot-name-sym name-infos :key #'car))
					 (name (if namep (cadar namep) slot-name-sym)))
				    (collect `(export ',(intern (concatenate 'string (string-upcase (name-des-string infix)) "-"
									     (string-upcase (name-des-string name))))))))))
			  ,@(let ((file-sym (gensym)))
			      (when (and *doc-generation* file)
				`((let ((,file-sym ,file))
				    (doc-foreign-struct ,struct-or-union ',(doc-create-info create-infos name-infos
									   init-form-infos
									   no-constructorp
									   default-createp
									   invisiblesp)
							',(doc-destroy-info no-destructorp)
							',(doc-accessors-info get-infos set-infos
									      name-infos
									      default-getp default-setp
									      invisiblesp)
							',type-infos ',virtual-slots ,struct-type ',infix
							,file-sym)))))))))))

(defmacro def-foreign-struct (file struct-type infix options &body slot-descriptors)
  (check-struct-type struct-type)
  (check-infix infix)
  (check-options options)
  (check-slot-descriptors slot-descriptors (cffi:foreign-slot-names (list :struct (name-des-symbol struct-type)))
			  struct-type (member :no-constructor options) (member :no-destructor options))
  (if (listp infix)
      `(progn
	 ,@(iter (for subinfix in infix)
	     (collect (apply #'def-foreign-single-struct file :struct struct-type subinfix options slot-descriptors))))
      (apply #'def-foreign-single-struct file :struct struct-type infix options slot-descriptors)))


;; -----------------------------
;; ----- def-foreign-union -----
;; -----------------------------

(defmacro def-foreign-union (file struct-type infix options &body slot-descriptors)
  (check-struct-type struct-type)
  (check-infix infix)
  (check-options options)
  (check-slot-descriptors slot-descriptors (cffi:foreign-slot-names (list :union (name-des-symbol struct-type)))
			  struct-type (member :no-constructor options) (member :no-destructor options))
  (if (listp infix)
      `(progn
	 ,@(iter (for subinfix in infix)
	     (collect (apply #'def-foreign-single-struct file :union struct-type subinfix options slot-descriptors))))
      (apply #'def-foreign-single-struct file :union struct-type infix options slot-descriptors)))
