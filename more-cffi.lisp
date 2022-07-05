

(in-package :mcffi)


;; ----------------------------
;; ----- Helper functions -----
;; ----------------------------

;; memset from C standard library (parece ser que puede dar problemas, mejor usar zero-struct)
(cffi:defcfun "memset" :pointer
  (str :pointer) (c :int) (n :size))

(defun remove-property (keyword l)
  (cond
    ((null l)             nil)
    ((eq (car l) keyword) (cddr l))
    (t                    (cons (car l) (remove-property keyword (cdr l))))))


;; Returns the list of symbols from slot-names that appear in expr
(defun find-slot-names (slot-names expr)
  (labels ((find-slot-names-aux (names l names-found)
	         (cond
                   ((and (symbolp l) (member l names)) (adjoin l names-found))
                   ((consp l) (find-slot-names-aux names (cdr l)
						   (find-slot-names-aux names (car l) names-found)))
                   (t names-found))))
    (find-slot-names-aux slot-names expr nil)))


;; Recursively search for the values from sums in l
(defun exists-rec (syms l)
  (if (consp l)
    (or (exists-rec syms (car l)) (exists-rec syms (cdr l)))
    (member l syms)))


;; Mostly like member but recursively
(defun find-many-rec (sym l)
  (if (listp l)
      (if (eq sym (car l))
	  (list l)
	  (iter (for elem in l)
	    (appending (find-many-rec sym elem))))
    nil))


;; Substitute every ocurrence of each symbol in assoc-symbol by
;; its associated symbol
(defun rec-substitute (assoc-symbols l)
  (cond
    ((and (symbolp l) (member l assoc-symbols)) (getf assoc-symbols l))
    ((consp l) (cons (rec-substitute assoc-symbols (car l)) (rec-substitute assoc-symbols (cdr l))))
    (t l)))


;; -------------------------------------------
;; ----- Documentation default functions -----
;; -------------------------------------------

;; Advance global option declaration
(defparameter *enable-doc-generation* nil)


(defun doc-header-default (name file)
  (when *enable-doc-generation*
    (format file "# ~a~%~%" name)))

(defun doc-subheader-default (name file)
  (when *enable-doc-generation*
    (format file "## ~a~%~%" name)))

(defun doc-subsubheader-default (name file)
  (when *enable-doc-generation*
    (format file "### ~a~%~%" name)))

(defun doc-note-default (note file)
  (when *enable-doc-generation*
    (format file "* **Note**: ~a~%~%" note)))

(defun doc-defwith-default (name create destroy file)
  (let ((name-name (if (symbolp name) (string-downcase (string name)) name))
	(create-name (if (symbolp create) (string-downcase (string create)) create))
	(destroy-name (if (symbolp destroy) (string-downcase (string destroy)) destroy)))
    (format file "**~a**~%```lisp~%(~a var-or-vars (&rest args)~%  &body body)~%```~%"
	    name-name name-name)
    (format file "Wrap the body expressions with `~a` and `~a`."
	    create-name destroy-name)
    (format file " The new object(s) is(are) bound to var.")
    (format file " The arguments `args` are passed to the constructor.")
    (format file "~%~%")))

(defun doc-foreign-function-default (name args type-decls result-decls file)
  (let* ((name-name (if (symbolp name) (string-downcase (string name)) name))
	 (arg-names-assoc (iter (for type-decl in type-decls)
			    (appending (list (if (symbolp (car type-decl))
						 (car type-decl)
						 (intern (string-upcase (car type-decl))))
					     (if (stringp (car type-decl))
						 (car type-decl)
						 (string-downcase (string (car type-decl))))))))
	 (arg-names (rec-substitute arg-names-assoc args)))
    (format file "**~a**~%```lisp~%(~a" name-name name-name)
    (when args
      (format file "~{ ~a~}" arg-names))
    (format file ")")
    (when result-decls
      (format file " => ")
      (if (> (length result-decls) 1)
	  (progn
	    (format file "(values")
	    (iter (for type-decl in result-decls)
	      (format file " ~a" (string-downcase (string (car type-decl)))))
	    (format file ")"))
	  (format file "~a" (string-downcase (string (caar result-decls))))))
    (format file "~%")
    (format file "```~%~%")
    (when type-decls
      (format file "* *Parameters*:~%")
      (iter (for type-decl in type-decls)
	(format file "  * *~a*: `~a`~%"
		(let ((arg-name (car type-decl)))
		  (if (symbolp arg-name)
		      (string-downcase (string arg-name))
		      arg-name))
		(let ((type (cadr type-decl)))
		  (if (symbolp type)
		      (string-downcase (string type))
		      type)))
	(finally (format file "~%"))))
    (when result-decls
      (format file "* *Return:*~%")
      (iter (for result-decl in result-decls)
	(format file "  * *~a*: `~a`~%"
		(let ((result-name (car result-decl)))
		  (if (symbolp result-name)
		      (string-downcase (string result-name))
		      result-name))
		(let ((type (cadr result-decl)))
		  (if (symbolp type)
		      (string-downcase (string type))
		      type)))
	(finally "~%")))))

(defun doc-foreign-struct-default (doc-create-info doc-destroy-info doc-accessors-info type-infos
				   type infix file)
  (destructuring-bind (no-constructor-p constructor-parameters) doc-create-info
    (let* ((type-name (if (symbolp type) (string-downcase (string type)) type))
	   (infix-name (if (symbolp infix) (string-downcase (string infix)) infix))
	   (constructor-name (concatenate 'string "create-" infix-name))
	   (destructor-name (concatenate 'string "destroy-" infix-name))
	   (no-destructor-p doc-destroy-info))
      (doc-subsubheader type-name file)
      (let* ((struct-members (union (mapcar #'car constructor-parameters)
				    (mapcar #'car doc-accessors-info)
				    :test (lambda (x y) (string= (string-upcase (string x))
								 (string-upcase (string y)))))))
	(when struct-members
	  (format file "**Members**~%")
	  (iter (for struct-member in struct-members)
	    (format file "* *~a*" (if (symbolp struct-member)
				      (string-downcase (string struct-member))
				      struct-member))
	    (let ((type-infop (member (if (symbolp struct-member)
					  struct-member
					  (intern (string-upcase struct-member)))
				      type-infos :key #'car)))
		  (when type-infop
		    (let ((type-info (cadar type-infop)))
		      (format file ": `~a`" (if (stringp type-info)
						type-info
						(string-downcase (format nil "~a" type-info)))))))
	    (format file "~%"))
	  (format file "~%")))
      (when (not no-constructor-p)
	(format file "**~a**~%```lisp~%(~a"
		constructor-name constructor-name)
	(when constructor-parameters
	  (format file " &key~%")
	  (iter (for param in constructor-parameters)
		(destructuring-bind (keyword init-form) param
		  (let ((keyword-name (if (symbolp keyword) (string-downcase (string keyword)) keyword))
			(num-spaces (+ (length constructor-name) 4)))
		    (format file (concatenate 'string "~" (write-to-string num-spaces) "T(~a ~a)~%")
			    keyword-name init-form))))
	  (format file ")~%```~%~%")))
    (when (not no-destructor-p)
      (format file "**~a**~%```lisp~%(~a obj)~%```~%~%"
	      destructor-name destructor-name))
    (when (and (not no-constructor-p) (not no-constructor-p))
      (doc-defwith (concatenate 'string "with-" infix-name) constructor-name destructor-name file))
    (when doc-accessors-info
      (format file "**Accessors**~%```lisp~%")
      (iter (for doc-accessor-info in doc-accessors-info)
	    (destructuring-bind (slot-name rest-parameters setf-ablep) doc-accessor-info
	      (let ((get-name (concatenate 'string infix-name "-" (if (symbolp slot-name)
								      (string-downcase (string slot-name))
								      slot-name))))
		(format file "(~a obj~{ ~a~})" get-name rest-parameters)
		(if setf-ablep
		    (format file " ; setf-able"))
		(format file "~%"))))
      (format file "```~%~%")))))


;; ----------------------------------------
;; ----- Documentation global options -----
;; ----------------------------------------

(defparameter *doc-header-proc* #'doc-header-default)
(defparameter *doc-subheader-proc* #'doc-subheader-default)
(defparameter *doc-subsubheader-proc* #'doc-subsubheader-default)
(defparameter *doc-note-proc* #'doc-note-default)
(defparameter *doc-defwith-proc* #'doc-defwith-default)
(defparameter *doc-foreign-function-proc* #'doc-foreign-function-default)
(defparameter *doc-foreign-struct-proc* #'doc-foreign-struct-default)


;; -----------------------------------
;; ----- Documentation functions -----
;; -----------------------------------

(defun doc-header (name file)
  (funcall *doc-header-proc* name file))

(defun doc-subheader (name file)
  (funcall *doc-subheader-proc* name file))

(defun doc-subsubheader (name file)
  (funcall *doc-subsubheader-proc* name file))

(defun doc-note (name file)
  (funcall *doc-note-proc* name file))

(defun doc-defwith (name create destroy file)
  (funcall *doc-defwith-proc* name create destroy file))

(defun doc-foreign-function (name args type-decls result-decls file)
  (funcall *doc-foreign-function-proc* name args type-decls result-decls file))

(defun doc-foreign-struct (doc-create-info doc-destroy-info doc-accessors-info type-infos
			   type infix file)
  (funcall *doc-foreign-struct-proc* doc-create-info doc-destroy-info doc-accessors-info
	   type-infos type infix file))


;; -------------------
;; ----- defwith -----
;; -------------------

;; Defines a with macro named name, using a constructor and a destructor
;; The constructor can receive zero or more arguments and can return one or more values
;; The destructor must receive 'destructor-arity' arguments. These arguments are the first values the
;; constructor returns
;; The resulting macro binds some vars to the results from the constructor. These vars can be fewer than the returned values
(defmacro defwith (name file create destroy &key (destructor-arity 1) (destructor-arguments nil))
  (with-gensyms ((var "var") (var-list "var-list") (args "args") (ret-list "ret-list") (body "body"))
    `(progn
       (defmacro ,name (,var ,args &body ,body)
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
					    ``(subseq ,,ret-list 0 ,',destructor-arity))))))))
       ,@(if (and *enable-doc-generation* file)
	     `((doc-defwith ',name ',create ',destroy ,file))
	     nil))))


;; --------------------------------
;; ----- def-foreign-function -----
;; --------------------------------

(defun check-name (name)
  (unless (or (symbolp name) (stringp name))
    (error "Expected a symbol or a string.~%Found:~%   ~S" name)))

(defun check-type-declarations (decl name args)
  (when (and (listp decl) (string= (string (car decl)) "DECLARE-TYPES"))
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
	  (unless (or (symbolp (car type-decl)) (stringp (car type-decl)))
	    (error "Expected a symbol or string at the start of a type declaration in ~a.~%Found:~%   ~a"
		   name type-decl))
	  (iter (for sym in (cdr type-decl))
	    (unless (or (symbolp sym) (stringp sym))
	      (error "Expected a symbol in ~a type declaration.~%Found:~%   ~a" name type-decl))
	    (when (= return-found 0)
	      (let ((sym-sym (if (symbolp sym) sym (intern (string-upcase sym)))))
		(unless (exists-rec (list sym-sym) args)
		  (error "There is no symbol ~a in arguments from ~a." sym name))))))))))

(defun extract-type-declarations (decl)
  (if (string= (string (car decl)) "DECLARE-TYPES")
      (iter outer (for type-decl in (cdr decl))
	(until (eq type-decl :return))
	(iter (for var in (cdr type-decl))
	  (in outer (collect (list var (car type-decl))))))
      nil))

(defun extract-return-declarations (decl)
  (if (string= (string (car decl)) "DECLARE-TYPES")
      (let ((type-decls (cdr (member :return decl))))
	(iter outer (for type-decl in type-decls)
	  (iter (for var in (cdr type-decl))
	    (in outer (collect (list var (car type-decl)))))))
      nil))

(defmacro def-foreign-function (name file args &body exprs)
  (check-name name)
  (let ((name-sym (if (symbolp name) name (intern (string-upcase name)))))
    (check-type-declarations (car exprs) name args)
    (let* ((type-declarations (extract-type-declarations (car exprs)))
	   (return-declarations (extract-return-declarations (car exprs)))
	   (final-body (if (or type-declarations return-declarations) (cdr exprs) exprs)))
      `(progn
	 (defun ,name-sym ,args ,@final-body)
	 ,@(if (and *enable-doc-generation* file)
	       `((doc-foreign-function ',name ',args ',type-declarations ',return-declarations ,file))
	       nil)))))


;; ------------------------------
;; ----- def-foreign-struct -----
;; ------------------------------

(defun check-struct-type (type)
  (unless (or (symbolp type)
	      (stringp type))
    (error "Expected a string or symbol designating a cffi struct type.~%Found:~%   ~S" type)))

(defun check-infix (infix)
  (unless (or (symbolp infix) (stringp infix))
    (error "Expected a symbol.~%Found:~%   ~S" infix)))

(defun check-options (options)
  (unless (listp options)
    (error "Expected a list.~%Found:~%   ~S" options))
  (iter (for option in options)
    (unless (member option '(:no-constructor :no-destructor :enable-default-get
			     :enable-default-set :enable-default-create :include-invisibles))
      (error "Expected :no-constructor, :no-destructor, :enable-default-get, :enable-default-set, :enable-default-create or :include-invisibles.~%Found:~%   ~S"
	     option))))

(defun check-name-option (name slot-name)
  (unless (or (symbolp name)
	      (stringp name))
    (error "Expected a symbol or a string in ~a descriptor.~%Found:~%   ~S"
	   slot-name name)))

;; (defun check-type-option (type slot-name)
;;   (unless (or (symbolp type)
;; 	      (stringp type))
;;     (error "Expected a symbol or a string in ~a descriptor.~%Found:~%   ~S"
;; 	   slot-name type)))

(defun check-init-form-option (create-optionp create-option name slot-name)
  (when create-optionp
    (let ((name-sym (if (symbolp name) name (intern (string-upcase name)))))
      (unless (exists-rec (list name-sym) create-option)
	(error "If the name ~a is not used in ~a descriptor, init-form is forbidden."
	       name slot-name)))))

(defun check-destroy-option (destroy slots slot-member)
  (unless (exists-rec slots destroy)
    (error "Expected the use of at least one slot member in ~a descriptor.~%Found:~%   ~S"
	   slot-member destroy)))

(defun check-get-option (get slot)
  (unless (or (null get)
	      (and (listp get)
		   (listp (car get))
		   (not (null (cadr get)))))
    (error "Expected a get expression ((&rest args) expr) in the ~S descriptor.~%Found:~%   ~S"
	   slot get)))

(defun check-set-option (set slot)
  (unless (or (null set)
	      (and (listp set)
		   (listp (car set))
		   (not (null (cadr set)))
		   (not (member (caar set) '(&optional &key &rest &aux &allow-other-keys)))))
    (error "Expected a set expression ((new-val &rest args) expr) in the ~S descriptor.~%Found:~%   ~S"
	   slot set)))

(defun check-slot-member (slot-member slot-names struct-type)
  (unless (member slot-member slot-names)
    (error "Expected a slot member from ~S~%Found:~%   ~S" struct-type slot-member)))

(defun check-slot-descriptor (descriptor slot-names struct-type no-constructor-p no-destructor-p)
  (if (and (listp descriptor) (not (null descriptor)))
      (progn
	(check-slot-member (car descriptor) slot-names struct-type)
	(when (not (null (cdr descriptor)))
	  (if (and no-constructor-p (member :create descriptor))
	      (error "While :no-constructor is enabled, :create is forbidden. Found :create in ~S descriptor."
		     (car descriptor)))
	  (if (and no-destructor-p (member :destroy descriptor))
	      (error "While :no-destructor is enabled, :destroy is forbidden. Found :destroy in ~S descriptor."
		     (car descriptor)))
	  (iter (for rest-descriptor on (cdr descriptor) by #'cddr)
	    (unless (member (car rest-descriptor) '(:name :type :init-form :pointer :create :destroy :get :set))
	      (error "Expected :name, :type, :init-form, :pointer, :create, :destroy, :get or :set in ~S descriptor.~%Found:~%   ~S"
		     (car descriptor) (car rest-descriptor)))
	    (cond
	      ((eq (car rest-descriptor) :name)
	       (check-name-option (cadr rest-descriptor) (car descriptor)))
	      ;; ((eq (car rest-descriptor) :type)
	      ;;  (check-type-option (cadr rest-descriptor) (car descriptor)))
	      ((eq (car rest-descriptor) :init-form)
	       (let* ((create-optionp (member :create descriptor))
		      (namep (member :name descriptor))
		      (name (if namep (cadr namep) (car descriptor))))
		 (check-init-form-option create-optionp (cadr create-optionp) name (car descriptor))))
	      ((eq (car rest-descriptor) :destroy)
	       (check-destroy-option (cadr rest-descriptor) slot-names (car descriptor)))
	      ((eq (car rest-descriptor) :get)
	       (check-get-option (cadr rest-descriptor) (car descriptor)))
	      ((eq (car rest-descriptor) :set)
	       (check-set-option (cadr rest-descriptor) (car descriptor)))))))
      (check-slot-member descriptor slot-names struct-type)))


(defun check-slot-descriptors (descriptors slot-names struct-type no-constructor-p no-destructor-p)
  (unless (listp descriptors)
    (error "Expected a list of slot descriptors.~%Found:~%   ~S" descriptors))
  (iter (for descriptor in descriptors)
    (check-slot-descriptor descriptor slot-names struct-type no-constructor-p no-destructor-p)))

(defun create-constructor-code (create-infos pointer-slots name-infos init-form-infos struct-type
				enable-default-creates enable-invisibles suffix)
  (iter (for create-info in create-infos)
    (destructuring-bind (slot-name invisiblep create createp) create-info
      (when (and (or enable-invisibles (not invisiblep))
		 (or enable-default-creates createp))
	(if (member slot-name pointer-slots)
	    (collect (list :pointer slot-name) into used-slots)
	    (collect slot-name into used-slots))
	(if createp
	    (progn
	      (when (exists-rec (list slot-name) create)
		(let* ((namep (member slot-name name-infos :key #'car))
		       (keyword (if namep (cadar namep) slot-name))
		       (init-formp (member slot-name init-form-infos :key #'car))
		       (init-form (if init-formp (cadar init-formp) 0)))
		  (collect (list (list (intern (string-upcase (string keyword)) "KEYWORD") slot-name) init-form)
		    into constructor-parameters))
		(let ((new-sym (gensym)))
		  (collect new-sym into constructor-syms)
		  (appending (list slot-name new-sym)
			     into constructor-parameter-syms)))
	      (let ((new-sym (gensym)))
		(collect new-sym into let-syms)
		(collect new-sym into setf-syms))
	      (collect create into let-exprs))
	    (progn
	      (let* ((namep (member slot-name name-infos :key #'car))
		     (keyword (if namep (cadar namep) slot-name))
		     (init-formp (member slot-name init-form-infos :key #'car))
		     (init-form (if init-formp (cadar init-formp) 0)))
		(collect (list (list (intern (string-upcase (string keyword)) "KEYWORD") slot-name) init-form)
		  into constructor-parameters))
	      (let ((new-sym (gensym)))
		(appending (list slot-name new-sym) into constructor-parameter-syms)
		(collect new-sym into setf-syms))))))
    (finally (return `(defun ,(intern (concatenate 'string "CREATE-" (string suffix)))
			  (&key ,@(rec-substitute constructor-parameter-syms constructor-parameters))
			,(let ((object-sym (gensym)))
			   `(let* (,@(mapcar #'list let-syms (rec-substitute constructor-parameter-syms
									     let-exprs))
				   (,object-sym (cffi:foreign-alloc ',struct-type)))
			      (memset ,object-sym 0 (cffi:foreign-type-size ',struct-type))
			      (cffi:with-foreign-slots (,used-slots ,object-sym ,struct-type)
				(setf ,@(apply #'append (mapcar #'list used-slots setf-syms))))
			      (values ,object-sym))))))))

(defun create-destructor-code (destroy-infos pointer-slots struct-type suffix)
  (let ((slot-names (cffi:foreign-slot-names struct-type)))
    (iter (for destroy-info in destroy-infos)
      (destructuring-bind (slot-name invisiblep destroy destroyp) destroy-info
	(declare (ignore slot-name invisiblep))
	(when destroyp
	  (collect destroy into destroy-exprs)
	  (unioning (find-slot-names slot-names destroy) into used-slots)))
      (finally (return (let ((arg (gensym))
			     (final-used-slots (mapcar (lambda (x) (if (member x pointer-slots)
								       (list :pointer x)
								       x))
						       used-slots)))
			 `(defun ,(intern (concatenate 'string "DESTROY-" (string suffix))) (,arg)
			    (cffi:with-foreign-slots (,final-used-slots ,arg ,struct-type)
			      ,@destroy-exprs)
			    (cffi:foreign-free ,arg))))))))

(defun create-with-code (suffix)
  `(defwith ,(intern (concatenate 'string "WITH-" (string suffix))) nil
       ,(intern (concatenate 'string "CREATE-" (string suffix)))
     ,(intern (concatenate 'string "DESTROY-" (string suffix)))))

(defun create-get-codes (get-infos pointer-slots name-infos struct-type
			 enable-default-get enable-invisibles prefix)
  (let ((slot-names (cffi:foreign-slot-names struct-type)))
    (iter (for get-info in get-infos)
      (destructuring-bind (slot-name invisiblep get-expr get-expr-p) get-info
	(when (and (or enable-invisibles (not invisiblep))
		   (or get-expr (and enable-default-get (not get-expr-p))))
	  (let* ((object-arg (gensym))
		 (args (cons object-arg (if get-expr (car get-expr) nil)))
		 (final-get-expr (if get-expr (cons 'progn (cdr get-expr)) slot-name))
		 (used-slots (find-slot-names slot-names final-get-expr))
		 (final-used-slots (mapcar (lambda (x) (if (member x pointer-slots)
							   (list :pointer x)
							   x))
					   used-slots))
		 (namep (member slot-name name-infos :key #'car))
		 (name (if namep (cadar namep) slot-name)))
	    (collect `(defun ,(intern (concatenate 'string (string-upcase (string prefix)) "-"
						   (string-upcase (string name))))
			,args
			(cffi:with-foreign-slots (,final-used-slots ,(car args) ,struct-type)
			  ,final-get-expr)))))))))

(defun create-set-codes (set-infos pointer-slots name-infos struct-type
			 enable-default-set enable-invisibles prefix)
  (let ((slot-names (cffi:foreign-slot-names struct-type)))
    (iter (for set-info in set-infos)
      (destructuring-bind (slot-name invisiblep set-expr set-expr-p) set-info
	(when (and (or enable-invisibles (not invisiblep))
		   (or set-expr (and enable-default-set (not set-expr-p))))
	  (let* ((object-arg (gensym))
		 (new-value-arg (if set-expr (caar set-expr) (gensym)))
		 (args (if set-expr
			   `(,new-value-arg ,object-arg ,@(cdar set-expr))
			   `(,new-value-arg ,object-arg)))
		 (final-set-expr (if set-expr (cons 'progn (cdr set-expr)) `(setf ,slot-name ,new-value-arg)))
		 (used-slots (find-slot-names slot-names final-set-expr))
		 (final-used-slots (mapcar (lambda (x) (if (member x pointer-slots)
							   (list :pointer x)
							   x))
					   used-slots))
		 (namep (member slot-name name-infos :key #'car))
		 (name (if namep (cadar namep) slot-name)))
	    (collect `(defun (setf ,(intern (concatenate 'string (string-upcase (string prefix)) "-"
							 (string-upcase (string name)))))
			,args
			(cffi:with-foreign-slots (,final-used-slots ,object-arg ,struct-type)
			  ,final-set-expr)))))))))

(defun doc-create-info (create-infos name-infos init-form-infos
			no-constructor-p enable-default-creates enable-invisibles)
  (if (not no-constructor-p)
      (iter (for create-info in create-infos)
	(destructuring-bind (slot-name invisiblep create createp) create-info
	  (when (and (or enable-invisibles (not invisiblep))
		     (or enable-default-creates createp))
	    (when (or (and createp (exists-rec (list slot-name) create))
		      (not createp))
	      (let* ((namep (member slot-name name-infos :key #'car))
		     (keyword (if namep (cadar namep) slot-name))
		     (init-formp (member slot-name init-form-infos :key #'car))
		     (init-form (if init-formp (cadar init-formp) 0)))
		(collect (list keyword init-form)
		  into constructor-parameters)))))
	(finally (return (list nil constructor-parameters))))
      (list t nil)))

(defun doc-destroy-info (no-destructor-p)
  no-destructor-p)

(defun doc-accessors-info (get-infos set-infos name-infos default-get default-set enable-invisibles)
  (iter (for get-info in get-infos)
    (destructuring-bind (get-slot-name get-invisiblep get-expr get-expr-p) get-info
      (when (and (or enable-invisibles (not get-invisiblep))
		 (or get-expr (and default-get (not get-expr-p))))
	(let* ((set-info (car (member get-slot-name set-infos :key #'car))))
	  (destructuring-bind (set-slot-name set-invisiblep set-expr set-expr-p) set-info
	    (declare (ignore set-slot-name))
	    (let* ((setf-ablep (and (or enable-invisibles (not set-invisiblep))
				    (or set-expr (and default-set (not set-expr-p)))))
		   (namep (member get-slot-name name-infos :key #'car))
		   (name (if namep (cadar namep) get-slot-name)))
	      (collect (list name
			     (if get-expr-p (car get-expr) nil)
			     setf-ablep)))))))))

;; Returns a list with four elements.
;; 1. The slot name of the descriptor.
;; 2. t if the slot is invisible. Otherwise nil.
;; 3. If the keyword is used, the expression after it. Otherwise nil.
;; 4. If the keyword is used, t. Otherwise nil.
(defun extract-descriptor-info (slot-name descriptor keyword)
  (cond
    ((null descriptor) (list slot-name t nil nil))
    ((symbolp descriptor) (list slot-name nil nil nil))
    (t (let ((key-expr (member keyword descriptor)))
	 (list slot-name nil (cadr key-expr) (and key-expr t))))))

(defmacro def-foreign-struct (type infix file options &body slot-descriptors)
  (check-struct-type type)
  (check-infix infix)
  (check-options options)
  (let ((struct-type (list :struct (if (symbolp type) type (intern (string-upcase type))))))
    (check-slot-descriptors slot-descriptors (cffi:foreign-slot-names struct-type)
			    struct-type
			    (member :no-constructor options) (member :no-destructor options))
    (let ((no-constructorp (member :no-constructor options))
	  (no-destructorp  (member :no-destructor options))
	  (default-createp (member :enable-default-create options))
	  (default-getp    (member :enable-default-get options))
	  (default-setp    (member :enable-default-set options))
	  (invisiblesp     (member :include-invisibles options))
	  (infix-sym       (if (symbolp infix) infix (intern (string-upcase infix)))))
      (iter (for slot-name in (cffi:foreign-slot-names struct-type))
	    (let ((slot-descriptor (car (member slot-name slot-descriptors :key (lambda (x) (if (listp x)
												(car x)
												x))))))
	      (if (and (listp slot-descriptor)
		       (not (null slot-descriptor))
		       (cadr (member :pointer slot-descriptor)))
		  (collect slot-name                                                    into pointer-slots))
	      (if (and (listp slot-descriptor)
		       (not (null slot-descriptor)))
		  (let ((namep (member :name slot-descriptor)))
		    (if namep
			(collect (list slot-name (cadr namep))                          into name-infos))))
	      (if (and (listp slot-descriptor)
		       (not (null slot-descriptor)))
		  (let ((typep (member :type slot-descriptor)))
		    (if typep
			(collect (list slot-name (cadr typep))                          into type-infos))))
	      (if (and (listp slot-descriptor)
		       (not (null slot-descriptor)))
		  (let ((init-formp (member :init-form slot-descriptor)))
		    (if init-formp
			(collect (list slot-name (cadr init-formp))                     into init-form-infos))))
	      (if (not no-constructorp)
		  (collect (extract-descriptor-info slot-name slot-descriptor :create)  into create-infos))
	      (if (not no-destructorp)
		  (collect (extract-descriptor-info slot-name slot-descriptor :destroy) into destroy-infos))
	      (collect (extract-descriptor-info slot-name slot-descriptor :get)         into get-infos)
	      (collect (extract-descriptor-info slot-name slot-descriptor :set)         into set-infos))
	    (finally (return `(progn
				,@(unless no-constructorp
				    (list (create-constructor-code create-infos pointer-slots name-infos
								   init-form-infos struct-type
								   default-createp
								   invisiblesp
								   infix-sym)))
				,@(unless no-destructorp
				    (list (create-destructor-code destroy-infos pointer-slots
								  struct-type infix-sym)))
				,@(unless (or no-constructorp
					      no-destructorp)
				    (list (create-with-code infix)))
				,@(create-get-codes get-infos pointer-slots name-infos struct-type
						    default-getp invisiblesp infix-sym)
				,@(create-set-codes set-infos pointer-slots name-infos struct-type
						    default-setp invisiblesp infix-sym)
				,@(let ((file-sym (gensym)))
				    (when (and *enable-doc-generation* file)
				      `((let ((,file-sym ,file))
					  (doc-foreign-struct ',(doc-create-info create-infos name-infos
										 init-form-infos
										 no-constructorp
										 default-createp
										 invisiblesp)
							      ',(doc-destroy-info no-destructorp)
							      ',(doc-accessors-info get-infos set-infos
										    name-infos
										    default-getp default-setp
										    invisiblesp)
							      ',type-infos ,type ',infix
							      ,file-sym))))))))))))
