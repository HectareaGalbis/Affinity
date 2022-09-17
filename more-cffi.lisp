

(in-package :mcffi)


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


;; Name designator predicate
(defun name-desp (name)
  (and (not (null name))
       (or (keywordp name)
	   (symbolp name)
	   (stringp name))))

;; Return t if two symbols of two name designators are the same
;; (defun name= (name1 name2)
;;   (eq (name-des-symbol name1) (name-des-symbol name2)))

;; Turn a name designator into a keyword
(defun name-des-keyword (name)
  (cond
    ((keywordp name) name)
    ((symbolp name) (intern (string name) "KEYWORD"))
    ((stringp name) (intern (string-upcase name) "KEYWORD"))))

;; Turn a name designator into a symbol
(defun name-des-symbol (name)
  (cond
    ((keywordp name) (intern (string name)))
    ((symbolp name) name)
    ((stringp name) (intern (string-upcase name)))))

;; Turn a name designator into a string
(defun name-des-string (name)
  (let ((*print-case* :downcase))
    (princ-to-string name)))


;; Type designator
(defun type-desp (type)
  (or (consp type)
      (stringp type)
      (symbolp type)))

;; Turn a type designator into a string
(defun type-des-string (str)
  (let ((*print-case* :downcase))
    (princ-to-string str)))

;; -------------------------------------------
;; ----- Documentation default functions -----
;; -------------------------------------------

(defun doc-header-default (name file)
  (format file "# ~a~%~%" name))

(defun doc-subheader-default (name file)
  (format file "## ~a~%~%" name))

(defun doc-subsubheader-default (name file)
  (format file "---~%~%### ~a~%~%" name))

(defun doc-note-default (note file)
  (format file "* **Note**: ~a~%~%" note))

(defun doc-lisp-function-default (name args docstring arg-declarations return-declarations file)
  (doc-subheader-default (name-des-string name) file)
  (let* ((name-str (name-des-string name))
	 (arg-names-sym-str (iter (for arg-decl in arg-declarations)
				(appending (list (name-des-symbol (car arg-decl))
						 (name-des-string (car arg-decl))))))
	 (args-str (rec-substitute arg-names-sym-str args)))
    (format file "**~a**~%```lisp~%(~a" name-str name-str)
    (when args
      (format file "~{ ~a~}" args-str))
    (format file ")")
    (when return-declarations
      (format file " => ")
      (if (> (length return-declarations) 1)
	  (progn
	    (format file "(values")
	    (iter (for type-decl in return-declarations)
	      (format file " ~a" (name-des-string (car type-decl))))
	    (format file ")"))
	  (format file "~a" (name-des-string (caar return-declarations)))))
    (format file "~%")
    (format file "```~%~%")
    (when docstring
      (format file "~a~%~%" docstring))
    (when arg-declarations
      (format file "* *Parameters*:~%")
      (iter (for type-decl in arg-declarations)
	(format file "  * *~a*: `~a`~%"
		(name-des-string (car type-decl)) (type-des-string (cadr type-decl)))
	(finally (format file "~%"))))
    (when return-declarations
      (format file "* *Return:*~%")
      (iter (for result-decl in return-declarations)
	(format file "  * *~a*: `~a`~%"
		(name-des-string (car result-decl)) (type-des-string (cadr result-decl)))
	(finally (format file "~%"))))))

(defun doc-lisp-macro-default (name args docstring file)
  (doc-subheader-default (name-des-string name) file)
  (let ((name-str (name-des-string name)))
    (format file "**~a**~%```lisp~%(~a" name-str name-str)
    (when args
      (format file "~{ ~a~}" (mapcar #'string args)))
    (format file ")")
    (format file "~%")
    (format file "```~%~%")
    (when docstring
      (format file "~a~%~%" docstring))))

(defun doc-defwith-default (name create destroy file)
  (let ((name-str (name-des-string name))
	(create-str (name-des-string create))
	(destroy-str (name-des-string destroy)))
    (format file "**~a**~%```lisp~%(~a var (&rest args)~%  &body body)~%```~%"
	    name-str name-str)
    (format file "Wrap the body expressions with `~a` and `~a`."
	    create-str destroy-str)
    (format file " The new object(s) is(are) bound to `var`.")
    (format file " The arguments `args` are passed to the constructor.")
    (format file "~%~%")))

(defun doc-foreign-constant-default (foreign-name name value file)
  (doc-subheader-default foreign-name file)
  (format file "```lisp~%(defconstant ~a ~a)~%```~%~%"
	  (name-des-string name) value))

(defun doc-foreign-enum-default (name descriptors file)
  (doc-subheader-default (name-des-string name) file)
  (format file "```lisp~%~{~a~%~}```~%~%"
	  (mapcar (lambda (x) (list 'defconstant (name-des-string (car x)) (cadr x))) descriptors)))

(defun doc-foreign-callback-definer-default (foreign-type name create-info return-info file)
  (let ((foreign-type-str (name-des-string foreign-type))
	(name-str (name-des-string name))
	(args-str (mapcar (lambda (x) (name-des-string (car x))) create-info))
	(arg-types-str (mapcar (lambda (x) (name-des-string (cadr x))) create-info))
	(arg-typesp (mapcar #'caddr create-info))
	(ret-arg-str (name-des-string (car return-info)))
	(ret-type-str (name-des-string (cadr return-info)))
	(ret-typep (caddr return-info)))
    (format file "**~a**~%```lisp~%(~a (~{~a~^ ~})~% &body body) => ~a~%```~%"
	    foreign-type-str name-str args-str ret-arg-str)
    (format file "Define a callback function.~%~%")
    (when create-info
      (format file "* *Parameters:*~%")
      (iter (for arg-str in args-str)
	(for arg-type-str in arg-types-str)
	(for arg-typep in arg-typesp)
	(format file "  * *~a*" arg-str)
	(when arg-typep
	  (format file ": `~a`" arg-type-str))
	(format file "~%"))
      (format file "~%"))
    (when return-info
      (format file "* *Return:*~%")
      (format file "  * *~a*" ret-arg-str)
      (when ret-typep
	(format file ": `~a`" ret-type-str))
      (format file "~%~%"))))

(defun doc-foreign-function-default (foreign-name name docstring args type-decls result-decls file)
  (when foreign-name (doc-subheader-default (name-des-string foreign-name) file))
  (let* ((name-str (name-des-string name))
	 (typed-names-sym-str (iter (for type-decl in type-decls)
				(appending (list (name-des-symbol (car type-decl))
						 (name-des-string (car type-decl))))))
	 (args-str (rec-substitute typed-names-sym-str args)))
    (format file "**~a**~%```lisp~%(~a" name-str name-str)
    (when args
      (format file "~{ ~a~}" args-str))
    (format file ")")
    (when result-decls
      (format file " => ")
      (if (> (length result-decls) 1)
          (progn
	    (format file "(values")
	    (iter (for type-decl in result-decls)
	      (format file " ~a" (name-des-string (car type-decl))))
	    (format file ")"))
	  (format file "~a" (name-des-string (caar result-decls)))))
    (format file "~%")
    (format file "```~%~%")
    (when docstring
      (format file "~a~%~%" docstring))
    (when type-decls
      (format file "* *Parameters*:~%")
      (iter (for type-decl in type-decls)
	(format file "  * *~a*: `~a`~%"
		(name-des-string (car type-decl)) (type-des-string (cadr type-decl)))
	(finally (format file "~%"))))
    (when result-decls
      (format file "* *Return:*~%")
      (iter (for result-decl in result-decls)
	(format file "  * *~a*: `~a`~%"
		(name-des-string (car result-decl)) (type-des-string (cadr result-decl)))
	(finally (format file "~%"))))))

(defun doc-foreign-macro-default (foreign-name name args docstring file)
  (doc-subheader-default (name-des-string foreign-name) file)
  (let ((name-str (name-des-string name)))
    (format file "**~a**~%```lisp~%(~a" name-str name-str)
    (when args
      (format file "~{ ~a~}" (mapcar #'string args)))
    (format file ")")
    (format file "~%")
    (format file "```~%~%")
    (when docstring
      (format file "~a~%~%" docstring))))

(defun doc-foreign-struct-default (struct-or-union doc-create-info doc-destroy-info doc-accessors-info type-infos virtual-slots
				   type infix file)
  (destructuring-bind (no-constructor-p constructor-parameters) doc-create-info
    (let* ((type-str (name-des-string type))
	   (infix-str (name-des-string infix))
	   (constructor-str (concatenate 'string "create-" infix-str))
	   (destructor-str (concatenate 'string "destroy-" infix-str))
	   (no-destructor-p doc-destroy-info))
      (doc-subsubheader-default type-str file)
      (let* ((struct-members (iter (for member-type in (append (cffi:foreign-slot-names (list struct-or-union (name-des-symbol type))) virtual-slots))
			       (let ((constructor-memberp (member member-type constructor-parameters
								  :key #'car :test #'string-equal))
				     (accessor-memberp (member member-type doc-accessors-info
							       :key #'car :test #'string-equal)))
				 (when (or constructor-memberp accessor-memberp)
				   (collect (or (caar constructor-memberp) (caar accessor-memberp))))))))
	(when struct-members
	  (format file "**Members**~%")
	  (iter (for struct-member in struct-members)
	    (format file "* *~a*" (name-des-string struct-member))
	    (let ((type-infop (member struct-member type-infos :key #'car :test #'string-equal)))
	      (when type-infop
		(format file ": `~a`" (type-des-string (cadar type-infop)))))
	    (format file "~%"))
	  (format file "~%")))
      (when (not no-constructor-p)
	(format file "**~a**~%```lisp~%(~a"
		constructor-str constructor-str)
	(when constructor-parameters
	  (format file " &key~%")
	  (iter (for param in constructor-parameters)
	    (destructuring-bind (keyword init-form) param
	      (let ((keyword-str (name-des-string keyword))
		    (num-spaces (+ (length constructor-str) 4)))
		(when (not (first-iteration-p))
		  (format file "~%"))
		(format file (concatenate 'string "~" (write-to-string num-spaces) "T(~a ~a)")
			keyword-str init-form))))
	  (format file ")~%```~%~%")))
      (when (not no-destructor-p)
	(format file "**~a**~%```lisp~%(~a obj)~%```~%~%"
		destructor-str destructor-str))
      (when (and (not no-constructor-p) (not no-constructor-p))
	(doc-defwith (concatenate 'string "with-" infix-str) constructor-str destructor-str file))
      (when doc-accessors-info
	(format file "**Accessors**~%```lisp~%")
	(iter (for doc-accessor-info in doc-accessors-info)
	  (destructuring-bind (slot-name rest-parameters setf-ablep) doc-accessor-info
	    (let ((get-name (concatenate 'string infix-str "-" (name-des-string slot-name)))
		  (*print-case* :downcase))
	      (format file "(~a obj~{ ~a~})" get-name rest-parameters)
	      (if setf-ablep
		  (format file " ; setf-able"))
	      (format file "~%"))))
	(format file "```~%~%")))))


;; --------------------------
;; ----- Global options -----
;; --------------------------

;; Documentation generation
(defparameter *doc-generation* nil)

(defparameter *doc-header-proc* #'doc-header-default)
(defparameter *doc-subheader-proc* #'doc-subheader-default)
(defparameter *doc-subsubheader-proc* #'doc-subsubheader-default)
(defparameter *doc-note-proc* #'doc-note-default)
(defparameter *doc-lisp-function-proc* #'doc-lisp-function-default)
(defparameter *doc-lisp-macro-proc* #'doc-lisp-macro-default)
(defparameter *doc-defwith-proc* #'doc-defwith-default)
(defparameter *doc-foreign-constant-proc* #'doc-foreign-constant-default)
(defparameter *doc-foreign-enum-proc* #'doc-foreign-enum-default)
(defparameter *doc-foreign-callback-definer-proc* #'doc-foreign-callback-definer-default)
(defparameter *doc-foreign-function-proc* #'doc-foreign-function-default)
(defparameter *doc-foreign-macro-proc* #'doc-foreign-macro-default)
(defparameter *doc-foreign-struct-proc* #'doc-foreign-struct-default)


;; Export symbols
(defparameter *export-symbols* t)


;; -----------------------------------
;; ----- Documentation functions -----
;; -----------------------------------

(defmacro with-doc-file ((file path) &body body)
  (if *doc-generation*
      `(progn
	 (defparameter ,file (open ,path :direction :output :if-exists :supersede :if-does-not-exist :create))
	 ,(cons 'progn body)
	 (close ,file))
      (cons 'progn body)))

(defmacro doc-header (file name)
  (if (and *doc-generation* file)
      `(funcall *doc-header-proc* ,name ,file)))

(defmacro doc-subheader (file name)
  (if (and *doc-generation* file)
      `(funcall *doc-subheader-proc* ,name ,file)))

(defmacro doc-subsubheader (file name)
  (if (and *doc-generation* file)
      `(funcall *doc-subsubheader-proc* ,name ,file)))

(defmacro doc-note (file name)
  (if (and *doc-generation* file)
      `(funcall *doc-note-proc* ,name ,file)))

(defun doc-lisp-function (name args docstring arg-declarations return-declarations file)
  (funcall *doc-lisp-function-proc* name args docstring arg-declarations return-declarations file))

(defun doc-lisp-macro (name args docstring file)
  (funcall *doc-lisp-macro-proc* name args docstring file))

(defun doc-defwith (name create destroy file)
  (funcall *doc-defwith-proc* name create destroy file))

(defun doc-foreign-constant (foreign-name name value file)
  (funcall *doc-foreign-constant-proc* foreign-name name value file))

(defun doc-foreign-enum (name descriptors file)
  (funcall *doc-foreign-enum-proc* name descriptors file))

(defun doc-foreign-callback-definer (foreign-type name create-info return-info file)
  (funcall *doc-foreign-callback-definer-proc* foreign-type name create-info return-info file))

(defun doc-foreign-function (foreign-name name docstring args type-decls result-decls file)
  (funcall *doc-foreign-function-proc* foreign-name name docstring args type-decls result-decls file))

(defun doc-foreign-macro (foreign-name name args docstring file)
  (funcall *doc-foreign-macro-proc* foreign-name name args docstring file))

(defun doc-foreign-struct (struct-or-union doc-create-info doc-destroy-info doc-accessors-info type-infos
			   virtual-slots type infix file)
  (funcall *doc-foreign-struct-proc* struct-or-union doc-create-info doc-destroy-info doc-accessors-info
	   type-infos virtual-slots type infix file))


;; -----------------------------
;; ----- def-lisp-function -----
;; -----------------------------

(defun check-lisp-function-name (name)
  (unless (name-desp name)
    (error "Expected a name designator.~%Found:~%   ~S" name)))

(defun check-lisp-function-type-declarations (decl name args)
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

(defun extract-lisp-function-type-declarations (body)
  (iter (for type-decl in body)
    (when (and (listp type-decl) (string= (car type-decl) "DECLARE-TYPES"))
      (return type-decl))))

(defun remove-lisp-function-type-declarations (body)
  (remove "DECLARE-TYPES" body :test #'string= :key (lambda (decl)
						      (when (listp decl)
							(car decl)))))

(defun extract-lisp-function-docstring (body)
  (if (stringp (car body))
      (car body)
      nil))

(defun extract-lisp-function-arg-declarations (decl)
  (iter outer (for type-decl in (cdr decl))
    (until (eq type-decl :return))
    (iter (for var in (cdr type-decl))
      (in outer (collect (list var (car type-decl)))))))

(defun extract-lisp-function-return-declarations (decl)
  (let ((type-decls (cdr (member :return decl))))
    (iter outer (for type-decl in type-decls)
      (iter (for var in (cdr type-decl))
	(in outer (collect (list var (car type-decl))))))))

(defmacro def-lisp-function (doc-file name args &body body)
  (check-lisp-function-name name)
  (let ((decl (extract-lisp-function-type-declarations body)))
    (check-lisp-function-type-declarations decl name args)
    (let ((arg-declarations (extract-lisp-function-arg-declarations decl))
	  (return-declarations (extract-lisp-function-return-declarations decl))
	  (docstring (extract-lisp-function-docstring body))
	  (final-body (if decl
			  (remove-lisp-function-type-declarations body)
			  body)))
      `(progn
	 (defun ,(name-des-symbol name) ,args
	   ,@final-body)
	 ,@(when *export-symbols*
	     `((export ',(name-des-symbol name))))
	 ,@(when (and *doc-generation* doc-file)
	     `((doc-lisp-function ',name ',args ',docstring ',arg-declarations ',return-declarations ,doc-file)))))))


;; --------------------------
;; ----- def-lisp-macro -----
;; --------------------------

(defun check-lisp-macro-name (name)
  (unless (name-desp name)
    (error "Expected a name designator.~%Found~%:   ~s" name)))

(defun check-lisp-macro-args (args)
  (unless (and (listp args)
	       (iter (for arg in args)
		 (always (symbolp arg))))
    (error "Expected a list of symbols.~%Found:~%   ~s" args)))

(defun extract-lisp-macro-docstring (body)
  (if (stringp (car body))
      (car body)
      nil))

(defmacro def-lisp-macro (file name args &body body)
  (check-lisp-macro-name name)
  (check-lisp-macro-args args)
  (let ((docstring (extract-lisp-macro-docstring body)))
    `(progn
       (defmacro ,(name-des-symbol name) ,args
	 ,@body)
       ,@(when *export-symbols*
	   `((export ',(name-des-symbol name))))
       ,@(when (and *doc-generation* file)
	   `((doc-lisp-macro ',name ',args ',docstring ,file))))))


;; -------------------
;; ----- defcfun -----
;; -------------------

(defun check-defcfun-foreign-name (foreign-name)
  (unless (stringp foreign-name)
    (error "Expected a string. Found: ~s" foreign-name)))

(defun check-defcfun-name (name)
  (unless (symbolp name)
    (error "Expected a symbol. Found: ~s" name)))

(defun check-defcfun-funcall-name (funcall-name)
  (unless (or (null funcall-name) (symbolp funcall-name))
    (error "Expected nil or symbol. Found: ~s" funcall-name)))

(defun check-defcfun-arguments (arguments)
  (iter (for argument in arguments)
    (unless (and (listp argument)
		 (equal (length argument) 2)
		 (symbolp (car argument)))
      (error "Expected a list with a symbol and a foreign type. Found: ~s" argument))))

(defmacro defcfun ((foreign-name name &optional (funcall-name nil)) return-type &body arguments)
  (let ((name-args (mapcar #'car arguments))
	(ordered-args (apply #'append (mapcar (lambda (arg) (list (cadr arg) (car arg))) arguments)))
	(func-ptr (gensym)))
    (unless (or name funcall-name)
      (warn "MORE-CFFI:defcfun : name and funcall-name arguments are both nil"))
    `(progn
       ,@(when name
	   `((defun ,name ,name-args
	       (cffi:foreign-funcall ,foreign-name ,@ordered-args ,return-type))))
       ,@(when funcall-name
	   `((defun ,funcall-name ,(cons func-ptr name-args)
	       (cffi:foreign-funcall-pointer ,func-ptr () ,@ordered-args ,return-type)))))))


;; -------------------
;; ----- defwith -----  
;; -------------------

(defun check-defwith-name (name)
  (unless (name-desp name)
    (error "Expected a name designator.~%Found:~%   ~S" name)))

(defun check-create (name)
  (unless (name-desp name)
    (error "Expected a name designator.~%Found:~%   ~S" name)))

(defun check-destroy (name)
  (unless (name-desp name)
    (error "Expected a name designator.~%Found:~%   ~S" name)))

(defun check-destructor-arity (arity)
  (unless (and (integerp arity)
	       (>= arity 1))
    (error "Expected a positive integer after :destructor-arity.~%Found:~%   ~S" arity)))

(defun check-destructor-arguments (arg-positions)
  (unless (and (listp arg-positions)
	       (iter (for n in arg-positions)
		 (always (and (integerp n)
			      (>= n 0)))))
    (error "Expected a list of non-negative integers.~%Found:~%   ~S" arg-positions)))

;; Defines a with macro named name, using a constructor and a destructor
;; The constructor can receive zero or more arguments and can return one or more values
;; The destructor must receive 'destructor-arity' arguments or the arguments described by 'destructor-arguments'
;; The resulting macro binds some vars to the results from the constructor. These vars can be fewer than the returned values
(defmacro defwith (file name create destroy &key (destructor-arity 1 destructor-arityp) (destructor-arguments nil destructor-argumentsp))
  (check-defwith-name name)
  (check-create create)
  (check-destroy destroy)
  (when destructor-arityp
    (check-destructor-arity destructor-arity))
  (when destructor-argumentsp
    (check-destructor-arguments destructor-arguments))
  (with-gensyms ((var "var") (var-list "var-list") (args "args") (ret-list "ret-list") (body "body"))
    (let ((name-sym (name-des-symbol name)))
      `(progn
	 (defmacro ,name-sym (,var ,args &body ,body)
	   (with-gensyms ((,ret-list "ret-list"))
             (let ((,var-list (if (listp ,var)
				  ,var
				  (list ,var))))
               `(let ((,,ret-list (multiple-value-list (,',(name-des-symbol create) ,@,args))))
		  (unwind-protect
                       (multiple-value-bind ,,var-list (values-list ,,ret-list)
			 ,@,body)
                    (apply #',',(name-des-symbol destroy) ,,(if destructor-arguments
								``(loop for index in ',',destructor-arguments
									collect (nth index ,,ret-list))
								``(subseq ,,ret-list 0 ,',destructor-arity))))))))
	 ,@(when *export-symbols*
	     `((export ',name-sym)))
	 ,@(when (and *doc-generation* file)
	     `((doc-defwith ',name ',create ',destroy ,file)))))))


;; --------------------------------
;; ----- def-foreign-constant -----
;; --------------------------------

(defun check-constant-foreign-name (foreign-name)
  (unless (name-desp foreign-name)
    (error "Expected a name designator.~%Found~%:   ~s" foreign-name)))

(defun check-constant-name (name)
  (unless (name-desp name)
    (error "Expected a name designator.~%Found:~%:   ~s" name)))

(defmacro def-foreign-constant (file foreign-name name value)
  (check-constant-foreign-name foreign-name)
  (check-constant-name name)
  `(progn
     (defparameter ,(name-des-symbol name) ,value)
     ,@(when *export-symbols*
	 `((export ',(name-des-symbol name))))
     ,@(when (and *doc-generation* file)
	 `((doc-foreign-constant ',foreign-name ',name ',value ,file)))))


;; ----------------------------
;; ----- def-foreign-enum -----
;; ----------------------------

(defun check-enum-name (name)
  (unless (name-desp name) 
    (error "Expected a name designator.~%Found:~%   ~s" name)))

(defun check-enum-descriptor (descriptor)
  (unless (and (listp descriptor)
	       (= (length descriptor) 2))
    (error "Expected a list of two elements.~%Found:~%   ~s" descriptor))
  (unless (name-desp (car descriptor))
    (error "Expected a name designator.~%Found:~%   ~s" (car descriptor))))

(defun check-enum-descriptors (descriptors)
  (iter (for descriptor in descriptors)
    (check-enum-descriptor descriptor)))

(defmacro def-foreign-enum (file name &rest descriptors)
  (check-enum-name name)
  (check-enum-descriptors descriptors)
  `(progn
     (cffi:defctype ,(name-des-symbol name) :int)
     ,@(mapcar (lambda (x) (list 'defparameter (name-des-symbol (car x)) (cadr x))) descriptors)
     ,@(when *export-symbols*
	 (mapcar (lambda (x) `(export ',(name-des-symbol (car x)))) descriptors))
     ,@(when (and *doc-generation* file)
	 `((doc-foreign-enum ',name ',descriptors ,file)))))


;; ----------------------------------------
;; ----- def-foreign-callback-definer -----
;; ----------------------------------------

(defun check-definer-foreign-type (foreign-type)
  (unless (name-desp foreign-type)
    (error "Expected a name designator.~%Found:~%   ~S" foreign-type)))

(defun check-definer-name (name)
  (unless (name-desp name)
    (error "Expected a name designator.~%Found:~%   ~S" name)))

(defun check-arg-slot (arg)
  (unless (name-desp arg)
    (error "Expected a name designator at the start of descriptor.~%Found:~%   ~S" arg)))

(defun check-arg-type (type arg-slot)
  (unless (type-desp type)
    (error "Expected a type designator after :type in ~a descriptor.~%Found:~%   ~S" (name-des-string arg-slot) type)))

(defun check-arg-foreign-type (ftype arg-slot)
  (unless (or (symbolp ftype)
	      (and (listp ftype)
		   (or (eq (car ftype) :struct)
		       (eq (car ftype) :union))))
    (error "Expected a cffi type after :foreign-type in ~a descriptor.~%Found:~%   ~s" arg-slot ftype)))

(defun check-arg-create (arg-create virtualp arg-slot)
  (unless (or (not virtualp) (not (null arg-create)))
    (error "Virtual slots must have a non-nil create expression. Found in ~a argument." (name-des-string arg-slot)))
  (unless (or (null arg-create) virtualp (exists-rec (list (name-des-symbol arg-slot)) arg-create))
    (error "Create expression must use the ~a argument." (name-des-string arg-create))))

(defun check-arg-return (arg-return arg-slot)
  (unless (exists-rec (list (name-des-symbol arg-slot)) arg-return)
    (error "Return expression must use the ~a argument." (name-des-string arg-return))))

(defun check-arg-descriptor (arg-descriptor)
  (unless (and (listp arg-descriptor)
	       (not (null arg-descriptor)))
    (error "Expected a non-null list.~%Found:~%   ~S" arg-descriptor))
  (check-arg-slot (car arg-descriptor))
  (let ((createp (member :create arg-descriptor))
	(returnp (member :return arg-descriptor))
	(arg-ftypep (member :foreign-type arg-descriptor))
	(virtualp (cadr (member :virtual arg-descriptor))))
    (when (and virtualp returnp)
      (error "If :virtual is used, :return is forbidden. Found them in ~a descriptor"
	     (car arg-descriptor)))
    (when (and virtualp arg-ftypep)
      (error "If :virtual is used, :foreign-type is forbidden. Found them in ~a descriptor."
	     (car arg-descriptor)))
    (when (and createp returnp)
      (error "Just one of :create or :return can appear in an argument descriptor.~%Found in ~a descriptor:~%   ~S~%   ~S"
	     (name-des-string (car arg-descriptor)) (subseq createp 0 2) (subseq returnp 0 2)))
    (unless arg-ftypep
      (error "Expected :foreign-type and a cffi type in ~a descriptor." (car arg-descriptor)))
    (iter (for rest-descriptor on (cdr arg-descriptor) by #'cddr)
      (unless (member (car rest-descriptor) '(:foreign-type :type :virtual :create :return))
	(error "Expected :foreign-type, :type, :create or :return in ~a descriptor.~%Found:~%   ~S"
	       (name-des-string (car arg-descriptor)) (car rest-descriptor)))
      (cond
	((eq (car rest-descriptor) :type)
	 (check-arg-type (cadr rest-descriptor) (car arg-descriptor)))
	((eq (car rest-descriptor) :foreign-type)
	 (check-arg-foreign-type (cadr rest-descriptor) (car arg-descriptor)))
	((eq (car rest-descriptor) :create)
	 (check-arg-create (cadr rest-descriptor) virtualp (car arg-descriptor)))
	((eq (car rest-descriptor) :return)
	 (check-arg-return (cadr rest-descriptor) (car arg-descriptor)))))))

(defun check-arg-descriptors (arg-descriptors)
  (iter (for arg-descriptor in arg-descriptors)
    (check-arg-descriptor arg-descriptor)
    (let ((arg-returnp (member :return arg-descriptor)))
      (when arg-returnp
	(collect arg-returnp into return-args)))
    (finally (let ((num-return-args (length return-args)))
	       (unless (<= num-return-args 1)
		 (error "Expected zero or one return argument. There are ~a:~%   (~{~S~^ ~})"
			num-return-args return-args))))))

;; Return a list of lists with 5 elements
;; 1. The arg name
;; 2. The create expression
;; 3. The arg foreign type
;; 4. Whether is a foreign argument.
;; 5. Whether is a lisp argument.
(defun extract-create-arguments (arg-descriptors)
  (iter (for arg-descriptor in arg-descriptors)
    (let ((createp (member :create arg-descriptor))
	  (returnp (member :return arg-descriptor))
	  (virtualp (member :virtual arg-descriptor)))
      (when (or createp (not returnp))
	(collect (list (car arg-descriptor)
		       (if createp (cadr createp) (name-des-symbol (car arg-descriptor)))
		       (cadr (member :foreign-type arg-descriptor))
		       (not virtualp)
		       (or virtualp (not createp) (cadr createp))))))))

;; Return a list with two elements.
;; 1. The return arg name
;; 2. The return expression
;; 3. The return foreign type
(defun extract-return-argument (arg-descriptors)
  (iter (for arg-descriptor in arg-descriptors)
    (let ((returnp (member :return arg-descriptor)))
      (when returnp
	(return (list (car arg-descriptor) (cadr returnp) (cadr (member :foreign-type arg-descriptor))))))))

(defun create-definer-code (name create-arguments return-argument)
  (iter (for arg in create-arguments)
    (when (cadddr (cdr arg))
      (collect (name-des-symbol (car arg)) into lisp-args))
    (when (cadr arg)
      (collect (cadr arg) into lisp-create-exprs))
    (when (cadddr arg)
      (collect (name-des-symbol (car arg)) into foreign-args)
      (collect (caddr arg) into foreign-types))
    (finally (let* ((callback-name (gensym))
		    (callback-body (gensym))
		    (return-ftype (caddr return-argument))
		    (callback-args (mapcar (lambda (x) (gensym (string x))) foreign-args))
		    (lisp-args-callback-args (apply #'append (mapcar #'list lisp-args callback-args)))
		    (callback-create-exprs (mapcar (lambda (x) (rec-substitute lisp-args-callback-args x)) lisp-create-exprs))
		    (user-lisp-args (gensym))
		    (callback-let-create-exprs (gensym))
		    (callback-args-types (mapcar #'list callback-args foreign-types))
		    (callback-return-sym (gensym))
		    (lisp-return-expr (cadr return-argument))
		    (lisp-return-sym (name-des-symbol (car return-argument)))
		    (callback-return-expr (rec-substitute (list lisp-return-sym callback-return-sym) lisp-return-expr)))
	       (return `(defmacro ,(name-des-symbol name) (,callback-name ,lisp-args &body ,callback-body)
			  (let* ((,user-lisp-args ,(cons 'list lisp-args))
				 (,callback-let-create-exprs (mapcar #'list ,user-lisp-args ',callback-create-exprs)))
			    `(cffi:defcallback ,,callback-name ,',return-ftype ,',callback-args-types
			       (let ((,',callback-return-sym (let ,,callback-let-create-exprs
							       ,@,callback-body)))
				 ,',callback-return-expr)))))))))

(defun extract-doc-create-info (arg-descriptors)
  (iter (for arg-descriptor in arg-descriptors)
    (when (and (not (member :return arg-descriptor)) (or (not (member :create arg-descriptor)) (cadr (member :create arg-descriptor))))
      (let ((doc-typep (member :type arg-descriptor)))
	(if doc-typep
	    (collect (list (car arg-descriptor) (cadr doc-typep) t))
	    (collect (list (car arg-descriptor) nil nil)))))))

(defun extract-doc-return-info (arg-descriptors)
  (iter (for arg-descriptor in arg-descriptors)
    (when (member :return arg-descriptor)
      (let ((doc-typep (member :type arg-descriptor)))
	(if doc-typep
	    (return (list (car arg-descriptor) (cadr doc-typep) t))
	    (return (list (car arg-descriptor) nil nil)))))))

(defmacro def-foreign-callback-definer (file foreign-type name &body arg-descriptors)
  (check-definer-name name)
  (check-arg-descriptors arg-descriptors)
  `(progn
     ,(create-definer-code name
			   (extract-create-arguments arg-descriptors)
			   (extract-return-argument arg-descriptors))
     ,@(when *export-symbols*
	 `((export ',(name-des-symbol name))))
     ,@(when (and *doc-generation* file)
	 `((doc-foreign-callback-definer ',foreign-type ',name ',(extract-doc-create-info arg-descriptors)
					 ',(extract-doc-return-info arg-descriptors) ,file)))))


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


;; --------------------------
;; ----- def-foreign-macro -----
;; --------------------------

(defun check-foreign-macro-foreign-name (foreign-name)
  (unless (name-desp foreign-name)
    (error "Expected a name designator.~%Found:~%   ~s" foreign-name)))

(defun check-foreign-macro-name (name)
  (unless (name-desp name)
    (error "Expected a name designator.~%Found:~%   ~s" name)))

(defun check-foreign-macro-args (args)
  (unless (and (listp args)
	       (iter (for arg in args)
		 (always (symbolp arg))))
    (error "Expected a list of symbols.~%Found:~%   ~s" args)))

(defun extract-foreign-macro-docstring (body)
  (if (stringp (car body))
      (car body)
      nil))

(defmacro def-foreign-macro (file (foreign-name name) args &body body)
  (check-foreign-macro-foreign-name foreign-name)
  (check-foreign-macro-name name)
  (check-foreign-macro-args args)
  (let ((docstring (extract-foreign-macro-docstring body)))
    `(progn
       (defmacro ,(name-des-symbol name) ,args
	 ,@body)
       ,@(when *export-symbols*
	   `((export ',(name-des-symbol name))))
       ,@(when (and *doc-generation* file)
	   `((doc-foreign-macro ',foreign-name ',name ',args ',docstring ,file))))))


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
