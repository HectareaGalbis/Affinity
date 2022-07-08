

(in-package :mcffi)


;; ----------------------------
;; ----- Helper functions -----
;; ----------------------------

;; memset from C standard library (parece ser que puede dar problemas, mejor usar zero-struct)
(cffi:defcfun "memset" :pointer
  (str :pointer) (c :int) (n :size))


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
  (or (keywordp name)
      (symbolp name)
      (stringp name)))

;; Return t if two symbols of two name designators are the same
(defun name= (name1 name2)
  (eq (name-des-symbol name1) (name-des-symbol name2)))

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

(defun doc-foreign-callback-definer-default (name create-info return-info file)
  (let ((name-str (name-des-string name))
	(args-str (mapcar (lambda (x) (name-des-string (car x))) create-info))
	(arg-types-str (mapcar (lambda (x) (name-des-string (cadr x))) create-info))
	(arg-typesp (mapcar #'caddr create-info))
	(ret-arg-str (name-des-string (car return-info)))
	(ret-type-str (name-des-string (cadr return-info)))
	(ret-typep (caddr return-info)))
    (format file "**~a**~%```lisp~%(~a (~{~a~^ ~})~% &body body) => ~a~%```~%"
	    name-str name-str args-str ret-arg-str)
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

(defun doc-foreign-function-default (name args type-decls result-decls file)
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

(defun doc-foreign-struct-default (doc-create-info doc-destroy-info doc-accessors-info type-infos
				   type infix file)
  (destructuring-bind (no-constructor-p constructor-parameters) doc-create-info
    (let* ((type-str (name-des-string type))
	   (infix-str (name-des-string infix))
	   (constructor-str (concatenate 'string "create-" infix-str))
	   (destructor-str (concatenate 'string "destroy-" infix-str))
	   (no-destructor-p doc-destroy-info))
      (doc-subsubheader-default type-str file)
      (let* ((struct-members (iter (for member-type in (cffi:foreign-slot-names (list :struct (name-des-symbol type))))
			       (let ((constructor-memberp (member member-type constructor-parameters
								  :key (lambda (x) (name-des-symbol (car x)))))
				     (accessor-memberp (member member-type doc-accessors-info
							       :key (lambda (x) (name-des-symbol (car x))))))
				 (when (or constructor-memberp accessor-memberp)
				   (collect (or (caar constructor-memberp) (caar accessor-memberp))))))))
	(when struct-members
	  (format file "**Members**~%")
	  (iter (for struct-member in struct-members)
	    (format file "* *~a*" (name-des-string struct-member))
	    (let ((type-infop (member (name-des-symbol struct-member) type-infos :key #'car)))
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


;; ----------------------------------------
;; ----- Documentation global options -----
;; ----------------------------------------

(defparameter *enable-doc-generation* nil)

(defparameter *doc-header-proc* #'doc-header-default)
(defparameter *doc-subheader-proc* #'doc-subheader-default)
(defparameter *doc-subsubheader-proc* #'doc-subsubheader-default)
(defparameter *doc-note-proc* #'doc-note-default)
(defparameter *doc-defwith-proc* #'doc-defwith-default)
(defparameter *doc-foreign-callback-definer-proc* #'doc-foreign-callback-definer-default)
(defparameter *doc-foreign-function-proc* #'doc-foreign-function-default)
(defparameter *doc-foreign-struct-proc* #'doc-foreign-struct-default)


;; -----------------------------------
;; ----- Documentation functions -----
;; -----------------------------------

(defmacro with-doc-file ((file path) &body body)
  (if *enable-doc-generation*
      `(with-open-file (,file ,path :direction :output :if-exists :supersede :if-does-not-exist :create)
	 ,@body)
      `(progn ,@body)))

(defmacro doc-header (name file)
  (if (and *enable-doc-generation* file)
      `(funcall *doc-header-proc* ,name ,file)))

(defmacro doc-subheader (name file)
  (if (and *enable-doc-generation* file)
      `(funcall *doc-subheader-proc* ,name ,file)))

(defmacro doc-subsubheader (name file)
  (if (and *enable-doc-generation* file)
      `(funcall *doc-subsubheader-proc* ,name ,file)))

(defmacro doc-note (name file)
  (if (and *enable-doc-generation* file)
      `(funcall *doc-note-proc* ,name ,file)))

(defun doc-defwith (name create destroy file)
  (funcall *doc-defwith-proc* name create destroy file))

(defun doc-foreign-callback-definer (name create-info return-info file)
  (funcall *doc-foreign-callback-definer-proc* name create-info return-info file))

(defun doc-foreign-function (name args type-decls result-decls file)
  (funcall *doc-foreign-function-proc* name args type-decls result-decls file))

(defun doc-foreign-struct (doc-create-info doc-destroy-info doc-accessors-info type-infos
			   type infix file)
  (funcall *doc-foreign-struct-proc* doc-create-info doc-destroy-info doc-accessors-info
	   type-infos type infix file))


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
(defmacro defwith (name file create destroy &key (destructor-arity 1 destructor-arityp) (destructor-arguments nil destructor-argumentsp))
  (check-defwith-name name)
  (check-create create)
  (check-destroy destroy)
  (when destructor-arityp
    (check-destructor-arity destructor-arity))
  (when destructor-argumentsp
    (check-destructor-arguments destructor-arguments))
  (with-gensyms ((var "var") (var-list "var-list") (args "args") (ret-list "ret-list") (body "body"))
    `(progn
       (defmacro ,(name-des-symbol name) (,var ,args &body ,body)
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
       ,@(if (and *enable-doc-generation* file)
	     `((doc-defwith ',name ',create ',destroy ,file))
	     nil))))


;; ----------------------------------------
;; ----- def-foreign-callback-definer -----
;; ----------------------------------------

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
		   (eq (car ftype) :struct)))
    (error "Expected a cffi type after :foreign-type in ~a descriptor.~%Found:~%   ~s" arg-slot ftype)))

(defun check-arg-create (arg-create arg-slot)
  (unless (exists-rec (list (name-des-symbol arg-slot)) arg-create)
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
	(arg-ftypep (member :foreign-type arg-descriptor)))
    (when (and createp returnp)
      (error "Just one of :create or :return can appear in an argument descriptor.~%Found in ~a descriptor:~%   ~S~%   ~S"
	     (name-des-string (car arg-descriptor)) (subseq createp 0 2) (subseq returnp 0 2)))
    (unless arg-ftypep
      (error "Expected :foreign-type and a cffi type in ~a descriptor." (car arg-descriptor)))
    (iter (for rest-descriptor on (cdr arg-descriptor) by #'cddr)
      (unless (member (car rest-descriptor) '(:foreign-type :type :create :return))
	(error "Expected :foreign-type, :type, :create or :return in ~a descriptor.~%Found:~%   ~S"
	       (name-des-string (car arg-descriptor)) (car rest-descriptor)))
      (cond
	((eq (car rest-descriptor) :type)
	 (check-arg-type (cadr rest-descriptor) (car arg-descriptor)))
	((eq (car rest-descriptor) :foreign-type)
	 (check-arg-foreign-type (cadr rest-descriptor) (car arg-descriptor)))
	((eq (car rest-descriptor) :create)
	 (check-arg-create (cadr rest-descriptor) (car arg-descriptor)))
	((eq (car rest-descriptor) :return)
	 (check-arg-return (cadr rest-descriptor) (car arg-descriptor)))))))

(defun check-arg-descriptors (arg-descriptors)
  (iter (for arg-descriptor in arg-descriptors)
    (check-arg-descriptor arg-descriptor)
    (let ((arg-returnp (member :return arg-descriptor)))
      (when arg-returnp
	(collect arg-returnp into return-args)))
    (finally (let ((num-return-args (length return-args)))
	       (unless (= num-return-args 1)
		 (error "Expected only one return argument. There are ~a:~%   (~{~S~^ ~})"
			num-return-args return-args))))))

;; Return a list of lists with two elements
;; 1. The arg name
;; 2. The create expression
;; 3. The arg foreign type
(defun extract-create-arguments (arg-descriptors)
  (iter (for arg-descriptor in arg-descriptors)
    (let ((createp (member :create arg-descriptor))
	  (returnp (member :return arg-descriptor)))
      (when (or createp (not returnp))
	(collect (list (car arg-descriptor)
		       (if createp (cadr createp) (name-des-symbol (car arg-descriptor)))
		       (cadr (member :foreign-type arg-descriptor))))))))

;; Return a list with two elements.
;; 1. The return arg name
;; 2. The return expression
;; 3. The return foreign type
(defun extract-return-argument (arg-descriptors)
  (iter (for arg-descriptor in arg-descriptors)
    (let ((returnp (member :return arg-descriptor)))
      (when returnp
	(return (list (car arg-descriptor) (cadr returnp) (cadr (member :foreign-type arg-descriptor))))))))

(defun create-inner-macro (name parameter-create-syms parameter-create-gensyms create-exprs parameter-ftypes
			   return-argument parameter-gensyms body)
  (let* ((return-ftype (caddr return-argument))
	 (parameter-gensyms-ftypes (mapcar #'list parameter-gensyms parameter-ftypes))
	 (parameter-syms-gensyms (apply #'append (mapcar #'list parameter-create-syms parameter-gensyms)))
	 (final-create-exprs (mapcar (lambda (x)
				       (rec-substitute parameter-syms-gensyms x))
				     create-exprs))
	 (syms-final-create-exprs (mapcar #'list parameter-create-gensyms final-create-exprs))
	 (return-gensym (gensym))
	 (return-sym (name-des-symbol (car return-argument)))
	 (return-expr (cadr return-argument))
	 (final-return-expr (rec-substitute (list return-sym return-gensym) return-expr)))
    `(cffi:defcallback ,name ,return-ftype ,parameter-gensyms-ftypes
       (let ((,return-gensym (let ,syms-final-create-exprs
			       ,@body)))
	 ,final-return-expr))))
  
(defun create-definer-code (name create-arguments return-argument)
  (let* ((name-gensym (gensym))
	 (parameter-syms (mapcar (lambda (x) (name-des-symbol (car x))) create-arguments))
	 (parameter-exprs (mapcar #'cadr create-arguments))
	 (parameter-ftypes (mapcar #'caddr create-arguments))
	 (body-gensym (gensym))
	 (parameter-gensyms (mapcar (lambda (x) (gensym (string x))) parameter-syms))
	 (parameter-gensyms-gensym (gensym))
	 (parameter-create-gensym (gensym))
	 (let-gensyms (mapcar (lambda (x) (list x '(gensym))) parameter-gensyms)))
    `(defmacro ,(name-des-symbol name) (,name-gensym ,parameter-syms &body ,body-gensym)
       (let* (,@let-gensyms
	      (,parameter-gensyms-gensym (list ,@parameter-gensyms))
	      (,parameter-create-gensym (list ,@parameter-syms)))
	 (create-inner-macro ,name-gensym ',parameter-syms ,parameter-create-gensym ',parameter-exprs ',parameter-ftypes
			     ',return-argument ,parameter-gensyms-gensym ,body-gensym)))))

(defun extract-doc-create-info (arg-descriptors)
  (iter (for arg-descriptor in arg-descriptors)
    (when (not (member :return arg-descriptor))
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

(defmacro def-foreign-callback-definer (name file &body arg-descriptors)
  (check-definer-name name)
  (check-arg-descriptors arg-descriptors)
  `(progn
     ,(create-definer-code name
			   (extract-create-arguments arg-descriptors)
			   (extract-return-argument arg-descriptors))
     ,@(if (and *enable-doc-generation* file)
	   `((doc-foreign-callback-definer ',name ',(extract-doc-create-info arg-descriptors)
					   ',(extract-doc-return-info arg-descriptors) ,file))
	   nil)))


;; --------------------------------
;; ----- def-foreign-function -----
;; --------------------------------

(defun check-name (name)
  (unless (name-desp name)
    (error "Expected a name designator.~%Found:~%   ~S" name)))

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
	  (unless (type-desp (car type-decl))
	    (error "Expected a type designator at the start of a type declaration in ~a.~%Found:~%   ~a"
		   name type-decl))
	  (iter (for sym in (cdr type-decl))
	    (unless (name-desp sym)
	      (error "Expected a name designator in ~a type declaration.~%Found:~%   ~a" name type-decl))
	    (when (= return-found 0)
	      (let ((sym-sym (name-des-symbol sym)))
		(unless (exists-rec (list sym-sym) args)
		  (error "There is no symbol ~a in arguments from ~a." sym-sym name))))))))))

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
  (check-type-declarations (car exprs) name args)  
  (let* ((type-declarations (extract-type-declarations (car exprs)))
	 (return-declarations (extract-return-declarations (car exprs)))
	 (final-body (if (or type-declarations return-declarations) (cdr exprs) exprs)))
    `(progn
       (defun ,(name-des-symbol name) ,args ,@final-body)
       ,@(if (and *enable-doc-generation* file)
	     `((doc-foreign-function ',name ',args ',type-declarations ',return-declarations ,file))
	     nil))))


;; ------------------------------
;; ----- def-foreign-struct -----
;; ------------------------------

(defun check-struct-type (struct-type)
  (unless (name-desp struct-type)
    (error "Expected a name designator.~%Found:~%   ~S" struct-type)))

(defun check-infix (infix)
  (unless (name-desp infix)
    (error "Expected a name designator.~%Found:~%   ~S" infix)))

(defun check-options (options)
  (unless (listp options)
    (error "Expected a list.~%Found:~%   ~S" options))
  (iter (for option in options)
    (unless (member option '(:no-constructor :no-destructor :enable-default-get
			     :enable-default-set :enable-default-create :include-invisibles))
      (error "Expected :no-constructor, :no-destructor, :enable-default-get, :enable-default-set, :enable-default-create or :include-invisibles.~%Found:~%   ~S"
	     option))))

(defun check-slot-name (slot-name slot-names struct-type)
  (unless (name-desp slot-name)
    (error "Expected a name designator.~%Found:~%   ~S" slot-name))
  (unless (member (name-des-symbol slot-name) slot-names)
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

(defun check-create-option (create slot-name)
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
  (unless (exists-rec (list (name-des-symbol slot-name)) (cdr create))
    (error "Expected the use of ~a in its create expression." (name-des-string slot-name))))

(defun check-destroy-option (destroy slot-name)
  (unless (exists-rec (list (name-des-symbol slot-name)) destroy)
    (error "Expected the use of ~a in its destroy expression." (name-des-string slot-name))))

(defun check-get-option (get slot-name)
  (unless (or (null get)
	      (and (listp get)
		   (listp (car get))
		   (not (null (cadr get)))))
    (error "Expected a get expression ((&rest args) &body body) in the ~a descriptor.~%Found:~%   ~S"
	   (name-des-string slot-name) get))
  (unless (exists-rec (list (name-des-symbol slot-name)) get)
    (error "Expected the use of ~a in its get expression." (name-des-string slot-name))))

(defun check-set-option (set slot-name)
  (unless (or (null set)
	      (and (listp set)
		   (listp (car set))
		   (not (null (car set)))
		   (not (null (cadr set)))
		   (not (member (caar set) '(&optional &key &rest &aux &allow-other-keys)))))
    (error "Expected a set expression ((new-val &rest args) &body body) in the ~a descriptor.~%Found:~%   ~S"
	   (name-des-string slot-name) set))
  (unless (exists-rec (list (name-des-symbol slot-name)) set)
    (error "Expected the use of ~a in its set expression." (name-des-string slot-name))))

(defun check-slot-descriptor (descriptor slot-names struct-type no-constructor-p no-destructor-p)
  (if (and (listp descriptor) (not (null descriptor)))
      (progn
	(check-slot-name (car descriptor) slot-names struct-type)
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
	      ((eq (car rest-descriptor) :type)
	       (check-type-option (cadr rest-descriptor) (car descriptor)))
	      ((eq (car rest-descriptor) :init-form)
	       (let* ((create-optionp (member :create descriptor))
		      (namep (member :name descriptor))
		      (name (if namep (cadr namep) (car descriptor))))
		 (check-init-form-option create-optionp (cadr create-optionp) name (car descriptor))))
	      ((eq (car rest-descriptor) :create)
	       (check-create-option (cadr rest-descriptor) (car descriptor)))
	      ((eq (car rest-descriptor) :destroy)
	       (check-destroy-option (cadr rest-descriptor) (car descriptor)))
	      ((eq (car rest-descriptor) :get)
	       (check-get-option (cadr rest-descriptor) (car descriptor)))
	      ((eq (car rest-descriptor) :set)
	       (check-set-option (cadr rest-descriptor) (car descriptor)))))))
      (check-slot-name descriptor slot-names struct-type)))


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

(defun create-constructor-code (create-infos pointer-slots name-infos init-form-infos struct-type
				enable-default-creates enable-invisibles suffix)
  (iter (for create-info in create-infos)
    (destructuring-bind (slot-name-sym invisiblep create createp) create-info
      (when (and (or enable-invisibles (not invisiblep))
		 (or enable-default-creates createp))
	(if (member slot-name-sym pointer-slots)
	    (collect (list :pointer slot-name-sym) into used-slots)
	    (collect slot-name-sym into used-slots))
	(if createp
	    (progn
	      (when (not (null (car create)))
		(let* ((namep (member slot-name-sym name-infos :key #'car))
		       (keyword-name (if namep (cadar namep) slot-name-sym))
		       (arg (caar create))
		       (init-formp (member slot-name-sym init-form-infos :key #'car))
		       (init-form (if init-formp (cadar init-formp) 0)))
		  (collect (list (list (name-des-keyword keyword-name) arg) init-form)
		    into constructor-parameters)))
	      (appending (cdr create) into create-expressions))
	    (progn
	      (let* ((namep (member slot-name-sym name-infos :key #'car))
		     (keyword-name (if namep (cadar namep) slot-name-sym))
		     (arg (gensym))
		     (init-formp (member slot-name-sym init-form-infos :key #'car))
		     (init-form (if init-formp (cadar init-formp) 0)))
		(collect (list (list (name-des-keyword keyword-name) arg) init-form)
		  into constructor-parameters)
		(collect `(setf ,slot-name-sym ,arg) into create-expressions))))))
    (finally (return `(defun ,(intern (concatenate 'string "CREATE-" (string-upcase (name-des-string suffix))))
			  (&key ,@constructor-parameters)
			,(let ((object-sym (gensym)))
			   `(let ((,object-sym (cffi:foreign-alloc '(:struct ,(name-des-symbol struct-type))))				   )
			      (memset ,object-sym 0 (cffi:foreign-type-size '(:struct ,(name-des-symbol struct-type))))
			      (cffi:with-foreign-slots (,used-slots ,object-sym (:struct ,(name-des-symbol struct-type)))
				,@create-expressions)
			      (values ,object-sym))))))))

(defun create-destructor-code (destroy-infos pointer-slots struct-type suffix)
  (let ((slot-names (cffi:foreign-slot-names (list :struct (name-des-symbol struct-type)))))
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
			       `(cffi:with-foreign-slots (,final-used-slots ,arg (:struct ,(name-des-symbol struct-type)))
				  ,@destroy-exprs))
			    (cffi:foreign-free ,arg))))))))

(defun create-with-code (suffix)
  `(defwith ,(intern (concatenate 'string "WITH-" (string-upcase (name-des-string suffix)))) nil
     ,(intern (concatenate 'string "CREATE-" (string-upcase (name-des-string suffix))))
     ,(intern (concatenate 'string "DESTROY-" (string-upcase (name-des-string suffix))))))

(defun create-get-codes (get-infos pointer-slots name-infos struct-type
			 enable-default-get enable-invisibles prefix)
  (let ((slot-names (cffi:foreign-slot-names (list :struct (name-des-symbol struct-type)))))
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
			(cffi:with-foreign-slots (,final-used-slots ,(car args) (:struct ,(name-des-symbol struct-type)))
			  ,final-get-expr)))))))))

(defun create-set-codes (set-infos pointer-slots name-infos struct-type
			 enable-default-set enable-invisibles prefix)
  (let ((slot-names (cffi:foreign-slot-names (list :struct (name-des-symbol struct-type)))))
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
			(cffi:with-foreign-slots (,final-used-slots ,object-arg (:struct ,(name-des-symbol struct-type)))
			  ,final-set-expr)))))))))

(defmacro def-foreign-struct (struct-type infix file options &body slot-descriptors)
  (check-struct-type struct-type)
  (check-infix infix)
  (check-options options)
  (check-slot-descriptors slot-descriptors (cffi:foreign-slot-names (list :struct (name-des-symbol struct-type)))
			  struct-type (member :no-constructor options) (member :no-destructor options))
  (let ((no-constructorp (member :no-constructor options))
	(no-destructorp  (member :no-destructor options))
	(default-createp (member :enable-default-create options))
	(default-getp    (member :enable-default-get options))
	(default-setp    (member :enable-default-set options))
	(invisiblesp     (member :include-invisibles options)))
    (iter (for slot-name-sym in (cffi:foreign-slot-names (list :struct (name-des-symbol struct-type))))
      (let ((slot-descriptor (car (member slot-name-sym slot-descriptors :key (lambda (x) (if (listp x) (car x) x))))))
	(when (and (listp slot-descriptor)
		   (not (null slot-descriptor))
		   (cadr (member :pointer slot-descriptor)))
	  (collect slot-name-sym                                                 into pointer-slots))
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
			      (list (create-constructor-code create-infos pointer-slots name-infos
							     init-form-infos struct-type
							     default-createp
							     invisiblesp
							     infix)))
			  ,@(unless no-destructorp
			      (list (create-destructor-code destroy-infos pointer-slots
							    struct-type infix)))
			  ,@(unless (or no-constructorp
					no-destructorp)
			      (list (create-with-code infix)))
			  ,@(create-get-codes get-infos pointer-slots name-infos struct-type
					      default-getp invisiblesp infix)
			  ,@(create-set-codes set-infos pointer-slots name-infos struct-type
					      default-setp invisiblesp infix)
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
							',type-infos ,struct-type ',infix
							,file-sym)))))))))))
