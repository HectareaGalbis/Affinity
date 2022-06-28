

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


;; Recursively search for val in l
(defun member-rec (syms l)
  (if (consp l)
    (or (member-rec syms (car l)) (member-rec syms (cdr l)))
    (member l syms)))


;; Substitute every ocurrence of each symbol in assoc-symbol by
;; its associated symbol
(defun rec-substitute (assoc-symbols l)
  (cond
    ((and (symbolp l) (member l assoc-symbols)) (getf assoc-symbols l))
    ((consp l) (cons (rec-substitute assoc-symbols (car l)) (rec-substitute assoc-symbols (cdr l))))
    (t l)))


;; -------------------
;; ----- defwith -----
;; -------------------

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


;; ----------------------------------------
;; ----- def-foreign-struct-functions -----
;; ----------------------------------------

(defun check-infix (infix)
  (unless (symbolp infix)
    (error "Expected a symbol.~%Found:~%   ~S" suffix)))

(defun check-struct-type (type)
  (unless (and (listp type)
	       (eq (car type) :struct)
	       (symbolp (cadr type)))
    (error "Expected a list (:struct type) where type is a cffi type.~%Found:~%   ~S" type)))

(defun check-options (options)
  (unless (listp options)
    (error "Expected a list.~%Found:~%   ~S" options))
  (iter (for option in options)
    (unless (member option '(:no-constructor :no-destructor :enable-default-get
			     :enable-default-set :enable-default-create :include-invisibles))
      (error "Expected :no-constructor, :no-destructor, :enable-default-get, :enable-default-set, :enable-default-create or :include-invisibles.~%Found:~%   ~S" option))))

(defun get-init-arg-parameter (init-arg)
  (if (symbolp init-arg)
      init-arg
      (if (null init-arg)
	  nil
	  (if (symbolp (car init-arg))
	      (car init-arg)
	      (cadar init-arg)))))

(defun create-constructor-parameter (init-arg)
  (let ((key-arg (intern (string (get-init-arg-parameter init-arg)) "KEYWORD")))
    (if (symbolp init-arg)
	(list key-arg init-arg)
	(if (null init-arg)
	    nil
	    (if (symbolp (car init-arg))
		(list (list key-arg (car init-arg)) (cadr init-arg))
		init-arg)))))

(defun check-init-arg (init-arg)
  (unless (or (symbolp init-arg) (listp init-arg))
    (error "Expected a symbol or a list.~%Found:~%   ~S" init-arg))
  (when (listp init-arg)
    (unless (<= (length init-arg) 3)
      (error "Expected a list of less than three elements.~%Found:~%   ~S" init-arg))
    (when (not (null init-arg))
      (unless (or (symbolp (car init-arg)) (listp (car init-arg)))
	(error "Expected a symbol or a list.~%Found:   ~S" (car init-arg)))
      (when (listp (car init-arg))
	(unless (keywordp (caar init-arg))
	  (error "Expected a keyword.~%Found:~%   ~S" (caar init-arg)))
	(unless (symbolp (cadar init-arg))
	  (error "Expected a symbol.~%Found:~%   ~S" (cadar init-arg))))
      (when (not (null (caddr init-arg)))
	(unless (symbolp (caddr init-arg))
	  (error "Expected a symbol.~%Found:~%   ~S" (caddr init-arg)))))))

(defun check-create-expr (create init-parameters)
  (unless (member-rec init-parameters create)
    (error "Expected the use of at least one constructor parameter.~%Found:~%   ~S" create)))

(defun check-creates (creates)
  (let ((init-parameters (iter (for create in creates)
			       (check-init-arg (car create))
			       (let ((init-parameter (get-init-arg-parameter (car create))))
				 (if init-parameter
				     (collect init-parameter))))))
    (loop for create in creates
	  do (check-create-expr create init-parameters))))

(defun check-destroy (destroy slots)
  (unless (member-rec slots destroy)
    (error "Expected the use of at least one slot member.~%Found:~%   ~S" destroy)))

(defun check-get (getter slot)
  (unless (or (null getter)
	      (and (listp getter)
		   (listp (car getter))
		   (not (null (cadr getter)))))
    (error "Expected a getter expression ((&rest args) expr) in the ~S descriptor.~%Found:~%   ~S"
	   slot getter)))

(defun check-set (setter slot)
  (unless (or (null setter)
	      (and (listp setter)
		   (listp (car setter))
		   (not (null (cadr setter)))
		   (not (member (caar setter) '(&optional &key &rest &aux &allow-other-keys)))))
    (error "Expected a setter expression ((new-val &rest args) expr) in the ~S descriptor.~%Found:~%   ~S"
	   slot setter)))

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
	  (iter (for rest-descriptor on (cdr descriptor) by (lambda (x) (cdr (cdr x))))
		(unless (member (car rest-descriptor) '(:pointer :create :destroy :get :set))
		  (error "Expected :pointer, :create, :destroy, :get or :set in ~S descriptor.~%Found:~%   ~S"
			 (car descriptor) (car rest-descriptor)))
		(if (eq (car rest-descriptor) :destroy)
		    (check-destroy (cadr rest-descriptor) slot-names))
		(if (eq (car rest-descriptor) :get)
		    (check-get (cadr rest-descriptor) (car descriptor)))
		(if (eq (car rest-descriptor) :set)
		    (check-set (cadr rest-descriptor) (car descriptor))))))
      (check-slot-member descriptor slot-names struct-type)))


(defun check-slot-descriptors (descriptors slot-names struct-type no-constructor-p no-destructor-p)
  (unless (listp descriptors)
    (error "Expected a list of slot descriptors.~%Found:~%   ~S" descriptors))
  (iter (for descriptor in descriptors)
    (check-slot-descriptor descriptor slot-names struct-type no-constructor-p no-destructor-p)
    (if (listp descriptor)
	(let ((create-expr-p (member :create descriptor)))
	  (if create-expr-p
	      (collect (cadr create-expr-p) into creates))))
    (finally (if (not no-constructor-p)
		 (check-creates creates)))))

(defun create-constructor-code (create-infos pointer-slots struct-type enable-default-creates
				enable-invisibles suffix)
  (iter (for create-info in create-infos)
    (destructuring-bind (slot-name invisiblep create createp) create-info
      (when (and (or enable-invisibles (not invisiblep))
		 (or enable-default-creates createp))
	(if (member slot-name pointer-slots)
	    (collect (list :pointer slot-name) into used-slots)
	    (collect slot-name into used-slots))
	(if createp
	    (progn
	      (when (not (null (car create)))
		(collect (create-constructor-parameter (car create)) into constructor-parameters)
		(let ((new-sym (gensym)))
		  (collect new-sym into constructor-syms)
		  (appending (list (get-init-arg-parameter (car create)) new-sym)
			     into constructor-parameter-syms)))
	      (let ((new-sym (gensym)))
		(collect new-sym into let-syms)
		(collect new-sym into setf-syms))
	      (collect (cons 'progn (cdr create)) into let-exprs))
	    (progn
	      (collect (list (list (intern (string slot-name) "KEYWORD") slot-name) 0)
		into constructor-parameters)
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
  `(defwith ,(intern (concatenate 'string "WITH-" (string suffix)))
       ,(intern (concatenate 'string "CREATE-" (string suffix)))
     ,(intern (concatenate 'string "DESTROY-" (string suffix)))))

(defun create-get-codes (get-infos pointer-slots struct-type enable-default-get enable-invisibles prefix)
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
					   used-slots)))
	    (collect `(defun ,(intern (concatenate 'string (string prefix) "-" (string slot-name))) ,args
			(cffi:with-foreign-slots (,final-used-slots ,(car args) ,struct-type)
			  ,final-get-expr)))))))))

(defun create-set-codes (set-infos pointer-slots struct-type enable-default-set enable-invisibles prefix)
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
					   used-slots)))
	    (collect `(defun (setf ,(intern (concatenate 'string (string prefix) "-" (string slot-name))))
			,args
			(cffi:with-foreign-slots (,final-used-slots ,object-arg ,struct-type)
			  ,final-set-expr)))))))))

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

(defmacro def-foreign-struct-functions (infix type options &body slot-descriptors)
  (check-infix infix)
  (check-struct-type type)
  (check-options options)
  (check-slot-descriptors slot-descriptors (cffi:foreign-slot-names type) type
			  (member :no-constructor options) (member :no-destructor options))
  (iter (for slot-name in (cffi:foreign-slot-names type))
    (let ((slot-descriptor (car (member slot-name slot-descriptors :key (lambda (x) (if (listp x)
											(car x)
											x))))))
      (progn
	(if (and (listp slot-descriptor)
		 (not (null slot-descriptor))
		 (cadr (member :pointer slot-descriptor)))
	    (collect slot-name                                                    into pointer-slots))
	(if (not (member :no-constructor options))
	    (collect (extract-descriptor-info slot-name slot-descriptor :create)  into create-infos))
	(if (not (member :no-destructor options))
	    (collect (extract-descriptor-info slot-name slot-descriptor :destroy) into destroy-infos))
	(collect (extract-descriptor-info slot-name slot-descriptor :get)         into get-infos)
	(collect (extract-descriptor-info slot-name slot-descriptor :set)         into set-infos)))
    (finally (return `(progn
			,@(unless (member :no-constructor options)
			    (list (create-constructor-code create-infos pointer-slots type
							   (member :enable-default-create options)
							   (member :include-invisibles options)
							   infix)))
			,@(unless (member :no-destructor options)
			    (list (create-destructor-code destroy-infos pointer-slots type infix)))
			,@(unless (or (member :no-constructor options)
				      (member :no-destructor options))
			    (list (create-with-code infix)))
			,@(create-get-codes get-infos pointer-slots type
					    (member :enable-default-get options)
					    (member :include-invisibles options)
					    infix)
			,@(create-set-codes set-infos pointer-slots type
					    (member :enable-default-set options)
					    (member :include-invisibles options)
					    infix))))))
