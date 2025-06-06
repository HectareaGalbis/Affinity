
(in-package #:affinity)
(in-readtable affinity)


(cffi:define-foreign-type pointer-type ()
  ((ctype :initarg :ctype)))

(cffi:define-parse-method pointer (ctype)
  (make-instance 'pointer-type :ctype ctype :actual-type :pointer))

(defclass pointer (owner)
  ((cpointer :initarg :cpointer)
   (ctype :initarg :ctype)
   (subpointers :initform (make-pool))
   (private-access-symbol :initform (make-symbol "PRIVATE-ACCESS"))))

(defmethod own-pointer ((owner pointer) ptr)
  (with-slots (subpointers) owner
    (own-pointer subpointers ptr)))

(defmethod release-pointer ((owner pointer) ptr)
  (with-slots (subpointers) owner
    (release-pointer subpointers ptr)))

(defmethod owns-pointer-p ((owner pointer) ptr)
  (with-slots (subpointers) owner
    (owns-pointer-p subpointers ptr)))

(defmethod cffi:translate-to-foreign ((object pointer) (obj-type pointer-type))
  (declare (ignore obj-type))
  (with-slots (cpointer) object
    (values cpointer)))

(defmethod cffi:translate-from-foreign (pointer (obj-type pointer-type))
  (with-slots (ctype) obj-type
    (make-instance 'pointer :cpointer pointer :ctype ctype)))

(defmethod cffi:free-translated-object (pointer (obj-type pointer-type) param)
  (declare (ignore param))
  (values))



(defgeneric get-structure-value (ptr name slot))
(defgeneric (setf get-structure-value) (new-value ptr name slot))


(defmethod slot-missing (class (obj pointer) slot-name (op (eql 'slot-value)) &optional new-value)
  (declare (ignore new-value))
  (with-slots (ctype) obj
    (unless (eq (car ctype) :struct)
      (error "This pointer does not point to a structure."))
    (with-slots (private-access-symbol) obj
      (let ((private-access-p (symbol-value private-access-symbol)))
        (if private-access-p
            (pointer-foreign-slot-value obj slot-name (cadr ctype))
            (if (public-slot-name-p class slot-name)
                (progv (list private-access-symbol) '(t)
                  (slot-value obj slot-name))
                (error "The member ~s is private." slot-name)))))))

(defmethod slot-missing (class (obj pointer) slot-name (op (eql 'setf)) &optional new-value)
  (with-slots (ctype) obj
    (unless (eq (car ctype) :struct)
      (error "This pointer does not point to a structure."))
    (with-slots (private-access-symbol) obj
      (let ((private-access-p (symbol-value private-access-symbol)))
        (if private-access-p
            (with-slots (cpointer ctype) obj
              (let ((*owner* obj))
                (setf (pointer-foreign-slot-value obj slot-name (cadr ctype)) new-value)))
            (if (public-slot-name-p class slot-name)
                (progv (list private-access-symbol) '(t)
                  (setf (slot-value obj slot-name) new-value))
                (error "The member ~s is private." slot-name)))))))

;; (defmethod slot-missing (class (obj pointer) slot-name (op (eql 'slot-boundp)) &optional new-value)
;;   )

;; (defmethod slot-missing (class (obj pointer) slot-name (op (eql 'slot-makunbound)) &optional new-value)
;;   )



(defun cast-pointer (type ptr)
  "Change the type of the pointer."
  (check-type ptr pointer)
  (with-slots (ctype) ptr
    (setf ctype type)))

(defun foreign-free (ptr)
  (check-type ptr pointer)
  (with-slots (subpointers cpointer) ptr
    (free-pool subpointers)
    (cffi:foreign-free cpointer)))

(defun foreign-alloc (ctype &key initial-element initial-contents (count 1) null-terminated-p)
  (let* ((cpointer (funcall #'cffi:foreign-alloc ctype
                            :initial-element initial-element
                            :initial-contents initial-contents
                            :count count
                            :null-terminated-p null-terminated-p))
         (pointer-instance (make-instance 'pointer
                                          :cpointer cpointer
                                          :ctype ctype)))
    (when *owner*
      (own-pointer *owner* pointer-instance))
    (values pointer-instance)))

(defun foreign-symbol-pointer (foreign-name ctype &key library)
  (let ((cpointer (funcall #'cffi:foreign-symbol-pointer foreign-name :library library)))
    (and cpointer
         (make-instance 'pointer :cpointer cpointer :ctype ctype))))

(defun inc-pointer (ptr offset)
  (check-type ptr pointer)
  (with-slots (cpointer) ptr
    (cffi:inc-pointer cpointer offset)))

(defmacro incf-pointer (place &optional (offset 1))
  (with-gensyms (cpointer-sym)
    `(with-slots ((,cpointer-sym cpointer)) ,place
       (cffi:incf-pointer ,cpointer-sym ,offset))))

(defun make-pointer (address ctype)
  (make-instance 'pointer
                 :cpointer (cffi:make-pointer address)
                 :ctype ctype))

(defun mem-aptr (ptr &optional (index 0))
  (check-type ptr pointer)
  (with-slots (cpointer ctype) ptr
    (cffi:mem-aptr cpointer ctype index)))

(defun mem-aref (ptr &optional (index 0))
  (check-type ptr pointer)
  (with-slots (cpointer ctype) ptr
    (cffi:mem-aref cpointer ctype index)))

(defun (setf mem-aref) (new-value ptr &optional (index 0))
  (check-type ptr pointer)
  (with-slots (cpointer ctype) ptr
    (let ((*owner* ptr))
      (setf (cffi:mem-aref cpointer ctype index) new-value))))

(defun mem-ref (ptr &optional offset)
  (check-type ptr pointer)
  (with-slots (cpointer ctype) ptr
    (cffi:mem-ref cpointer ctype offset)))

(defun (setf mem-ref) (new-value ptr &optional offset)
  (check-type ptr pointer)
  (with-slots (cpointer ctype) ptr
    (let ((*owner* ptr))
      (setf (cffi:mem-ref cpointer ctype offset) new-value))))

(defun null-pointer ()
  (make-instance 'pointer
                 :cpointer (cffi:null-pointer)
                 :ctype :void))

(defun null-pointer-p (ptr)
  (check-type ptr pointer)
  (with-slots (cpointer) ptr
    (cffi:null-pointer-p cpointer)))

(defun pointerp (ptr)
  (typep ptr 'pointer))

(defun pointer-address (ptr)
  (check-type ptr pointer)
  (with-slots (cpointer) ptr
    (cffi:pointer-address cpointer)))

(defun pointer-eq (ptr1 ptr2)
  (with-slots (cpointer1 cpointer) ptr1
    (with-slots (cpointer2 cpointer) ptr2
      (cffi:pointer-eq cpointer1 cpointer2))))

(defmacro with-foreign-pointer ((var size &optional size-var) &body body)
  (with-gensyms (cvar)
    `(cffi:with-foreign-pointer (,cvar ,size ,size-var)
       (let ((,var (make-instance 'pointer
                                  :cpointer ,cvar
                                  :ctype :void)))
         ,@body))))


;; struct related

(defun foreign-slot-pointer (ptr slot-name)
  (check-type ptr pointer)
  (with-slots (cpointer ctype) ptr
    (cffi:foreign-slot-pointer cpointer ctype slot-name)))

(defun foreign-slot-value (ptr slot-name)
  (check-type ptr pointer)
  (with-slots (cpointer ctype) ptr
    (cffi:foreign-slot-value cpointer ctype slot-name)))

(defun (setf foreign-slot-value) (new-value ptr slot-name)
  (check-type ptr pointer)
  (with-slots (cpointer ctype) ptr
    (let ((*owner* ptr))
      (setf (cffi:foreign-slot-value cpointer ctype slot-name) new-value))))

(defmacro with-foreign-object ((var ctype &optional count) &body body)
  (with-gensyms (priv-var cvar ev-type subpointers)
    `(let ((,ev-type ,ctype))
       (cffi:with-foreign-object (,cvar ,ev-type ,count)
         (let* ((,priv-var (make-instance 'pointer
                                    :cpointer ,cvar
                                    :ctype ,ev-type))
                (,var ,priv-var))
           (unwind-protect
                (progn ,@body)
             (with-slots (,subpointers subpointers) ,priv-var
               (free-pool ,subpointers))))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defun make-foreign-slot-binding (var ptr-sym type-sym)
    (cond
      ((symbolp var)
       `(,var (cffi:foreign-slot-value ,ptr-sym ,type-sym ',var)))
      ((listp var)
       (let ((num-elems (length var)))
         (when (not (null var))
           (let ((binding-var (car var)))
             (cond
               ((= num-elems 1)
                `(,binding-var (cffi:foreign-slot-value ,ptr-sym ,type-sym ',binding-var)))
               ((= num-elems 2)
                (let ((option (cadr var)))
                  (if (eq option :pointer)
                      `(,binding-var (cffi:foreign-slot-pointer ,ptr-sym ,type-sym ',binding-var))
                      `(,binding-var (cffi:foreign-slot-value ,ptr-sym ,type-sym ',option)))))
               ((and (= num-elems 3) (member :pointer var))
                (let ((slot (if (eq (cadr var) :pointer) (caddr var) (cadr var))))
                  `(,binding-var (cffi:foreign-slot-pointer ,ptr-sym ,type-sym ',slot))))))))))))

(defmacro with-foreign-slots (vars (ptr) &body body)
  (with-gensyms (ptr-sym type-sym)
    (let ((macrolet-bindings (mapcar #¿(make-foreign-slot-binding ? ptr-sym type-sym) (ensure-list vars))))
      `(with ((((,ptr-sym cpointer) (,type-sym ctype)) (slots ,ptr)))
         (symbol-macrolet ,macrolet-bindings
           ,@body)))))

(defwith foreign-slots (vars (ptr) body)
  `(with-foreign-slots ,vars (,ptr)
     ,@body))


(define-primitive-affi-type :pointer (&optional (inner-affi-type :void))
  `(pointer ,inner-affi-type))
