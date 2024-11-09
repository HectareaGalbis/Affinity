
(in-package #:affinity)
(in-readtable affinity)


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defclass slot ()
    ((name :initarg :name :reader slot-name)
     (affi-type :initarg :affi-type :reader slot-affi-type)
     (cffi-type :initarg :cffi-type :reader slot-cffi-type)
     (private :initarg :private
              :initform nil
              :reader slot-private)))

  (defvar *options* '(:private))
  
  (defun check-slot-syntax (slot)
    (check-type slot list)
    (unless (cdr slot)
      (error "Expected at least a name and a affi type but found: ~s" slot))
    (check-type (car slot) symbol)
    (when (cadr slot)
      (check-affi-type (cadr slot)))
    (unless (evenp (length slot))
      (error "Expected a even quantity of elements in this slot: ~s" slot))
    (let ((options (loop for option in (cddr slot) by #'cddr
                         if (not (member option *options*))
                           do (error "Found the unknown option ~s in the slot: ~s" option slot)
                         else 
                           collect option)))
      (unless (= (length options) (length (remove-duplicates options)))
        (error "Duplicated options at the slot: ~s" slot))))
  
  (defun parse-slot (slot)
    (check-slot-syntax slot)
    (let ((name (car slot))
          (affi-type (cadr slot))
          (cffi-type (affi-to-cffi (cadr slot)))
          (options (mapcan #¿(list ?option (getf (cddr slot) ?option)) *options*)))
      (apply #'make-instance 'slot :name name :affi-type affi-type :cffi-type cffi-type options)))

  (defun slot-foreign-p (slot)
    (and (slot-value slot 'cffi-type) t))

  (defun slot-public-p (slot)
    (not (slot-value slot 'private)))

  (defun slot-expand-getter (slot)
    (with-slots (name affi-type) slot
      (expand-getter name affi-type)))

  (defun slot-expand-setter (new-value slot)
    (with-slots (name affi-type) slot
      (expand-setter new-value name affi-type))))
