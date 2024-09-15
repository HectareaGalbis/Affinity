
(in-package #:affinity)



(eval-when (:compile-toplevel :load-toplevel :execute)

  (defstruct slot
    name
    type
    private)

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
    (let ((options (loop option in (cddr slot) by #'cddr
                         collect option)))
      (unless (subsetp options *options*)
        (error "Found an unknown option in the slot: ~s" slot))
      (unless (= (length options) (length (remove-duplicates options)))
        (error "Duplicated options at the slot: ~s" slot))))
  
  (defun parse-slot (slot)
    (let ((name (car slot))
          (type (cadr slot))
          (options (mapcar #¿(getf (cddr slot) ?) *options*)))
      (apply #'make-slot-info :name name :type type options)))

  (defun slot-private-p (slot)
    (slot-value ,slot 'type))

  (defun slot-public-p (slot)
    (not (slot-value ,slot 'private))))
