
(in-package #:affinity)


;; ------ current owner ------
 (defvar *owner* nil
   "The owner of the next allocated memory")
 
 
;; ------ owners ------
(defclass owner () ())

(defun ownerp (owner)
  (typep owner 'owner))

 (defgeneric own-pointer (owner ptr)
   (:documentation
    "Set OBJ as owned by OWNER."))
 
 (defgeneric release-pointer (owner ptr)
   (:documentation
    "Release the ownnership from OWNER of OBJ."))
 
 (defgeneric owns-pointer-p (owner ptr)
   (:documentation
    "Retrieves a list with the objects owned by OWNER. If OWNER does not own any objects, NIL is returned."))
 
;; TODO: Hacer POOL con hashmaps.
