
(in-package #:mcffi)


;; ------ current owner ------
(defvar *owner* nil
  "The owner of the next allocated memory")


;; ------ owners ------
(defgeneric own (owner obj)
  (:documentation
   "Set OBJ as owned by OWNER."))

(defgeneric disown (owner obj)
  (:documentation
   "Release the ownnership from OWNER of OBJ."))

(defgeneric get-owned (owner)
  (:documentation
   "Retrieves a list with the objects owned by OWNER. If OWNER does not own any objects, NIL is returned."))

(defun ownsp (owner obj)
  "Check wether OBJ is owned by OWNER."
  (and (member obj (get-owned owner)) t))


;; ------ owned ------
(defgeneric get-owner (owned)
  (:documentation
   "Retrieve the owner of OWNED. If OWNED is not owned by anyone, NIL is returned."))

(defgeneric set-owner (obj owner)
  (:documentation
   "Set to OBJ the owner OWNER. Removes the owner of OBJ if OWNER is NIL."))

(defun (setf get-owner) (owner obj)
  "Set to OBJ the owner OWNER."
  (set-owner obj owner))

(defun ownedp (owned-obj)
  "Checks whether OWNED-OBJ is owned by someone."
  (and (get-owner owned-obj) t))


;; ------ ownership ------
(defun owner-related-p (owner obj)
  "Checks if OBJ is owned by OWNER. Also check for possible ownership inconsistencies."
  (let ((owner-owns-p (ownsp owner obj))
        (obj-owned-p (eq (get-owner obj) owner)))
    (if (eql owner-owns-p obj-owned-p)
        (values owner-owns-p)
        (error "Ownership inconsistency."))))

(defun remove-ownership (obj &key (errorp t))
  "Remove the ownership of OBJ and his owner. If errorp is T, an error is raised when
OBJ is not owned by anyone. Otherwise, NIL is returned. If the ownership is
removed, T is returned."
  (let ((owner (get-owner obj)))
    (unless (owner-related-p owner obj)
      (if errorp
          (error "OBJ is not owned by anyone.")
          (return-from remove-ownership nil))))
  (disown owner obj)
  (set-owner obj nil)
  (values t))

(defun establish-ownership (owner obj &key force)
  "Establish an owner relationship between OWNER and OBJ. OBJ will be owned by OWNER.
If FORCE is NIL, an error is raised when OBJ is already owned. Otherwise, the possible
ownership of OBJ is removed before the new one is established."
  (when (owner-related-p owner obj)
    (if force
        (remove-ownership obj)
        (error "OBJ has already an OWNER.")))
  (own       owner obj)
  (set-owner obj owner)
  (values))
