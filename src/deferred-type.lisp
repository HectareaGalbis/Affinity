
(in-package #:more-cffi)


(defclass deferred-type ()
  ((actual-type :initarg :actual-type))
  (:documentation
   "A deferred type is like a regular type, but can use other memeber or arguments
to read or write data."))


(defgeneric )
