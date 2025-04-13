

(defpackage #:affinity
  (:use #:cl #:alexandria #:named-readtables #:clith)
  (:nicknames #:affi)
  (:import-from #:cffi
                #:define-foreign-library
                #:use-foreign-library)
  (:export #:define-foreign-library
           #:use-foreign-library
           #:defctype
           #:defcfun
           #:define-c-function
           #:defcallback
           #:define-callback-definer
           #:defcstruct
           #:foreign-alloc
           #:mem-ref))
