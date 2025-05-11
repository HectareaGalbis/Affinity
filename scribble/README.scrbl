
(in-package #:affi-docs)

@output-file["README.md"]

@title[:toc nil]{Affinity}

Welcome to Affinity!

@bold{WARNING}: This is still a WIP project. It can still be broken and API can change heavily.


@table-of-contents[]

@subtitle{Introduction}

Affinity is a higher layer of abstraction over @link[:address "https://github.com/cffi/cffi"]{CFFI}. It tries to make the writing of bindings easier. Specifically, Affinity prioritize the next features in decreasing order:

@enumerate[
@item{Ease of use.}
@item{Power.}
@item{Efficiency.}
]

@subtitle{Installation}

Manually for now.

@code-block[:lang "common-lisp"]{
cd ~/common-lisp
git clone https://github.com/HectareaGalbis/Affinity.git
}


@subtitle{Reference}

@itemize[
@item{@tref[reference]}
]


@subtitle{Quick view}

@subsubtitle{Functions and callbacks}

@code-block[:lang "common-lisp"]{
;; A function called foo.
(affi:defcfun (foo "Foo") :int ((str :string-ptr) (b :bool))
  "Do cool stuff.")

;; Another function to set callbacks.
(affi:defcfun (set-callback "SetCallback") ((func (:function :int (arg1 :string-ptr) (arg2 :bool))))
  "Sets a callback.")

;; A callback definer.
(affi:define-callback-definer define-bar-callback :int (arg1 :string-ptr) (arg2 :bool))

;; A callback named foo2
(define-bar-callback foo2 (str b)
  ;; Do something cool and return an integer
  )

;; Set foo and foo2 as callbacks
(set-callback #'foo)
(set-callback #'foo2)

;; Call foo and foo2
(foo "Hey" t)
(foo2 "This works too!" nil)

;; Defining another function with the extended version
(affi:define-c-function get-size (err-code :int)
     ((w (:pointer :int) :private t :with (affi:foreign-alloc :int))  ; Stack allocated int (yes, stack)
      (h (:pointer :int) :private t :with (affi:foreign-alloc :int))) ; Stack allocated int (yes, stack)
  (declare (ignore err-code))
  (values (affi:mem-ref w) (affi:mem-ref h)))

(multiple-value-bind (w h) (get-size)
  ;; Do something with the size
  )
}

@subsubtitle{Structures}

@code-block[:lang "common-lisp"]{
(affi:defcstruct foo
  (str (:string-array 100) :with "This is the default value of str.")
  (elements (:list-ptr :float size)) ; The type uses size slot
  (size :size :private t))           ; The size slot is private

(let ((lisp-foo (make-foo :elements '(1.5 2.6 3.7)))) ;; A regular list object
  (with-slots (str elements) lisp-foo
    ;; (slot-value lisp-foo 'size) <- Error, size is private
    (print str))) ; -> "This is the default value of str."

(affi:with-foreign-object (ptr-foo (:struct foo)) ; Stack allocated foreign structure
  (with-slots (str elements) ptr-foo  ; Same syntax as regular lisp objects
    ;; (slot-value lisp-foo 'size) <- Error, size is private
    (print str) ; -> "This is the default value of str."
    (setf elements '(1.0 2.0 3.0)))) ; The size slot is set automatically to 3.
                                     ; The allocated memory is also freed automatically.
                                     ; But not be scared, you have total control over memory management.
}
