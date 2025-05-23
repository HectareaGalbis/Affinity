

<a id="TITLE:AFFI-DOCS:TAG17"></a>
# Affinity

Welcome to Affinity\!

**WARNING**\: This is still a WIP project\. It can still be broken and API can change heavily\.


* [Introduction](/README.md#TITLE:AFFI-DOCS:TAG18)
* [Installation](/README.md#TITLE:AFFI-DOCS:TAG19)
* [Reference](/README.md#TITLE:AFFI-DOCS:TAG20)
* [Quick view](/README.md#TITLE:AFFI-DOCS:TAG21)
  * [Functions and callbacks](/README.md#TITLE:AFFI-DOCS:TAG22)
  * [Structures](/README.md#TITLE:AFFI-DOCS:TAG23)


<a id="TITLE:AFFI-DOCS:TAG18"></a>
## Introduction

Affinity is a higher layer of abstraction over [CFFI](https://github.com/cffi/cffi)\. It tries to make the writing of bindings easier\. Specifically\, Affinity prioritize the next features in decreasing order\:

1. Ease of use\.
2. Power\.
3. Efficiency\.


<a id="TITLE:AFFI-DOCS:TAG19"></a>
## Installation

Manually for now\.

`````common-lisp
cd ~/common-lisp
git clone https://github.com/HectareaGalbis/Affinity.git
`````


<a id="TITLE:AFFI-DOCS:TAG20"></a>
## Reference

* [Reference](/docs/scribble/reference.md#TITLE:AFFI-DOCS:REFERENCE)



<a id="TITLE:AFFI-DOCS:TAG21"></a>
## Quick view

<a id="TITLE:AFFI-DOCS:TAG22"></a>
### Functions and callbacks

`````common-lisp
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
`````

<a id="TITLE:AFFI-DOCS:TAG23"></a>
### Structures

`````common-lisp
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
`````