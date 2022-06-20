

(asdf:defsystem "more-cffi"
  :depends-on ("cffi" "alexandria" "iterate")
  :components ((:file "package")
	       (:file "more-cffi")))
