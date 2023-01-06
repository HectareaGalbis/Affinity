

(asdf:defsystem "more-cffi"
  :author "Hector Galbis Sanchis <hectometrocuadrado@gmail.com>"
  :description "Extension of the CFFI project. A facility to wrap C bindings and write documentation."
  :license "The Unlicense"
  :depends-on ("cffi" "alexandria" "iterate" "adp" "clith")
  :components ((:file "package")
	       (:file "more-cffi")))


(asdf:defsystem "more-cffi/docs"
  :author "Hector Galbis Sanchis <hectometrocuadrado@gmail.com>"
  :description "Extension of the CFFI project. A facility to wrap C bindings and write documentation."
  :license "The Unlicense"
  :depends-on ("cffi" "alexandria" "iterate" "adp" "clith")
  :components ((:file "package")
	       (:file "readme")
	       (:file "more-cffi")))
