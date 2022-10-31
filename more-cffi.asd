

(asdf:defsystem "more-cffi"
  :author "Hector Galbis Sanchis <hectometrocuadrado@gmail.com>"
  :description "Extension of the CFFI project. A facility to wrap C bindings and write documentation."
  :license "The Unlicense"
  :depends-on ("cffi" "alexandria" "iterate" "adp")
  :components ((:file "package")
	       (:file "more-cffi")))
