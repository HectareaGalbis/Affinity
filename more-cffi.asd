

(asdf:defsystem "more-cffi"
  :author "Hector Galbis Sanchis <hectometrocuadrado@gmail.com>"
  :description "Extension of the CFFI project. A facility to wrap C bindings."
  :license "MIT"
  :depends-on ("cffi")
  :components ((:module "src"
                :components ((:file "package")
	                     (:file "more-cffi")))))


;; (asdf:defsystem "more-cffi/doc"
;;   :author "Hector Galbis Sanchis <hectometrocuadrado@gmail.com>"
;;   :description "Extension of the CFFI project. A facility to wrap C bindings and write documentation."
;;   :defsystem-depends-on ("adp-github")
;;   :depends-on ("more-cffi")
;;   :components (:module "scribble"
;;                :components ((:scribble "README"))))
