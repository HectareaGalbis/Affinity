

(defsystem "more-cffi"
  :author "Hector Galbis Sanchis <hectometrocuadrado@gmail.com>"
  :description "Extension of the CFFI project. A facility to wrap C bindings."
  :license "MIT"
  :depends-on ("cffi" "clith" "alexandria" "named-readtables" "allioli")
  :serial t
  :components ((:file "src/package")
               (:file "src/readtable")
               (:module "src"
                :components ((:file "owner")
                             (:file "pointer")))))


;; (asdf:defsystem "more-cffi/doc"
;;   :author "Hector Galbis Sanchis <hectometrocuadrado@gmail.com>"
;;   :description "Extension of the CFFI project. A facility to wrap C bindings and write documentation."
;;   :defsystem-depends-on ("adp-github")
;;   :depends-on ("more-cffi")
;;   :components (:module "scribble"
;;                :components ((:scribble "README"))))
