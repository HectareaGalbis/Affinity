

(defsystem "affinity"
  :author "Hector Galbis Sanchis <hectometrocuadrado@gmail.com>"
  :description "A higher layer of abstraction over CFFI."
  :license "MIT"
  :depends-on ("cffi" "clith" "alexandria" "named-readtables" "allioli" "expanders")
  :serial t
  :components ((:file "src/package")
               (:file "src/readtable")
               (:module "src"
                :serial t
                :components ((:file "types")
                             (:file "slots")
                             (:file "functions")
                             (:file "pointer")
                             (:file "structure")
                             (:file "string")))))


;; (asdf:defsystem "affinity/doc"
;;   :author "Hector Galbis Sanchis <hectometrocuadrado@gmail.com>"
;;   :description "Extension of the CFFI project. A facility to wrap C bindings and write documentation."
;;   :defsystem-depends-on ("adp-github")
;;   :depends-on ("more-cffi")
;;   :components (:module "scribble"
;;                :components ((:scribble "README"))))
