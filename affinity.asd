

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
                             (:file "primitive")
                             (:file "functions")
                             (:file "pointer")
                             (:file "structure")
                             (:file "string")
                             (:file "list-array")
                             (:file "list-ptr")))))


;; (asdf:defsystem "affinity/docs"
;;   :author "Hector Galbis Sanchis <hectometrocuadrado@gmail.com>"
;;   :description "Dpcumentation of Affinity."
;;   :defsystem-depends-on ("adp-github")
;;   :class :adp-github
;;   :depends-on ("affinity")
;;   :components ((:module "scribble"
;;                 :components ((:file "package")
;;                              (:scribble "README")
;;                              (:scribble "reference")))))
