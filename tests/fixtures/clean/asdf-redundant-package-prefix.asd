;; Clean fixture for asdf-redundant-package-prefix rule
;; No redundant package prefixes in .asd file

(defsystem "asdf-redundant-package-prefix"
  :description "A well-formed ASDF system with no redundant prefixes"
  :depends-on ("alexandria" "cl-ppcre")
  :components ((:file "main")
               (:file "utils")))

(defsystem "asdf-redundant-package-prefix/tests"
  :depends-on ("asdf-redundant-package-prefix" "fiveam")
  :components ((:file "tests")))
