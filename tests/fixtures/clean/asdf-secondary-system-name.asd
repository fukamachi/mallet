;; Clean fixture for asdf-secondary-system-name rule
;; All secondary system names follow the primary/suffix convention

(defsystem "asdf-secondary-system-name"
  :description "Primary system"
  :depends-on ("alexandria")
  :components ((:file "main")))

(defsystem "asdf-secondary-system-name/tests"
  :description "Tests for primary system"
  :depends-on ("asdf-secondary-system-name" "fiveam")
  :components ((:file "tests")))

(defsystem "asdf-secondary-system-name/docs"
  :description "Documentation for primary system"
  :depends-on ("asdf-secondary-system-name")
  :components ((:file "docs")))
