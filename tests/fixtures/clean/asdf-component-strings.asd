;; Clean fixture for asdf-component-strings rule
;; All system names, dependencies, and component names use strings

(defsystem "asdf-component-strings"
  :description "A well-formed ASDF system using strings throughout"
  :depends-on ("alexandria" "cl-ppcre")
  :components ((:file "main")
               (:file "utils")
               (:module "impl"
                :components ((:file "core")
                             (:file "helpers")))))

(defsystem "asdf-component-strings/tests"
  :depends-on ("asdf-component-strings" "fiveam")
  :components ((:file "tests")))
