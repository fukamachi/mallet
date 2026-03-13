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

;; Version dep-spec forms (bare version operator, not :version keyword) are valid
(defsystem "asdf-component-strings/with-version-deps"
  :depends-on ((version 'alexandria "1.0")
               (version "cl-ppcre" "2.0")
               (:feature :sbcl (version 'trivial-features "0.9")))
  :components ((:file "impl")))

(defsystem "asdf-component-strings/tests"
  :depends-on ("asdf-component-strings" "fiveam")
  :components ((:file "tests")))
