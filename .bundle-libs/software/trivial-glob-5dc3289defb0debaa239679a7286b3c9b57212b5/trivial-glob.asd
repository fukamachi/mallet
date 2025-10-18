(defsystem "trivial-glob"
  :description "Shell-style glob pattern matching and filesystem globbing for Common Lisp"
  :author "Eitaro Fukamachi <e.arrows@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("uiop")
  :pathname "src"
  :serial t
  :components
  ((:file "compiler")
   (:file "filesystem")
   (:file "main"))
  :in-order-to ((test-op (test-op "trivial-glob/tests"))))

(defsystem "trivial-glob/tests"
  :description "Test suite for trivial-glob"
  :depends-on ("trivial-glob"
               "rove")
  :pathname "tests"
  :serial t
  :components
  ((:file "compiler")
   (:file "filesystem"))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
