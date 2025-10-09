(defsystem "malvolio"
  :version "0.1.0"
  :description "A relentless guardian of code integrity for Common Lisp"
  :author "Eitaro Fukamachi <e.arrows@gmail.com>"
  :license "MIT"
  :depends-on ("alexandria"
               "eclector")
  :in-order-to ((test-op (test-op "malvolio/tests")))
  :pathname "src"
  :serial t
  :components
  (;; Core data structures
   (:file "violation")
   (:file "parser")

   ;; Parsing implementations
   (:module "parser-impl"
    :pathname "parser"
    :components
    ((:file "text")
     (:file "tokenizer")
     (:file "reader")))

   ;; Rule system
   (:module "rules-impl"
    :pathname "rules"
    :components
    ((:file "base")
     (:file "text")
     (:module "forms"
      :pathname "forms"
      :components
      ((:file "control-flow")
       (:file "variables")))))
   (:file "rules")

   ;; Configuration
   (:file "config")

   ;; Linting engine and formatters
   (:file "engine")
   (:file "formatter")

   (:file "main")))

(defsystem "malvolio/tests"
  :depends-on ("malvolio"
               "cl-ppcre"
               "rove")
  :pathname "tests"
  :components
  ((:file "config-test")

   (:module "parser"
    :pathname "parser"
    :components
    ((:file "tokenizer-test")
     (:file "reader-test")
     (:file "unknown-reader-macros")))

   (:module "rules"
    :pathname "rules"
    :components
    ((:file "registry-test")
     (:file "line-length-test")
     (:file "if-without-else-test")
     (:file "bare-progn-in-if-test")
     (:file "missing-otherwise-test")
     (:file "wrong-otherwise-test")
     (:file "unused-variables-test")
     (:file "text-formatting-test"))))

  :perform (test-op (o c) (symbol-call :rove '#:run c)))
