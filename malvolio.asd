(defsystem "malvolio"
  :version "0.1.0"
  :description "A relentless guardian of code integrity for Common Lisp"
  :author "Eitaro Fukamachi <e.arrows@gmail.com>"
  :license "MIT"
  :depends-on ("alexandria"
               "cl-ppcre"
               "split-sequence"
               "yason")
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
   (:file "rules")

   ;; Linting engine and formatters
   (:file "engine")
   (:file "formatter")

   (:file "main")))

(defsystem "malvolio/tests"
  :depends-on ("malvolio"
               "rove")
  :pathname "tests"
  :components
  ((:module "parser"
    :pathname "parser"
    :components
    ((:file "tokenizer-test")
     (:file "reader-test")))

   (:module "rules"
    :pathname "rules"
    :components
    ((:file "registry-test")
     (:file "line-length-test")
     (:file "comment-level-test"))))

  :perform (test-op (o c) (symbol-call :rove '#:run c)))
