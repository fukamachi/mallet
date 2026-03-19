(defsystem "mallet"
  :version "0.2.0"
  :description "A sensible Common Lisp linter that catches mistakes, not style"
  :author "Eitaro Fukamachi <e.arrows@gmail.com>"
  :license "MIT"
  :depends-on ("alexandria"
               "cl-ppcre"
               "eclector"
               "trivial-glob")
  :in-order-to ((test-op (test-op "mallet/tests")))
  :pathname "src"
  :build-operation "program-op"
  :build-pathname "../mallet"
  :entry-point "mallet:main"
  :serial t
  :components
  (;; Shared utilities
   (:file "utils")
   (:file "utils/scope")

   ;; Error conditions
   (:file "errors")

   ;; Core data structures
   (:file "violation")
   (:file "suppression")
   (:file "parser")

   ;; Parsing implementations
   (:module "parser-impl"
    :pathname "parser"
    :components
    ((:file "text")
     (:file "tokenizer")
     (:file "reader")
     (:file "loop")))

   ;; Rule system
   (:module "rules-impl"
    :pathname "rules"
    :serial t
    :components
    ((:file "base")
     (:file "text")
     (:file "forms/package-exports")
     (:module "tokens"
      :pathname "tokens"
      :components
      ((:file "bare-float-literal")
       (:file "double-colon-access")))
     (:module "forms"
      :pathname "forms"
      :components
      ((:file "control-flow")
       (:file "variables")
       (:file "local-functions")
       (:file "package")
       (:file "naming")
       (:file "lambda-list")
       (:file "asdf")
       (:file "asdf-defsystem")
       (:file "metrics")
       (:file "no-eval")
       (:file "runtime-intern")
       (:file "runtime-unintern")
       (:file "no-ignore-errors")
       (:file "error-usage")
       (:file "docstring")))
     (:file "stale-suppression")
     (:file "asdf-reader-conditional")))
   (:file "rules")

   ;; Configuration
   (:file "config")

   ;; Linting engine and formatters
   (:file "engine")
   (:file "formatter")
   (:file "fixer")

   (:file "main")))

(defsystem "mallet/tests"
  :depends-on ("mallet"
               "cl-ppcre"
               "rove")
  :pathname "tests"
  :components
  ((:file "errors-test")
   (:file "utils-test")
   (:file "config-test")
   (:file "config-ignore-test")
   (:file "cli-parsing-test")
   (:file "fixer-test")
   (:file "formatter-test")
   (:file "suppression")
   (:file "suppression-declarations")
   (:file "suppression-integration")
   (:file "comment-directives")
   (:file "engine-integration-test")
   (:file "engine-comment-suppression")
   (:file "engine-cross-file-integration-test")

   (:module "parser"
    :pathname "parser"
    :components
    ((:file "tokenizer-test")
     (:file "reader-test")
     (:file "loop-test")
     (:file "unknown-reader-macros")
     (:file "interpolation-extractor-test")))

   (:module "rules"
    :pathname "rules"
    :components
    ((:file "line-length-test")
     (:file "if-without-else-test")
     (:file "progn-in-conditional-test")
     (:file "redundant-progn-test")
     (:file "missing-otherwise-test")
     (:file "wrong-otherwise-test")
     (:file "unused-variables-test")
     (:file "needless-let-star-test")
     (:file "unused-local-nicknames-test")
     (:file "interned-package-symbol-test")
     (:file "unused-imported-symbols-test")
     (:file "text-formatting-test")
     (:file "closing-paren-on-own-line-test")
     (:file "naming")
     (:file "lambda-list")
     (:file "asdf")
     (:file "asdf-defsystem-test")
     (:file "asdf-reader-conditional-test")
     (:file "special-forms-test")
     (:file "with-macros-test")
     (:file "metrics-test")
     (:file "bare-float-literal-test")
     (:file "no-eval-test")
     (:file "runtime-intern-test")
     (:file "runtime-unintern-test")
     (:file "no-ignore-errors-test")
     (:file "no-package-use-test")
     (:file "double-colon-access-test")
     (:file "test-framework-detection-test")
     (:file "error-with-string-only-test")
     (:file "docstring-utilities-test")
     (:file "missing-docstring-test")
     (:file "missing-package-docstring-test")
     (:file "missing-variable-docstring-test")
     (:file "missing-struct-docstring-test")
     (:file "one-package-per-file-test")
     (:file "rule-type-system-test")
     (:file "stale-suppression-test")
     (:file "package-exports-test")
     (:file "coalton-rule-base-test"))))

  :perform (test-op (o c) (symbol-call :rove '#:run c)))
