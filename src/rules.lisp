(uiop:define-package #:mallet/rules
  (:use #:cl)
  (:local-nicknames (#:utils #:mallet/utils))
  (:use-reexport #:mallet/rules/base
                 #:mallet/rules/text
                 #:mallet/rules/tokens/bare-float-literal
                 #:mallet/rules/tokens/double-colon-access
                 #:mallet/rules/forms/control-flow
                 #:mallet/rules/forms/variables
                 #:mallet/rules/forms/local-functions
                 #:mallet/rules/forms/package
                 #:mallet/rules/forms/naming
                 #:mallet/rules/forms/lambda-list
                 #:mallet/rules/forms/asdf
                 #:mallet/rules/forms/asdf-defsystem
                 #:mallet/rules/forms/metrics
                 #:mallet/rules/forms/no-eval
                 #:mallet/rules/forms/runtime-intern
                 #:mallet/rules/forms/runtime-unintern
                 #:mallet/rules/forms/no-ignore-errors
                 #:mallet/rules/forms/error-usage
                 #:mallet/rules/forms/docstring
                 #:mallet/rules/stale-suppression
                 #:mallet/rules/asdf-reader-conditional
                 #:mallet/rules/forms/coalton)
  (:export #:make-rule))
(in-package #:mallet/rules)

(defun make-rule (name &rest options)
  "Create a rule instance based on NAME and OPTIONS.
Always returns a rule object - enabled/disabled state is handled by config.
Severity and category defaults are defined in each rule class's :default-initargs."
  ;; Resolve deprecated aliases before dispatch
  (let ((name (utils:resolve-rule-alias name)))
    ;; Create rule instance based on name
    (case name
      ;; Text rules
      (:line-length
       (apply #'make-instance 'line-length-rule options))
      (:trailing-whitespace
       (apply #'make-instance 'trailing-whitespace-rule options))
      (:no-tabs
       (apply #'make-instance 'no-tabs-rule options))
      (:missing-final-newline
       (apply #'make-instance 'final-newline-rule options))
      (:consecutive-blank-lines
       (apply #'make-instance 'consecutive-blank-lines-rule options))
      (:closing-paren-on-own-line
       (apply #'make-instance 'closing-paren-on-own-line-rule options))

      ;; Token rules
      (:bare-float-literal
       (apply #'make-instance 'bare-float-literal-rule options))
      (:double-colon-access
       (apply #'make-instance 'double-colon-access-rule options))

      ;; Form rules
      (:missing-else
       (apply #'make-instance 'if-without-else-rule options))
      (:progn-in-conditional
       (apply #'make-instance 'progn-in-conditional-rule options))
      (:redundant-progn
       (apply #'make-instance 'redundant-progn-rule options))
      (:missing-otherwise
       (apply #'make-instance 'missing-otherwise-rule options))
      (:wrong-otherwise
       (apply #'make-instance 'wrong-otherwise-rule options))
      (:needless-let*
       (apply #'make-instance 'needless-let*-rule options))
      (:unused-variables
       (apply #'make-instance 'unused-variables-rule options))
      (:unused-loop-variables
       (apply #'make-instance 'unused-loop-variables-rule options))
      (:unused-local-functions
       (apply #'make-instance 'unused-local-functions-rule options))
      (:defpackage-interned-symbol
       (apply #'make-instance 'interned-package-symbol-rule options))
      (:unused-local-nicknames
       (apply #'make-instance 'unused-local-nicknames-rule options))
      (:unused-imported-symbols
       (apply #'make-instance 'unused-imported-symbols-rule options))
      (:no-package-use
       (apply #'make-instance 'no-package-use-rule options))
      (:one-package-per-file
       (apply #'make-instance 'one-package-per-file-rule options))
      (:special-variable-naming
       (apply #'make-instance 'special-variable-naming-rule options))
      (:constant-naming
       (apply #'make-instance 'constant-naming-rule options))
      (:mixed-optional-and-key
       (apply #'make-instance 'mixed-optional-and-key-rule options))
      (:no-allow-other-keys
       (apply #'make-instance 'allow-other-keys-rule options))
      (:asdf-component-strings
       (apply #'make-instance 'asdf-component-strings-rule options))
      (:asdf-redundant-package-prefix
       (apply #'make-instance 'asdf-redundant-package-prefix-rule options))
      (:asdf-operate-in-perform
       (apply #'make-instance 'asdf-operate-in-perform-rule options))
      (:asdf-secondary-system-name
       (apply #'make-instance 'asdf-secondary-system-name-rule options))
      (:asdf-if-feature-keyword
       (apply #'make-instance 'asdf-if-feature-keyword-rule options))
      (:asdf-reader-conditional
       (apply #'make-instance 'asdf-reader-conditional-rule options))

      ;; Safety rules
      (:no-eval
       (apply #'make-instance 'no-eval-rule options))
      (:runtime-intern
       (apply #'make-instance 'runtime-intern-rule options))
      (:runtime-unintern
       (apply #'make-instance 'runtime-unintern-rule options))
      (:no-ignore-errors
       (apply #'make-instance 'no-ignore-errors-rule options))
      (:error-with-string-only
       (apply #'make-instance 'error-with-string-only-rule options))
      (:missing-docstring
       (apply #'make-instance 'missing-docstring-rule options))
      (:missing-package-docstring
       (apply #'make-instance 'missing-package-docstring-rule options))
      (:missing-variable-docstring
       (apply #'make-instance 'missing-variable-docstring-rule options))
      (:missing-struct-docstring
       (apply #'make-instance 'missing-struct-docstring-rule options))

      ;; Coalton rules
      (:coalton-missing-declare
       (apply #'make-instance 'coalton-missing-declare-rule options))

      ;; Suppression rules
      (:stale-suppression
       (apply #'make-instance 'stale-suppression-rule options))

      ;; Metric rules
      (:function-length
       (apply #'make-instance 'function-length-rule options))
      (:cyclomatic-complexity
       (apply #'make-instance 'cyclomatic-complexity-rule options))
      (:comment-ratio
       (apply #'make-instance 'comment-ratio-rule options))

      (otherwise
       (error "Unknown rule name: ~A" name)))))
