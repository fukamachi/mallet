(uiop:define-package #:mallet/rules
  (:use #:cl)
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
                 #:mallet/rules/forms/metrics
                 #:mallet/rules/forms/eval-usage
                 #:mallet/rules/forms/runtime-intern
                 #:mallet/rules/forms/runtime-unintern
                 #:mallet/rules/forms/ignore-errors-usage
                 #:mallet/rules/forms/error-usage
                 #:mallet/rules/stale-suppression)
  (:export #:make-rule))
(in-package #:mallet/rules)

(defun make-rule (name &rest options)
  "Create a rule instance based on NAME and OPTIONS.
Always returns a rule object - enabled/disabled state is handled by config.
Severity and category defaults are defined in each rule class's :default-initargs."
  ;; Create rule instance based on name
  (case name
    ;; Text rules
    (:line-length
     (apply #'make-instance 'line-length-rule options))
    (:trailing-whitespace
     (apply #'make-instance 'trailing-whitespace-rule options))
    (:no-tabs
     (apply #'make-instance 'no-tabs-rule options))
    (:final-newline
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
    (:if-without-else
     (apply #'make-instance 'if-without-else-rule options))
    (:bare-progn-in-if
     (apply #'make-instance 'bare-progn-in-if-rule options))
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
    (:interned-package-symbol
     (apply #'make-instance 'interned-package-symbol-rule options))
    (:unused-local-nicknames
     (apply #'make-instance 'unused-local-nicknames-rule options))
    (:unused-imported-symbols
     (apply #'make-instance 'unused-imported-symbols-rule options))
    (:no-package-use
     (apply #'make-instance 'no-package-use-rule options))
    (:special-variable-naming
     (apply #'make-instance 'special-variable-naming-rule options))
    (:constant-naming
     (apply #'make-instance 'constant-naming-rule options))
    (:mixed-optional-and-key
     (apply #'make-instance 'mixed-optional-and-key-rule options))
    (:allow-other-keys
     (apply #'make-instance 'allow-other-keys-rule options))
    (:asdf-component-strings
     (apply #'make-instance 'asdf-component-strings-rule options))

    ;; Safety rules
    (:eval-usage
     (apply #'make-instance 'eval-usage-rule options))
    (:runtime-intern
     (apply #'make-instance 'runtime-intern-rule options))
    (:runtime-unintern
     (apply #'make-instance 'runtime-unintern-rule options))
    (:ignore-errors-usage
     (apply #'make-instance 'ignore-errors-usage-rule options))
    (:error-with-string-only
     (apply #'make-instance 'error-with-string-only-rule options))

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
     (error "Unknown rule name: ~A" name))))
