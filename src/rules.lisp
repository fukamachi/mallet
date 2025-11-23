(uiop:define-package #:mallet/rules
  (:use #:cl)
  (:use-reexport #:mallet/rules/base
                 #:mallet/rules/text
                 #:mallet/rules/forms/control-flow
                 #:mallet/rules/forms/variables
                 #:mallet/rules/forms/local-functions
                 #:mallet/rules/forms/package
                 #:mallet/rules/forms/naming
                 #:mallet/rules/forms/lambda-list
                 #:mallet/rules/forms/asdf
                 #:mallet/rules/forms/metrics)
  (:export #:make-rule))
(in-package #:mallet/rules)

(defun make-rule (name &rest options &key severity &allow-other-keys)
  "Create a rule instance based on NAME and OPTIONS.
Always returns a rule object - enabled/disabled state is handled by config.
Severity defaults are defined in each rule class's :default-initargs."
  (let ((initargs (append
                   (and severity
                        `(:severity ,severity))
                   options)))
    ;; Create rule instance based on name
    (case name
      ;; Text rules
      (:line-length
       (apply #'make-instance 'line-length-rule
              initargs))
      (:trailing-whitespace
       (apply #'make-instance 'trailing-whitespace-rule
              initargs))
      (:no-tabs
       (apply #'make-instance 'no-tabs-rule
              initargs))
      (:final-newline
       (apply #'make-instance 'final-newline-rule
              initargs))
      (:consecutive-blank-lines
       (apply #'make-instance 'consecutive-blank-lines-rule
              initargs))

      ;; Form rules
      (:if-without-else
       (apply #'make-instance 'if-without-else-rule
              initargs))
      (:bare-progn-in-if
       (apply #'make-instance 'bare-progn-in-if-rule
              initargs))
      (:missing-otherwise
       (apply #'make-instance 'missing-otherwise-rule
              initargs))
      (:wrong-otherwise
       (apply #'make-instance 'wrong-otherwise-rule
              initargs))
      (:needless-let*
       (apply #'make-instance 'needless-let*-rule
              initargs))
      (:unused-variables
       (apply #'make-instance 'unused-variables-rule
              initargs))
      (:unused-loop-variables
       (apply #'make-instance 'unused-loop-variables-rule
              initargs))
      (:unused-local-functions
       (apply #'make-instance 'unused-local-functions-rule
              initargs))
      (:unused-local-nicknames
       (apply #'make-instance 'unused-local-nicknames-rule
              initargs))
      (:unused-imported-symbols
       (apply #'make-instance 'unused-imported-symbols-rule
              initargs))
      (:special-variable-naming
       (apply #'make-instance 'special-variable-naming-rule
              initargs))
      (:constant-naming
       (apply #'make-instance 'constant-naming-rule
              initargs))
      (:mixed-optional-and-key
       (apply #'make-instance 'mixed-optional-and-key-rule
              initargs))
      (:asdf-component-strings
       (apply #'make-instance 'asdf-component-strings-rule
              initargs))

      ;; Metric rules
      (:function-length
       (apply #'make-instance 'function-length-rule
              initargs))
      (:cyclomatic-complexity
       (apply #'make-instance 'cyclomatic-complexity-rule
              initargs))

      (otherwise
       (error "Unknown rule name: ~A" name)))))
