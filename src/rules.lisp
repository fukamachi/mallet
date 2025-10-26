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
Always returns a rule object - enabled/disabled state is handled by config."
  ;; Create rule instance based on name
  (case name
    ;; Text rules
    (:line-length
     (make-instance 'line-length-rule
                    :severity (or severity :info)
                    :max (getf options :max 80)))
    (:trailing-whitespace
     (make-instance 'trailing-whitespace-rule
                    :severity (or severity :format)))
    (:no-tabs
     (make-instance 'no-tabs-rule
                    :severity (or severity :format)))
    (:final-newline
     (make-instance 'final-newline-rule
                    :severity (or severity :format)))
    (:consecutive-blank-lines
     (make-instance 'consecutive-blank-lines-rule
                    :severity (or severity :info)
                    :max (getf options :max 2)))

    ;; Form rules
    (:if-without-else
     (make-instance 'if-without-else-rule
                    :severity (or severity :convention)))
    (:bare-progn-in-if
     (make-instance 'bare-progn-in-if-rule
                    :severity (or severity :convention)))
    (:missing-otherwise
     (make-instance 'missing-otherwise-rule
                    :severity (or severity :warning)))
    (:wrong-otherwise
     (make-instance 'wrong-otherwise-rule
                    :severity (or severity :error)))
    (:unused-variables
     (make-instance 'unused-variables-rule
                    :severity (or severity :warning)))
    (:unused-loop-variables
     (make-instance 'unused-loop-variables-rule
                    :severity (or severity :info)))
    (:unused-local-functions
     (make-instance 'unused-local-functions-rule
                    :severity (or severity :warning)))
    (:unused-local-nicknames
     (make-instance 'unused-local-nicknames-rule
                    :severity (or severity :info)))
    (:unused-imported-symbols
     (make-instance 'unused-imported-symbols-rule
                    :severity (or severity :info)))
    (:special-variable-naming
     (make-instance 'special-variable-naming-rule
                    :severity (or severity :convention)))
    (:constant-naming
     (make-instance 'constant-naming-rule
                    :severity (or severity :info)))
    (:mixed-optional-and-key
     (make-instance 'mixed-optional-and-key-rule
                    :severity (or severity :warning)))
    (:asdf-component-strings
     (make-instance 'asdf-component-strings-rule
                    :severity (or severity :convention)))

    ;; Metric rules
    (:function-length
     (make-instance 'function-length-rule
                    :severity (or severity :info)
                    :max (getf options :max 50)))
    (:cyclomatic-complexity
     (make-instance 'cyclomatic-complexity-rule
                    :severity (or severity :info)
                    :max (getf options :max 20)
                    :variant (getf options :variant :standard)))

    (otherwise
     (error "Unknown rule name: ~A" name))))
