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
  ;; Helper to add :severity only if explicitly provided
  (flet ((add-severity (initargs)
           (if severity
               (list* :severity severity initargs)
               initargs)))
    ;; Create rule instance based on name
    (case name
      ;; Text rules
      (:line-length
       (apply #'make-instance 'line-length-rule
              (add-severity (list :max (getf options :max 80)))))
      (:trailing-whitespace
       (apply #'make-instance 'trailing-whitespace-rule
              (add-severity nil)))
      (:no-tabs
       (apply #'make-instance 'no-tabs-rule
              (add-severity nil)))
      (:final-newline
       (apply #'make-instance 'final-newline-rule
              (add-severity nil)))
      (:consecutive-blank-lines
       (apply #'make-instance 'consecutive-blank-lines-rule
              (add-severity (list :max (getf options :max 2)))))

      ;; Form rules
      (:if-without-else
       (apply #'make-instance 'if-without-else-rule
              (add-severity nil)))
      (:bare-progn-in-if
       (apply #'make-instance 'bare-progn-in-if-rule
              (add-severity nil)))
      (:missing-otherwise
       (apply #'make-instance 'missing-otherwise-rule
              (add-severity nil)))
      (:wrong-otherwise
       (apply #'make-instance 'wrong-otherwise-rule
              (add-severity nil)))
      (:unused-variables
       (apply #'make-instance 'unused-variables-rule
              (add-severity nil)))
      (:unused-loop-variables
       (apply #'make-instance 'unused-loop-variables-rule
              (add-severity nil)))
      (:unused-local-functions
       (apply #'make-instance 'unused-local-functions-rule
              (add-severity nil)))
      (:unused-local-nicknames
       (apply #'make-instance 'unused-local-nicknames-rule
              (add-severity nil)))
      (:unused-imported-symbols
       (apply #'make-instance 'unused-imported-symbols-rule
              (add-severity nil)))
      (:special-variable-naming
       (apply #'make-instance 'special-variable-naming-rule
              (add-severity nil)))
      (:constant-naming
       (apply #'make-instance 'constant-naming-rule
              (add-severity nil)))
      (:mixed-optional-and-key
       (apply #'make-instance 'mixed-optional-and-key-rule
              (add-severity nil)))
      (:asdf-component-strings
       (apply #'make-instance 'asdf-component-strings-rule
              (add-severity nil)))

      ;; Metric rules
      (:function-length
       (apply #'make-instance 'function-length-rule
              (add-severity (list :max (getf options :max 50)))))
      (:cyclomatic-complexity
       (apply #'make-instance 'cyclomatic-complexity-rule
              (add-severity (list :max (getf options :max 20)
                                  :variant (getf options :variant :standard)))))

      (otherwise
       (error "Unknown rule name: ~A" name)))))
