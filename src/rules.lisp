(uiop:define-package #:malo/rules
  (:use #:cl)
  (:use-reexport #:malo/rules/base
                 #:malo/rules/text
                 #:malo/rules/forms/control-flow
                 #:malo/rules/forms/variables
                 #:malo/rules/forms/package
                 #:malo/rules/forms/naming
                 #:malo/rules/forms/lambda-list
                 #:malo/rules/forms/asdf))
(in-package #:malo/rules)
