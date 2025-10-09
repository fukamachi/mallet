(uiop:define-package #:malo/rules
  (:use #:cl)
  (:use-reexport #:malo/rules/base
                 #:malo/rules/text
                 #:malo/rules/forms/control-flow
                 #:malo/rules/forms/variables
                 #:malo/rules/forms/package))
(in-package #:malo/rules)
