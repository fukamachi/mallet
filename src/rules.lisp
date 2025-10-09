(uiop:define-package #:malvolio/rules
  (:use #:cl)
  (:use-reexport #:malvolio/rules/base
                 #:malvolio/rules/text
                 #:malvolio/rules/forms/control-flow
                 #:malvolio/rules/forms/variables
                 #:malvolio/rules/forms/package))
(in-package #:malvolio/rules)
