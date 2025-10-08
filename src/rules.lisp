(uiop:define-package #:malvolio/rules
  (:use #:cl)
  (:use-reexport #:malvolio/rules/base
                 #:malvolio/rules/text
                 #:malvolio/rules/tokens
                 #:malvolio/rules/forms/control-flow
                 #:malvolio/rules/forms/variables))
(in-package #:malvolio/rules)
