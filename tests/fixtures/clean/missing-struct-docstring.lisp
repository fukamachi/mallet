;;; Clean examples for missing-struct-docstring rule

;; defstruct with body docstring
(defstruct point
  "A 2D point."
  x
  y)

;; defstruct with :documentation in name-and-options
(defstruct (node (:documentation "A linked list node."))
  value
  next)
