;; Test fixtures for missing-struct-docstring rule

;; Bad: defstruct with simple name and no docstring
(defstruct point
  x
  y)

;; Good: defstruct with body docstring
(defstruct color
  "An RGB color value."
  red
  green
  blue)

;; Bad: defstruct with name-and-options but no :documentation
(defstruct (node (:conc-name node-))
  value
  next)

;; Good: defstruct with :documentation in name-and-options
(defstruct (edge (:documentation "A graph edge."))
  from
  to)

;; Bad: defstruct with no slots and no docstring
(defstruct empty-marker)
