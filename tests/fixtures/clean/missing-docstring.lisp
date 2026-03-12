;;; Clean examples for missing-docstring rule

(defun add (x y)
  "Add X and Y."
  (+ x y))

(defmacro when+ (condition &body body)
  "Execute BODY when CONDITION is true."
  `(when ,condition ,@body))

(defgeneric serialize (object)
  (:documentation "Serialize OBJECT to a string representation."))

(defclass point ()
  ((x :initarg :x :accessor point-x)
   (y :initarg :y :accessor point-y))
  (:documentation "A 2D point in Cartesian coordinates."))

;; defmethod is always skipped — no docstring required
(defmethod serialize ((p point))
  (format nil "(~A, ~A)" (point-x p) (point-y p)))
