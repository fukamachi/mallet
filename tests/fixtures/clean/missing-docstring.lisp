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

;; deftype with single body form (type expansion, not checkable)
(deftype index ()
  '(integer 0))

;; deftype with docstring + expansion
(deftype non-negative ()
  "A non-negative integer type."
  '(integer 0))

(define-condition app-error (error)
  ()
  (:documentation "Base class for application errors."))
