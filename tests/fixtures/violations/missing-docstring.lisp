;; Test fixtures for missing-docstring rule

;; Bad: defun without docstring
(defun undocumented-fn (x y) (+ x y))

;; Good: defun with docstring
(defun documented-fn (x y)
  "Add X and Y."
  (+ x y))

;; Bad: defmacro without docstring
(defmacro undocumented-macro (c &body b) `(when ,c ,@b))

;; Bad: defgeneric without :documentation
(defgeneric undocumented-gf (x))

;; Good: defgeneric with :documentation
(defgeneric documented-gf (x)
  (:documentation "Does something with X."))

;; Bad: defclass without :documentation
(defclass undocumented-class () ())

;; Good: defclass with :documentation
(defclass documented-class ()
  ()
  (:documentation "A documented class."))

;; Good: defmethod — always skipped
(defmethod undocumented-gf ((x integer)) (* x 2))

;; Bad: deftype with 2-body forms but no docstring
;; (2 forms allows a docstring; first form is not a string so it's missing)
(deftype undocumented-type (n)
  (declare (type integer n))
  '(integer 0))

;; Good: deftype with single body form (type expansion, not checkable)
(deftype single-expansion ()
  'integer)

;; Good: deftype with docstring + expansion
(deftype documented-type ()
  "A documented numeric type."
  'integer)

;; Bad: define-condition without :documentation
(define-condition undocumented-error (error)
  ())

;; Good: define-condition with :documentation
(define-condition documented-error (error)
  ()
  (:documentation "A documented error condition."))
