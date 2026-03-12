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
