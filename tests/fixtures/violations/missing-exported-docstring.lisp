;; Test fixtures for missing-exported-docstring rule

(defpackage #:mallet-fixture/missing-exported-docstring
  (:export #:exported-fn
           #:exported-macro
           #:exported-generic
           #:exported-class
           #:internal-fn))
(in-package #:mallet-fixture/missing-exported-docstring)

;; Bad: exported defun without docstring
(defun exported-fn (x) x)

;; Good: exported defun WITH docstring
(defun exported-fn-with-doc (x)
  "Returns X."
  x)

;; Bad: exported defmacro without docstring
(defmacro exported-macro (x) x)

;; Bad: exported defgeneric without docstring
(defgeneric exported-generic (x))

;; Bad: exported defclass without docstring
(defclass exported-class () ())

;; Good: internal (non-exported) defun without docstring — no violation
(defun internal-fn (x) x)
