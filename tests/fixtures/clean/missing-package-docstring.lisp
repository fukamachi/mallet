;; Clean fixtures for missing-package-docstring rule

;; All packages have :documentation options
(defpackage #:well-documented
  (:use #:cl)
  (:documentation "This package is well-documented."))

(define-package #:also-documented
  (:use #:cl)
  (:documentation "Also documented."))
