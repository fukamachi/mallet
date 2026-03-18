;; Test fixtures for missing-package-docstring rule

;; Bad: defpackage without :documentation
(defpackage #:my-undocumented-pkg
  (:use #:cl))

;; Good: defpackage with :documentation (should not be flagged)
(defpackage #:my-documented-pkg
  (:use #:cl)
  (:documentation "A documented package."))

;; Bad: define-package without :documentation
(define-package #:another-undocumented-pkg
  (:use #:cl))
