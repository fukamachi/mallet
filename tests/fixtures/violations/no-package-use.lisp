;;; Test fixture for no-package-use rule — violation cases

;; Bad: :use of a non-exempt package
(defpackage #:my-package
  (:use #:cl #:alexandria))
