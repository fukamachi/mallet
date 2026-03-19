;; Clean fixture: file defines its own package before using in-package.

(defpackage #:my-clean-pkg
  (:use #:cl)
  (:documentation "A properly defined package."))

(in-package #:my-clean-pkg)

(defun hello ()
  "Return a greeting string."
  "Hello, World!")
