;; Test fixture for package-per-file rule
;; This file uses in-package without a defpackage defined in this file.

(in-package #:cl-user)

(defun greet (name)
  "Say hello to NAME."
  (format nil "Hello, ~A!" name))
