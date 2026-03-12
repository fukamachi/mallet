;;; Test cases for double-colon-access rule

;;; Violations - double-colon internal symbol access
(defun bad-function ()
  (some-package::internal-function 42))

(defun another-bad ()
  (let ((x (other-pkg::private-value)))
    x))

;;; Violation in a method
(defmethod my-method (obj)
  (third-lib::internal-helper obj))

;;; Valid - single colon public access
(defun good-function ()
  (some-package:public-function 42))

;;; Valid - unqualified symbols
(defun also-good ()
  (let ((x 1))
    (+ x 2)))

;;; Valid - uninterned symbols
(defpackage #:my-package
  (:use #:cl)
  (:export #:my-function))

;;; Valid - keywords
(defun keyword-user ()
  (list :foo :bar :baz))
