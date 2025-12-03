;;; Clean example for interned-package-symbol rule

(defpackage #:clean-pkg
  (:use #:cl)
  (:export #:foo #:bar))
(in-package #:clean-pkg)
