;;; Test fixture for interned-package-symbol rule

(defpackage :keyword-pkg
  (:use :cl)
  (:export :foo :bar))
(in-package :keyword-pkg)

(defpackage bare-pkg
  (:use cl)
  (:export foo))
(in-package bare-pkg)

(uiop:define-package #:uiop-pkg
  (:mix :cl)
  (:recycle :cl))
(in-package #:uiop-pkg)
