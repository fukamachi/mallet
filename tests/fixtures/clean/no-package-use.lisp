;;; Test fixture for no-package-use rule — ok cases

;; OK: :use of only exempt packages
(defpackage #:my-cl-package
  (:use #:cl))

(defpackage #:my-common-lisp-package
  (:use #:common-lisp))

(defpackage #:my-coalton-package
  (:use #:coalton))

(defpackage #:my-coalton-prelude-package
  (:use #:coalton-prelude))

;; OK: :use of multiple exempt packages together
(defpackage #:my-mixed-exempt-package
  (:use #:cl #:coalton-prelude))
