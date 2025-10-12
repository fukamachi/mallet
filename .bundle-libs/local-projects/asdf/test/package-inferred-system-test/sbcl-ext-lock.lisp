(defpackage package-inferred-system-test/sbcl-ext-lock
  (:use :cl)
  #+(or sbcl ecl) (:lock t)
  #+allegro (:implementation-packages :package-inferred-system-test/sbcl-ext-implement)
  (:export #:implemented-by-other-package))

(in-package :package-inferred-system-test/sbcl-ext-lock)
