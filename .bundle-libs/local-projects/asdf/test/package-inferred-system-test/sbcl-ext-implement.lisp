(defpackage package-inferred-system-test/sbcl-ext-implement
  (:use :cl)
  #+sbcl (:implement :package-inferred-system-test/sbcl-ext-lock)
  #-sbcl (:import-from :package-inferred-system-test/sbcl-ext-lock))

(in-package :package-inferred-system-test/sbcl-ext-implement)

#-ecl                                   ; there's no facility to unlock as in Allegro and SBCL.
(defun package-inferred-system-test/sbcl-ext-lock:implemented-by-other-package ()
  "implemented by other package")
