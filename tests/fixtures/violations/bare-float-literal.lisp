;;; Test cases for bare-float-literal rule

;;; Violations - bare decimal floats
(defparameter *bare-decimal-1* 1.0)
(defparameter *bare-decimal-2* 0.5)
(defparameter *bare-decimal-3* -3.14)

;;; Violations - floats with e marker (ambiguous type)
(defparameter *e-marker-1* 1e10)
(defparameter *e-marker-2* 1.0e0)
(defparameter *e-marker-3* 0.5e5)

;;; Valid - explicit type markers (no violations)
(defparameter *explicit-f* 1.0f0)
(defparameter *explicit-d* 1.0d0)
(defparameter *explicit-s* 1.0s0)
(defparameter *explicit-l* 1.0l0)
(defparameter *explicit-d-exp* 1d10)

;;; Valid - integers and ratios (not floats)
(defparameter *integer* 42)
(defparameter *ratio* 1/2)

;;; Function with multiple bare floats
(defun test-function ()
  (let ((x 0.5)
        (y 0.5e0)
        (z 0.5f0))
    (+ x y z 3.14)))
