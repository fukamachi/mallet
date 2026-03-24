;;; Test file for error-without-custom-condition rule

;; Bad: error with bare string literal
(defun check-positive (n)
  (when (< n 0)
    (error "n must be positive")))

;; Bad: error with format string and arguments
(defun validate-range (x limit)
  (when (< x limit)
    (error "value ~A is below limit" x)))

;; Bad: error with CL-qualified built-in condition
(defun check-type-valid (x)
  (unless (integerp x)
    (error 'cl:simple-error :format-control "not an integer")))

;; Bad: error with unqualified known CL condition
(defun check-bounds (x)
  (when (< x 0)
    (error 'simple-error :format-control "out of bounds")))

;; Good: error with custom condition type
(defun check-range (x)
  (when (< x 0)
    (error 'out-of-bounds :value x)))

;; Good: no error calls
(defun add (x y)
  (+ x y))
