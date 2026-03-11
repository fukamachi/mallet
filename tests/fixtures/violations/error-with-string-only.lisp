;;; Test file for error-with-string-only rule

;; Bad: error with bare string literal
(defun check-positive (n)
  (when (< n 0)
    (error "n must be positive")))

;; Bad: error with format string and arguments
(defun validate-range (x limit)
  (when (< x limit)
    (error "value ~A is below limit" x)))

;; Good: error with condition type
(defun check-bounds (x)
  (when (< x 0)
    (error 'out-of-bounds :value x)))

;; Good: no error calls
(defun add (x y)
  (+ x y))
