;;; Test file for error-without-custom-condition rule

;; Bad: error with bare string literal
(defun check-positive (n)
  (when (< n 0)
    (error "n must be positive")))

;; Bad: error with format string and arguments
(defun validate-range (x limit)
  (when (< x limit)
    (error "value ~A below limit" x)))

;; Bad: error with CL-qualified condition symbol
(defun check-type-qualified (x)
  (when (not (integerp x))
    (error 'cl:simple-error :message "bad")))

;; Bad: error with unqualified known CL condition name
(defun check-type-unqualified (x)
  (when (not (stringp x))
    (error 'simple-error "message")))

;; Good: error with custom condition type
(defun check-bounds (x)
  (when (< x 0)
    (error 'out-of-bounds :value x)))

;; Good: no error calls
(defun add (x y)
  (+ x y))
