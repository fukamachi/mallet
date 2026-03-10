;;; Test fixture for comment-ratio rule

;; Good function - low comment ratio (should not violate with default max 0.3)
(defun clean-function (x)
  "Process and return x."
  (let ((y (* x 2)))
    (when (< 0 y)
      (print y)
      (+ y 1))))

;; Bad function - high comment ratio (should violate)
;; 6 comments out of 9 non-blank = 0.67 > 0.30
(defun over-commented (x)
  ;; First we need to validate the input
  ;; Make sure it is a number
  ;; Then process it
  (let ((y (* x 2)))
    ;; Now check if the result is valid
    ;; We want positive results only
    ;; Return the final value
    (abs y)))
