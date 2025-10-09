;; Test file with unbalanced parens - closes more than it opens

(defun bar ()
  (let ((x 10)  ;; Missing closing paren for let bindings
    (+ x 20)))
