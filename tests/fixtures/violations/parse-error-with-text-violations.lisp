;; Test that text-level violations are reported even with parse errors

(defun foo ()
  (let ((x 10))
    (+ x 20)
;; Missing closing parens - parse error - line 3 has trailing spaces
