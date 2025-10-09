;; Test file with unmatched opening paren (premature EOF)

(defun foo ()
  (let ((x 10))
    (+ x 20)
;; Missing closing parens - should report parse error
