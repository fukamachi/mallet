;; Test file with multiple parse errors

(defun foo ()
  'hello))  ;; Extra closing paren

(defun bar ()
  (let ((x 10))
    (+ x 20)
;; Missing closing parens
