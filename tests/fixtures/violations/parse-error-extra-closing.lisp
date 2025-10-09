;; Test file with extra closing parens

(defun foo ()
  "foo"
  'hello))  ;; Extra closing paren - should report parse error
