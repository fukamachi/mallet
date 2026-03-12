;;; Test file for closing-paren-on-own-line rule

;; Bad: single closing paren on its own line (non-idiomatic CL style)
(defun remove-symbol (sym)
  (when sym
    (format t "removing ~A" sym)
    )
  )

;; Bad: multiple closing parens alone on a line
(defun nested-example (x)
  (let ((y (* x 2))
        )
    y))

;; Good: standard CL style — parens at end of expression
(defun add (x y)
  (+ x y))

;; Good: multiple parens at end of line
(defun check (a b c)
  (when (and a b c)
    (list a b c)))
