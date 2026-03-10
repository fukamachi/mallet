;;; Test file for intern-usage rule

;; Bad: direct cl:intern
(defun make-symbol-dynamic (name)
  (cl:intern name))

;; Bad: alexandria:symbolicate
(defun build-accessor (prefix slot)
  (alexandria:symbolicate prefix '- slot))

;; Bad: funcall with cl:intern
(defun intern-it (name pkg)
  (funcall #'cl:intern name pkg))

;; Good: no interning
(defun add (x y)
  (+ x y))
