;;; Valid progn usage — no redundant-progn violations

;;; Valid: progn with multiple body forms
(progn
  (do-something)
  (do-another))

;;; Valid: empty progn
(progn)

;;; Valid: progn with three forms
(defun test-function ()
  (progn
    (step-one)
    (step-two)
    (step-three)))

;;; Valid: non-progn forms
(when condition
  (do-it))

(let ((x 1))
  (use x))

;;; Valid: progn symbol as case clause key (not a progn form)
(defun classify-head (x)
  (case x
    (progn :progn-keyword)
    (otherwise :other)))

;;; Valid: progn symbol in handler-case condition-type position
(defun run-safely ()
  (handler-case (do-it)
    (progn (e) (log e))))
