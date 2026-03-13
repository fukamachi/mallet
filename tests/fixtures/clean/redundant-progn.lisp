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
