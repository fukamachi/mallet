;;; Test cases for redundant-progn rule

;;; Violations: progn with a single body form
(progn (do-something))

(progn 42)

(progn "a string")

(defun test-function ()
  (progn (call-something)))

(let ((x 1))
  (progn (use-x x)))

(when *condition*
  (progn (handle-it)))
