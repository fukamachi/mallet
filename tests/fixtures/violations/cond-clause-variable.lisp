;;; Clause-based macro fixtures for unused-variables

;; Variables used in cond test position should count as references
(defun test-cond-with-cl-function-names ()
  (let ((count 5))
    (cond (count 'yes)
          (t 'no))))

;; Variables in case keyform are expressions
(defun test-case-keyform-used ()
  (let ((x 1))
    (case x
      (1 'one)
      (otherwise 'other))))

;; Case keys are literals, not variable references
(defun test-case-key-not-reference ()
  (let ((x 1))
    (case 'foo
      (x 'matched-x)
      (otherwise 'other))))
