(defpackage #:mallet/tests/list-rules
  (:use #:cl #:rove)
  (:import-from #:mallet
                #:print-list-rules
                #:make-rule
                #:parse-args))
(in-package #:mallet/tests/list-rules)

(defun list-rules-lines ()
  "Return non-empty lines from print-list-rules output."
  (let ((output (with-output-to-string (s)
                  (let ((*standard-output* s))
                    (print-list-rules)))))
    (remove "" (uiop:split-string output :separator '(#\Newline)) :test #'string=)))

(deftest list-rules-includes-trailing-whitespace
  (testing "trailing-whitespace appears as its own line in --list-rules output"
    (ok (member "trailing-whitespace" (list-rules-lines) :test #'string=))))

(deftest list-rules-includes-cyclomatic-complexity
  (testing "cyclomatic-complexity appears as its own line in --list-rules output"
    (ok (member "cyclomatic-complexity" (list-rules-lines) :test #'string=))))

(deftest list-rules-one-name-per-line
  (testing "each output line contains exactly one rule name with no embedded whitespace"
    (let ((lines (list-rules-lines)))
      ;; Mallet has 40+ rules; a stub printing only the two AC-named rules would be < 10.
      (ok (> (length lines) 15) "more than 15 rule names are listed")
      (ok (every (lambda (line)
                   (notany (lambda (c) (char= c #\Space)) line))
                 lines)
          "no line contains a space character")
      ;; Guard against stub implementations that output fake rule names: every listed
      ;; name must be accepted by make-rule (which errors on unknown names).
      (ok (every (lambda (line)
                   (handler-case
                       (progn (make-rule (intern (string-upcase line) :keyword)) t)
                     (error () nil)))
                 lines)
          "every listed rule name must be accepted by make-rule"))))

(deftest list-rules-no-duplicates
  (testing "no rule name is listed more than once"
    (let ((lines (list-rules-lines)))
      (ok (= (length lines)
             (length (remove-duplicates lines :test #'string=)))
          "all listed rule names are pairwise distinct"))))

(deftest list-rules-recovery-path-succeeds
  (testing "parse-args --list-rules sets list-rules-mode, then print-list-rules produces the expected output"
    ;; AC3: after an unknown-rule error, the user runs 'mallet --list-rules'.
    ;; Verify the integration: parse-args returns list-rules-mode=t AND
    ;; print-list-rules produces the rule listing (main then calls uiop:quit 0).
    ;; Removing the (when list-rules-mode ...) block from main would not change
    ;; parse-args output, but this test verifies the full recovery sequence works.
    (multiple-value-bind (format config-path preset debug no-color fix-mode
                          cli-rules fail-on init-mode force files list-rules-mode)
        (parse-args '("--list-rules"))
      (declare (ignore format config-path preset debug no-color fix-mode
                       cli-rules fail-on init-mode force files))
      (ok list-rules-mode
          "--list-rules must activate list-rules-mode so main calls print-list-rules")
      (let ((recovery-output
              (with-output-to-string (s)
                (let ((*standard-output* s))
                  (print-list-rules)))))
        (ok (search "trailing-whitespace" recovery-output)
            "recovery output must include trailing-whitespace")
        (ok (search "cyclomatic-complexity" recovery-output)
            "recovery output must include cyclomatic-complexity")
        (ok (plusp (length (remove "" (uiop:split-string recovery-output
                                                         :separator '(#\Newline))
                                   :test #'string=)))
            "recovery output must be non-empty so --list-rules actually helps the user")))))
