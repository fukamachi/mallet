(defpackage #:mallet/tests/rules/closing-paren-on-own-line
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/closing-paren-on-own-line)

;;; Helper

(defun check-closing-paren (text)
  (let ((rule (make-instance 'rules:closing-paren-on-own-line-rule)))
    (rules:check-text rule text #p"test.lisp")))

;;; Valid cases — no violations

(deftest closing-paren-valid
  (testing "Standard CL style — parens at end of form"
    (ok (null (check-closing-paren
               "(defun foo ()
  (+ 1 2))"))))

  (testing "Multiple closing parens at end of line is fine"
    (ok (null (check-closing-paren
               "(when t
  (if x
    y
    z))"))))

  (testing "Empty file"
    (ok (null (check-closing-paren ""))))

  (testing "Single line form"
    (ok (null (check-closing-paren "(defun foo () nil)"))))

  (testing "Line with only opening paren — not flagged"
    (ok (null (check-closing-paren "(defun foo ()\n  (bar))"))))

  (testing "Closing paren after code on same line"
    (ok (null (check-closing-paren
               "(defun foo ()
  (bar x)
  (baz y))"))))

  (testing "Comment line is not flagged"
    (ok (null (check-closing-paren
               "(defun foo ()
  ; some comment
  nil)"))))

  (testing "String containing closing paren — not flagged"
    (ok (null (check-closing-paren
               "(defun foo () \"has ) paren\")"))))

  (testing "Multi-line string with ) on its own line — not flagged"
    (ok (null (check-closing-paren
               (format nil "(defun foo ()~%  (let ((x \"hello~%  )~%  world\"))~%    x))")))))

  (testing "Multi-line string: after string closes, real ) on own line is still flagged"
    (let ((violations (check-closing-paren
                       (format nil "(defun foo ()~%  (bar \"x~%  )\")~%  )"))))
      ;; Line 3 is inside the string (not flagged), line 4 is outside (flagged)
      (ok (= 1 (length violations)))
      (ok (= 4 (violation:violation-line (first violations))))))

  (testing "Line with paren and non-whitespace before it"
    (ok (null (check-closing-paren
               "(let ((x 1)
      (y 2))
  (+ x y))"))))

  (testing "Whitespace-only line is not flagged"
    (ok (null (check-closing-paren "(defun foo ()\n  \n  nil)"))))

  (testing "Closing paren after comment line — not flagged"
    (ok (null (check-closing-paren
               "(defun foo ()
  (bar) ; trailing comment
  )"))))

  (testing "Closing paren after line ending with full-line comment — not flagged"
    (ok (null (check-closing-paren
               "(defvar *alist*
  '((a . 1)
    (b . 2) ; last entry
    ))"))))

  (testing "Closing paren after non-comment line — still flagged"
    (let ((violations (check-closing-paren
                       "(defun foo ()
  (bar)
  )")))
      (ok (= 1 (length violations)))))

  (testing "Semicolon inside string is not treated as comment"
    (let ((violations (check-closing-paren
                       "(defun foo ()
  (bar \"has ; semicolon\")
  )")))
      (ok (= 1 (length violations))))))

;;; Invalid cases — violations expected

(deftest closing-paren-violations
  (testing "Single closing paren on its own line"
    (let ((violations (check-closing-paren
                       "(defun foo ()
  (bar)
  )")))
      (ok (= 1 (length violations)))
      (ok (eq :closing-paren-on-own-line
              (violation:violation-rule (first violations))))
      (ok (= 3 (violation:violation-line (first violations))))))

  (testing "Multiple closing parens on their own line"
    (let ((violations (check-closing-paren
                       "(when t
  (if x
    y
    z
    )
  )")))
      (ok (= 2 (length violations)))))

  (testing "Indented single closing paren"
    (let ((violations (check-closing-paren
                       "(defun foo ()
  (let ((x 1)
        (y 2)
        )
    (+ x y)))")))
      (ok (= 1 (length violations)))
      (ok (= 4 (violation:violation-line (first violations))))))

  (testing "Multiple closing parens alone on line"
    (let ((violations (check-closing-paren
                       "(defun foo ()
  (bar
    ))")))
      (ok (= 1 (length violations)))))

  (testing "Severity is :warning"
    (let ((violations (check-closing-paren
                       (format nil "(defun foo ()~%  )"))))
      (ok (= 1 (length violations)))
      (ok (eq :warning (violation:violation-severity (first violations))))))

  (testing "Violation message mentions closing paren"
    (let* ((violations (check-closing-paren
                        (format nil "(defun foo ()~%  )")))
           (msg (violation:violation-message (first violations))))
      (ok (search "closing" (string-downcase msg)))))

  (testing "Column points to start of paren content"
    (let ((violations (check-closing-paren
                       "(defun foo ()
  ))")))
      (ok (= 1 (length violations)))
      (ok (= 2 (violation:violation-column (first violations)))))))

;;; Registration tests

(deftest closing-paren-registration
  (testing ":closing-paren-on-own-line is in default config"
    (let* ((cfg (mallet/config:get-built-in-config :default))
           (rule-names (mapcar #'rules:rule-name (mallet/config:config-rules cfg))))
      (ok (member :closing-paren-on-own-line rule-names))))

  (testing ":closing-paren-on-own-line is in :all config"
    (let* ((cfg (mallet/config:get-built-in-config :all))
           (rule-names (mapcar #'rules:rule-name (mallet/config:config-rules cfg))))
      (ok (member :closing-paren-on-own-line rule-names))))

  (testing ":closing-paren-on-own-line rule has :format category"
    (let ((rule (rules:make-rule :closing-paren-on-own-line)))
      (ok (eq :format (rules:rule-category rule))))))
