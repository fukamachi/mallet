(defpackage #:mallet/tests/rules/metrics
  (:use #:cl #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules/forms/metrics)
   (#:base #:mallet/rules/base)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/metrics)

;;; Test infrastructure helpers

(defun test-no-violation (rule code)
  "Test that CODE does not violate RULE."
  (let* ((forms (parser:parse-forms code #P"test.lisp"))
         (form (first forms))
         (violations (base:check-form rule form #P"test.lisp")))
    (ok (null violations))))

(defun test-violation (rule code expected-message-fragment)
  "Test that CODE violates RULE with message containing EXPECTED-MESSAGE-FRAGMENT."
  (let* ((forms (parser:parse-forms code #P"test.lisp"))
         (form (first forms))
         (violations (base:check-form rule form #P"test.lisp")))
    (ok (= 1 (length violations)))
    (when (first violations)
      (ok (search expected-message-fragment
                  (violation:violation-message (first violations)))))))

;;; Function-length tests

(deftest function-length-one-line
  (testing "One-line function should have length 1"
    (let* ((rule (make-instance 'rules:function-length-rule :max-lines 0))
           (code "(defun foo (x) (+ x 1))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (form (first forms))
           (violations (base:check-form rule form #P"test.lisp")))
      (ok (= 1 (length violations)))
      (ok (search "1 lines" (violation:violation-message (first violations)))))))

(deftest function-length-empty
  (testing "Empty function should have length 1"
    (let* ((rule (make-instance 'rules:function-length-rule :max-lines 0))
           (code "(defun empty ())")
           (forms (parser:parse-forms code #P"test.lisp"))
           (form (first forms))
           (violations (base:check-form rule form #P"test.lisp")))
      (ok (= 1 (length violations)))
      (ok (search "1 lines" (violation:violation-message (first violations)))))))

(deftest function-length-short-defun
  (testing "Short function (10 lines) should not violate"
    (let* ((rule (make-instance 'rules:function-length-rule))
           (code "(defun foo (x)
  \"Docstring.\"
  (let ((y (* x 2)))
    (when (> y 0)
      (print y)
      (+ y 1))))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (form (first forms))
           (violations (base:check-form rule form #P"test.lisp")))
      (ok (null violations)))))

(deftest function-length-long-defun
  (testing "Long function (60 lines) should violate"
    (let* ((rule (make-instance 'rules:function-length-rule :max-lines 50))
           ;; Generate a function with 60 lines
           (code (format nil "(defun long-function (x)~%~{  (print ~D)~%~})"
                         (loop for i from 1 to 59 collect i)))
           (forms (parser:parse-forms code #P"test.lisp"))
           (form (first forms))
           (violations (base:check-form rule form #P"test.lisp")))
      (ok (= 1 (length violations)))
      (ok (search "60 lines" (violation:violation-message (first violations)))))))

(deftest function-length-at-limit
  (testing "Function exactly at limit (50 lines) should not violate"
    (let* ((rule (make-instance 'rules:function-length-rule :max-lines 50))
           ;; Generate a function with exactly 50 lines
           (code (format nil "(defun at-limit (x)~%~{  (print ~D)~%~})"
                         (loop for i from 1 to 49 collect i)))
           (forms (parser:parse-forms code #P"test.lisp"))
           (form (first forms))
           (violations (base:check-form rule form #P"test.lisp")))
      (ok (null violations)))))

(deftest function-length-defmethod
  (testing "defmethod is counted"
    (let* ((rule (make-instance 'rules:function-length-rule :max-lines 5))
           (code "(defmethod process ((x integer))
  (print x)
  (print x)
  (print x)
  (print x)
  (print x))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (form (first forms))
           (violations (base:check-form rule form #P"test.lisp")))
      (ok (= 1 (length violations)))
      (ok (search "process" (violation:violation-message (first violations)))))))

(deftest function-length-nested-flet
  (testing "flet inner functions are counted separately"
    (let* ((rule (make-instance 'rules:function-length-rule :max-lines 5))
           ;; Outer function: 6 lines (VIOLATE)
           ;; Inner function: 4 lines (OK)
           (code "(defun outer (x)
  (flet ((inner (y)
           (print y)
           (print y)
           (print y)))
    (inner x)))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (form (first forms))
           (violations (base:check-form rule form #P"test.lisp")))
      ;; Should have 1 violation for outer function (6 lines > 5 max)
      (ok (= 1 (length violations)))
      (ok (search "outer" (violation:violation-message (first violations))))
      (ok (search "6 lines" (violation:violation-message (first violations)))))))

(deftest function-length-custom-max
  (testing "Custom max-lines configuration"
    (let* ((rule (make-instance 'rules:function-length-rule :max-lines 75))
           (code (format nil "(defun custom-max (x)~%~{  (print ~D)~%~})"
                         (loop for i from 1 to 70 collect i)))
           (forms (parser:parse-forms code #P"test.lisp"))
           (form (first forms))
           (violations (base:check-form rule form #P"test.lisp")))
      ;; 71 lines, max 75 - should not violate
      (ok (null violations)))))

;;; Cyclomatic-complexity tests

(defun test-complexity (code expected-complexity &optional (max-complexity nil))
  "Test that CODE has EXPECTED-COMPLEXITY.
   If MAX-COMPLEXITY is not provided, use (1- expected-complexity) to trigger violation."
  (let* ((rule (make-instance 'rules:cyclomatic-complexity-rule
                              :max-complexity (or max-complexity (1- expected-complexity))))
         (forms (parser:parse-forms code #P"test.lisp"))
         (form (first forms))
         (violations (base:check-form rule form #P"test.lisp")))
    (ok (= 1 (length violations)))
    (when (first violations)
      (ok (search (format nil "complexity of ~D" expected-complexity)
                  (violation:violation-message (first violations)))))))

(defun test-no-complexity-violation (code max-complexity)
  "Test that CODE does not violate with given MAX-COMPLEXITY."
  (let* ((rule (make-instance 'rules:cyclomatic-complexity-rule
                              :max-complexity max-complexity))
         (forms (parser:parse-forms code #P"test.lisp"))
         (form (first forms))
         (violations (base:check-form rule form #P"test.lisp")))
    (ok (null violations))))

(deftest complexity-base
  (testing "Empty function has complexity 1"
    (test-complexity "(defun empty () nil)" 1)))

(deftest complexity-if
  (testing "IF adds 1 to complexity"
    (test-complexity "(defun foo (x) (if (> x 0) 1 2))" 2)))

(deftest complexity-when
  (testing "WHEN adds 1 to complexity"
    (test-complexity "(defun foo (x) (when (> x 0) 1))" 2)))

(deftest complexity-unless
  (testing "UNLESS adds 1 to complexity"
    (test-complexity "(defun foo (x) (unless (> x 0) 1))" 2)))

(deftest complexity-multiple-ifs
  (testing "Multiple IFs add to complexity"
    (test-complexity
     "(defun foo (x y)
        (if (> x 0) 1 2)
        (if (> y 0) 3 4))" 3)))

(deftest complexity-cond
  (testing "COND with 3 clauses adds 3"
    (test-complexity
     "(defun foo (x)
        (cond
          ((< x 0) -1)
          ((> x 0) 1)
          (t 0)))" 4)))

(deftest complexity-case
  (testing "CASE with 4 clauses adds 1 (modified variant)"
    (test-complexity
     "(defun foo (x)
        (case x
          (a 1)
          (b 2)
          (c 3)
          (otherwise 4)))" 2)))

(deftest complexity-typecase
  (testing "TYPECASE adds 1 (modified variant)"
    (test-complexity
     "(defun foo (x)
        (typecase x
          (integer 1)
          (string 2)
          (otherwise 3)))" 2)))

;; Phase 3B: Loops

(deftest complexity-dotimes
  (testing "DOTIMES adds 1"
    (test-complexity
     "(defun foo (n)
        (dotimes (i n)
          (print i)))" 2)))

(deftest complexity-dolist
  (testing "DOLIST adds 1"
    (test-complexity
     "(defun foo (list)
        (dolist (x list)
          (print x)))" 2)))

(deftest complexity-loop-simple
  (testing "Simple LOOP adds 1"
    (test-complexity
     "(defun foo (list)
        (loop for x in list collect x))" 2)))

(deftest complexity-loop-when
  (testing "LOOP with WHEN adds 2"
    (test-complexity
     "(defun foo (list)
        (loop for x in list
              when (evenp x)
                collect x))" 3)))

;; Phase 3C: Advanced forms

(deftest complexity-and-two-args
  (testing "AND with 2 arguments adds 1"
    (test-complexity
     "(defun foo (a b)
        (and a b))" 2)))

(deftest complexity-and-four-args
  (testing "AND with 4 arguments adds 3"
    (test-complexity
     "(defun validate (a b c d)
        (and a b c d))" 4)))

(deftest complexity-or-three-args
  (testing "OR with 3 arguments adds 2"
    (test-complexity
     "(defun foo (a b c)
        (or a b c))" 3)))

(deftest complexity-ignore-errors
  (testing "IGNORE-ERRORS adds 1"
    (test-complexity
     "(defun foo ()
        (ignore-errors
          (risky-operation)))" 2)))

(deftest complexity-handler-case-two
  (testing "HANDLER-CASE with 2 handlers adds 2"
    (test-complexity
     "(defun foo ()
        (handler-case
            (risky-operation)
          (error (e) (log-error e))
          (warning (w) (log-warning w))))" 3)))

;; Complex example

(deftest complexity-violation-example
  (testing "Violation example from plan (complexity 11 > 10)"
    (test-complexity
     "(defun handle-command (cmd args)
        (cond
          ((string= cmd \"start\") (start-server))
          ((string= cmd \"stop\") (stop-server))
          ((string= cmd \"restart\") (restart-server))
          ((string= cmd \"status\") (show-status))
          ((string= cmd \"config\") (show-config))
          ((string= cmd \"init\") (initialize))
          ((string= cmd \"destroy\") (destroy))
          ((string= cmd \"pause\") (pause-server))
          ((string= cmd \"resume\") (resume-server))
          (t (error \"Unknown command\"))))" 11)))
