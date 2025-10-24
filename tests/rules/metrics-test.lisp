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
