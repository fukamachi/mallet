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

(defun test-complexity (code expected-complexity &optional (max nil))
  "Test that CODE has EXPECTED-COMPLEXITY.
   If MAX is not provided, use (1- expected-complexity) to trigger violation."
  (let* ((rule (make-instance 'rules:cyclomatic-complexity-rule
                              :max (or max (1- expected-complexity))))
         (forms (parser:parse-forms code #P"test.lisp"))
         (form (first forms))
         (violations (base:check-form rule form #P"test.lisp")))
    (ok (= 1 (length violations)))
    (when (first violations)
      (ok (search (format nil "complexity of ~D" expected-complexity)
                  (violation:violation-message (first violations)))))))

(defun test-no-complexity-violation (code max)
  "Test that CODE does not violate with given MAX."
  (let* ((rule (make-instance 'rules:cyclomatic-complexity-rule
                              :max max))
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
  (testing "COND with 3 clauses (2 conditionals + 1 else) adds 2"
    (test-complexity
     "(defun foo (x)
        (cond
          ((< x 0) -1)
          ((> x 0) 1)
          (t 0)))" 3)))

(deftest complexity-cond-no-else
  (testing "COND with 3 clauses (all conditionals) adds 3"
    (test-complexity
     "(defun foo (x)
        (cond
          ((< x 0) -1)
          ((= x 0) 0)
          ((> x 0) 1)))" 4)))

(deftest complexity-cond-otherwise
  (testing "COND with otherwise instead of t also excludes it"
    (test-complexity
     "(defun foo (x)
        (cond
          ((< x 0) -1)
          ((> x 0) 1)
          (otherwise 0)))" 3)))

(deftest complexity-case
  (testing "CASE with 3 cases + otherwise adds 3"
    (test-complexity
     "(defun foo (x)
        (case x
          (a 1)
          (b 2)
          (c 3)
          (otherwise 4)))" 4)))

(deftest complexity-case-no-otherwise
  (testing "CASE with 4 cases (no otherwise) adds 4"
    (test-complexity
     "(defun foo (x)
        (case x
          (a 1)
          (b 2)
          (c 3)
          (d 4)))" 5)))

(deftest complexity-typecase
  (testing "TYPECASE with 2 types + otherwise adds 2"
    (test-complexity
     "(defun foo (x)
        (typecase x
          (integer 1)
          (string 2)
          (otherwise 3)))" 3)))

(deftest complexity-ecase
  (testing "ECASE with 3 cases adds 3 (no otherwise needed)"
    (test-complexity
     "(defun foo (x)
        (ecase x
          (a 1)
          (b 2)
          (c 3)))" 4)))

(deftest complexity-etypecase
  (testing "ETYPECASE with 2 types adds 2 (no otherwise needed)"
    (test-complexity
     "(defun foo (x)
        (etypecase x
          (integer 1)
          (string 2)))" 3)))

;; Phase 3B: Loops

(deftest complexity-dotimes
  (testing "DOTIMES adds 0 (simple iteration)"
    (test-no-complexity-violation
     "(defun foo (n)
        (dotimes (i n)
          (print i)))" 10)))

(deftest complexity-dolist
  (testing "DOLIST adds 0 (simple iteration)"
    (test-no-complexity-violation
     "(defun foo (list)
        (dolist (x list)
          (print x)))" 10)))

(deftest complexity-do
  (testing "DO adds 1 (has end-test condition)"
    (test-complexity
     "(defun foo (n)
        (do ((i 0 (1+ i)))
            ((>= i n))
          (print i)))" 2)))

(deftest complexity-do-star
  (testing "DO* adds 1 (has end-test condition)"
    (test-complexity
     "(defun foo (n)
        (do* ((i 0 (1+ i)))
             ((>= i n))
          (print i)))" 2)))

(deftest complexity-loop-simple
  (testing "Simple LOOP adds 0 (no conditionals)"
    (test-no-complexity-violation
     "(defun foo (list)
        (loop for x in list collect x))" 10)))

(deftest complexity-loop-when
  (testing "LOOP with WHEN adds 1"
    (test-complexity
     "(defun foo (list)
        (loop for x in list
              when (evenp x)
                collect x))" 2)))

(deftest complexity-loop-multiple-conditionals
  (testing "LOOP with multiple conditionals"
    (test-complexity
     "(defun foo (list)
        (loop for x in list
              when (evenp x)
                collect x
              unless (zerop x)
                sum x))" 3)))

;; Phase 3C: Advanced forms

(deftest complexity-and
  (testing "AND adds 1 (regardless of argument count)"
    (test-complexity
     "(defun foo (a b)
        (and a b))" 2)))

(deftest complexity-and-many-args
  (testing "AND with 4 arguments still adds 1"
    (test-complexity
     "(defun validate (a b c d)
        (and a b c d))" 2)))

(deftest complexity-or
  (testing "OR adds 1 (regardless of argument count)"
    (test-complexity
     "(defun foo (a b c)
        (or a b c))" 2)))

(deftest complexity-nested-and-or
  (testing "Nested AND/OR each adds 1"
    (test-complexity
     "(defun foo (x y z)
        (and x (or y z)))" 3)))

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
  (testing "Violation example (9 conditions + 1 else = 9 added, total 10)"
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
          (t (error \"Unknown command\"))))" 10)))

;; Modified variant tests

(deftest complexity-case-modified-variant
  (testing "CASE with modified variant counts as +1 total"
    (let* ((rule (make-instance 'rules:cyclomatic-complexity-rule
                                :max 1
                                :variant :modified))
           (code "(defun foo (x)
                    (case x
                      (a 1)
                      (b 2)
                      (c 3)
                      (otherwise 4)))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (form (first forms))
           (violations (base:check-form rule form #P"test.lisp")))
      ;; Complexity = 1 (base) + 1 (case) = 2 with modified variant
      (ok (= 1 (length violations)))
      (ok (search "complexity of 2" (violation:violation-message (first violations)))))))

(deftest complexity-case-standard-variant
  (testing "CASE with standard variant counts per clause"
    (let* ((rule (make-instance 'rules:cyclomatic-complexity-rule
                                :max 1
                                :variant :standard))
           (code "(defun foo (x)
                    (case x
                      (a 1)
                      (b 2)
                      (c 3)
                      (otherwise 4)))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (form (first forms))
           (violations (base:check-form rule form #P"test.lisp")))
      ;; Complexity = 1 (base) + 3 (case clauses) = 4 with standard variant
      (ok (= 1 (length violations)))
      (ok (search "complexity of 4" (violation:violation-message (first violations)))))))

(deftest complexity-typecase-modified-variant
  (testing "TYPECASE with modified variant counts as +1 total"
    (let* ((rule (make-instance 'rules:cyclomatic-complexity-rule
                                :max 1
                                :variant :modified))
           (code "(defun foo (x)
                    (typecase x
                      (integer 1)
                      (string 2)
                      (otherwise 3)))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (form (first forms))
           (violations (base:check-form rule form #P"test.lisp")))
      ;; Complexity = 1 (base) + 1 (typecase) = 2 with modified variant
      (ok (= 1 (length violations)))
      (ok (search "complexity of 2" (violation:violation-message (first violations)))))))
