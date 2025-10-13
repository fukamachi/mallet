(defpackage #:mallet/tests/rules/if-without-else
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/if-without-else)

(deftest if-without-else-valid
  (testing "Valid: if with else clause"
    (let* ((code "(if condition
                     (do-something)
                     (do-else))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:if-without-else-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: when instead of if without else"
    (let* ((code "(when condition
                     (do-something))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:if-without-else-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: unless instead of if without else"
    (let* ((code "(unless condition
                     (do-something))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:if-without-else-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)))))

(deftest if-without-else-invalid
  (testing "Invalid: if without else clause"
    (let* ((code "(if condition
                     (do-something))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:if-without-else-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :if-without-else))
      (ok (string= (violation:violation-message (first violations))
                   "Use 'when' or 'unless' instead of 'if' without else clause"))))

  (testing "Invalid: nested if without else"
    (let* ((code "(defun foo (x)
                     (if (> x 0)
                         (print x)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:if-without-else-rule))
           ;; Check the defun form - rule should recurse into nested forms
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :if-without-else)))))
