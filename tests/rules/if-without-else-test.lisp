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
      (ok (null violations))))

  (testing "Valid: quoted list with string \"IF\" should not trigger"
    (let* ((code "(defun conditional-p (head)
                     (member head '(\"IF\" \"WHEN\" \"UNLESS\") :test #'string-equal))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:if-without-else-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: quoted list with symbols should not trigger"
    (let* ((code "(defun form-types ()
                     '(IF WHEN UNLESS))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:if-without-else-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: string literal in code should not trigger"
    (let* ((code "(defun test ()
                     (list \"IF\" \"WHEN\"))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:if-without-else-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: macro argument with string literals should not trigger"
    (let* ((code "(defmacro with-quote (form)
                     `(list ,@form))
                   (defun test ()
                     (with-quote (\"IF\" \"WHEN\" \"UNLESS\")))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:if-without-else-rule)))
      ;; Check both the defmacro and defun forms
      (dolist (form forms)
        (let ((violations (rules:check-form rule form #p"test.lisp")))
          (ok (null violations)))))))

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
