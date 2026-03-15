(defpackage #:mallet/tests/rules/no-eval
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/no-eval)

;;; Helper

(defun check-eval (code)
  (let ((forms (parser:parse-forms code #p"test.lisp"))
        (rule (make-instance 'rules:no-eval-rule)))
    (mapcan (lambda (form)
              (rules:check-form rule form #p"test.lisp"))
            forms)))

;;; Valid cases (no violations expected)

(deftest eval-usage-valid
  (testing "No eval: plain function call"
    (ok (null (check-eval "(defun foo (x) (+ x 1))"))))

  (testing "No eval: quoted list containing eval symbol (data, not code)"
    (ok (null (check-eval "'(eval something)"))))

  (testing "No eval: string containing eval"
    (ok (null (check-eval "(defun doc () \"Use eval to execute.\")"))))

  (testing "No eval: symbol named eval-something (partial match should not trigger)"
    (ok (null (check-eval "(defun test () (eval-when (:compile-toplevel) 42))"))))

  (testing "No eval: funcall with unrelated function"
    (ok (null (check-eval "(funcall #'print \"hello\")"))))

  (testing "No eval: apply with unrelated function"
    (ok (null (check-eval "(apply #'+ '(1 2 3))")))))

;;; Invalid cases (violations expected)

(deftest eval-usage-direct
  (testing "Direct (eval ...) call"
    (let ((violations (check-eval "(eval '(print 1))")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :no-eval))
      (ok (string= (violation:violation-message (first violations))
                   "Avoid using cl:eval at runtime for safety"))))

  (testing "Direct eval nested inside defun"
    (let ((violations (check-eval "(defun dangerous (expr) (eval expr))")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :no-eval))))

  (testing "Multiple direct eval calls"
    (let ((violations (check-eval "(progn (eval '(foo)) (eval '(bar)))")))
      (ok (= (length violations) 2)))))

(deftest eval-usage-funcall
  (testing "(funcall #'eval expr)"
    (let ((violations (check-eval "(funcall #'eval '(print 1))")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :no-eval))
      (ok (search "funcall" (violation:violation-message (first violations))))))

  (testing "(funcall 'eval expr)"
    (let ((violations (check-eval "(funcall 'eval '(print 1))")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :no-eval))))

  (testing "(funcall eval expr) -- bare symbol reference"
    ;; eval-symbol-p handles this via the first `or' clause (base:symbol-matches-p),
    ;; matching the string symbol produced by the Eclector parse-result client.
    (let ((violations (check-eval "(funcall eval '(print 1))")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :no-eval))
      (ok (search "funcall" (violation:violation-message (first violations)))))))

(deftest eval-usage-apply
  (testing "(apply #'eval args)"
    (let ((violations (check-eval "(apply #'eval '((print 1)))")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :no-eval))
      (ok (search "apply" (violation:violation-message (first violations))))))

  (testing "(apply 'eval args)"
    (let ((violations (check-eval "(apply 'eval '((print 1)))")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :no-eval)))))
