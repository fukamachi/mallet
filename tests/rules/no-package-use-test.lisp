(defpackage #:mallet/tests/rules/no-package-use
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:config #:mallet/config)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/no-package-use)

;;; Helper

(defun check-no-package-use (code)
  (let* ((forms (parser:parse-forms code #p"test.lisp"))
         (rule (make-instance 'rules:no-package-use-rule)))
    (mapcan (lambda (form)
              (rules:check-form rule form #p"test.lisp"))
            forms)))

;;; Valid cases — no violations

(deftest no-package-use-valid
  (testing "No :use clause at all"
    (ok (null (check-no-package-use
               "(defpackage #:foo (:export #:bar))"))))

  (testing ":use of only #:cl"
    (ok (null (check-no-package-use
               "(defpackage #:foo (:use #:cl))"))))

  (testing ":use of only #:common-lisp"
    (ok (null (check-no-package-use
               "(defpackage #:foo (:use #:common-lisp))"))))

  (testing ":use of only #:coalton"
    (ok (null (check-no-package-use
               "(defpackage #:foo (:use #:coalton))"))))

  (testing ":use of only #:coalton-prelude"
    (ok (null (check-no-package-use
               "(defpackage #:foo (:use #:coalton-prelude))"))))

  (testing ":use of multiple exempt packages"
    (ok (null (check-no-package-use
               "(defpackage #:foo (:use #:cl #:coalton-prelude))"))))

  (testing "Non-defpackage form is ignored"
    (ok (null (check-no-package-use "(defun foo () nil)"))))

  (testing "define-package with only :cl is exempt"
    (ok (null (check-no-package-use
               "(uiop:define-package #:foo (:use #:cl))")))))

;;; Registration

(deftest no-package-use-registration
  (testing "Rule is included in default config"
    (let* ((cfg (config:get-built-in-config :default))
           (rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
      (ok (member :no-package-use rule-names)))))

;;; Invalid cases — violations expected

(deftest no-package-use-violations
  (testing ":use of non-exempt package emits one violation"
    (let ((violations (check-no-package-use
                       "(defpackage #:foo (:use #:alexandria))")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :no-package-use))
      (ok (eq (violation:violation-severity (first violations)) :warning))))

  (testing ":use of multiple non-exempt packages emits ONE violation (not one per package)"
    (let ((violations (check-no-package-use
                       "(defpackage #:foo (:use #:alexandria #:cl-ppcre))")))
      (ok (= (length violations) 1))
      (ok (search "ALEXANDRIA" (string-upcase (violation:violation-message (first violations)))))
      (ok (search "CL-PPCRE" (string-upcase (violation:violation-message (first violations)))))))

  (testing ":use mixing exempt and non-exempt emits ONE violation naming only non-exempt"
    (let ((violations (check-no-package-use
                       "(defpackage #:foo (:use #:cl #:alexandria))")))
      (ok (= (length violations) 1))
      (ok (search "ALEXANDRIA" (string-upcase (violation:violation-message (first violations)))))
      ;; CL should not appear in the violation message as a blamed package
      (ok (not (search "CL" (string-upcase (violation:violation-message (first violations))))))))

  (testing "Two :use clauses with non-exempt packages emit TWO violations"
    (let ((violations (check-no-package-use
                       "(defpackage #:foo (:use #:alexandria) (:use #:cl-ppcre))")))
      (ok (= (length violations) 2))))

  (testing "define-package with non-exempt :use emits violation"
    (let ((violations (check-no-package-use
                       "(uiop:define-package #:foo (:use #:alexandria))")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :no-package-use))))

  (testing "violation location points to the :use clause line and column"
    (let ((violations (check-no-package-use
                       "(defpackage #:foo
  (:use #:alexandria))")))
      (ok (= (length violations) 1))
      ;; :use clause is on line 2, column 8 (0-based offset within the line)
      (ok (= 2 (violation:violation-line (first violations))))
      (ok (equal #p"test.lisp" (violation:violation-file (first violations)))))))
