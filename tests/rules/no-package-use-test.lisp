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
  (let ((forms (parser:parse-forms code #p"test.lisp"))
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

;;; Test framework exemption

(deftest no-package-use-test-framework-exempt
  (testing ":use of rove is exempt (known test framework)"
    (ok (null (check-no-package-use
               "(defpackage #:foo (:use #:cl #:rove))"))))

  (testing ":use of fiveam is exempt"
    (ok (null (check-no-package-use
               "(defpackage #:foo (:use #:fiveam))"))))

  (testing ":use of parachute is exempt"
    (ok (null (check-no-package-use
               "(defpackage #:foo (:use #:parachute))"))))

  (testing "non-framework :use alongside rove still emits violation for the non-framework"
    (let ((violations (check-no-package-use
                       "(defpackage #:foo (:use #:cl #:rove #:some-random-pkg))")))
      (ok (= (length violations) 1))
      (ok (search "SOME-RANDOM-PKG" (string-upcase (violation:violation-message (first violations)))))
      ;; rove should NOT appear in the violation message
      (ok (not (search "ROVE" (string-upcase (violation:violation-message (first violations))))))))

  (testing "lack/test is NOT exempt (not a known test framework)"
    (let ((violations (check-no-package-use
                       "(defpackage #:foo (:use #:lack/test))")))
      (ok (= (length violations) 1)))))

;;; :allow option

(defun check-no-package-use-with-allow (code &rest allow)
  (let ((forms (parser:parse-forms code #p"test.lisp"))
        (rule (make-instance 'rules:no-package-use-rule :allow allow)))
    (mapcan (lambda (form)
              (rules:check-form rule form #p"test.lisp"))
            forms)))

(deftest no-package-use-allow-option
  (testing ":allow (\"cffi\") exempts cffi"
    (ok (null (check-no-package-use-with-allow
               "(defpackage #:foo (:use #:cffi))" "cffi"))))

  (testing ":allow is case-insensitive"
    (ok (null (check-no-package-use-with-allow
               "(defpackage #:foo (:use #:CFFI))" "cffi")))
    (ok (null (check-no-package-use-with-allow
               "(defpackage #:foo (:use #:cffi))" "CFFI"))))

  (testing ":allow only exempts the listed package, not others"
    (let ((violations (check-no-package-use-with-allow
                       "(defpackage #:foo (:use #:cffi #:other-pkg))" "cffi")))
      (ok (= (length violations) 1))
      (ok (search "OTHER-PKG" (string-upcase (violation:violation-message (first violations)))))))

  (testing ":allow with no extra packages still applies test-framework exemption"
    (ok (null (check-no-package-use-with-allow
               "(defpackage #:foo (:use #:rove))"))))

  (testing ":allow accepts keyword symbols"
    (ok (null (let ((forms (parser:parse-forms "(defpackage #:foo (:use #:cffi))" #p"test.lisp"))
                    (rule (make-instance 'rules:no-package-use-rule :allow '(:cffi))))
               (mapcan (lambda (form)
                         (rules:check-form rule form #p"test.lisp"))
                       forms)))))

  (testing ":allow accepts uninterned symbols"
    (ok (null (let ((forms (parser:parse-forms "(defpackage #:foo (:use #:cffi))" #p"test.lisp"))
                    (rule (make-instance 'rules:no-package-use-rule :allow '(#:cffi))))
               (mapcan (lambda (form)
                         (rules:check-form rule form #p"test.lisp"))
                       forms)))))

  (testing ":allow accepts plain symbols"
    (ok (null (let ((forms (parser:parse-forms "(defpackage #:foo (:use #:cffi))" #p"test.lisp"))
                    (rule (make-instance 'rules:no-package-use-rule :allow '(cffi))))
               (mapcan (lambda (form)
                         (rules:check-form rule form #p"test.lisp"))
                       forms))))))

;;; Registration

(deftest no-package-use-registration
  (testing "Rule is NOT in :default config (moved to :strict)"
    (let* ((cfg (config:get-built-in-config :default))
           (rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
      (ok (not (member :no-package-use rule-names)))))
  (testing "Rule IS in :strict config"
    (let* ((cfg (config:get-built-in-config :strict))
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
      ;; Exact message check: CL is exempt so only ALEXANDRIA should be named
      (ok (equal (violation:violation-message (first violations))
                 "Avoid :use of alexandria; prefer :import-from or :local-nicknames"))))

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
      (ok (= 8 (violation:violation-column (first violations))))
      (ok (equal #p"test.lisp" (violation:violation-file (first violations)))))))
