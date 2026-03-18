(defpackage #:mallet/tests/rules/missing-package-docstring
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/missing-package-docstring)

(defun check-missing-package-docstring (code)
  "Check CODE for missing-package-docstring violations."
  (let ((forms (parser:parse-forms code #p"test.lisp"))
        (rule (make-instance 'rules:missing-package-docstring-rule)))
    (mapcan (lambda (form)
              (rules:check-form rule form #p"test.lisp"))
            forms)))

;;; Valid cases (no violations)

(deftest missing-package-docstring-valid
  (testing "defpackage with :documentation is not flagged"
    (ok (null (check-missing-package-docstring
               "(defpackage #:my-pkg
  (:use #:cl)
  (:documentation \"My package.\"))"))))

  (testing "define-package with :documentation is not flagged"
    (ok (null (check-missing-package-docstring
               "(define-package #:my-pkg
  (:use #:cl)
  (:documentation \"My package.\"))"))))

  (testing "defun form is not flagged by this rule"
    (ok (null (check-missing-package-docstring
               "(defun foo () (+ 1 2))"))))

  (testing "defclass form is not flagged by this rule"
    (ok (null (check-missing-package-docstring
               "(defclass point () ())")))))

;;; Invalid cases (violations expected)

(deftest missing-package-docstring-invalid
  (testing "defpackage without :documentation is flagged"
    (let ((violations (check-missing-package-docstring
                       "(defpackage #:my-pkg (:use #:cl))")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :missing-package-docstring))))

  (testing "define-package without :documentation is flagged"
    (let ((violations (check-missing-package-docstring
                       "(define-package #:my-pkg (:use #:cl))")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :missing-package-docstring))))

  (testing "defpackage with no options at all is flagged"
    (let ((violations (check-missing-package-docstring
                       "(defpackage #:empty)")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :missing-package-docstring))))

  (testing "severity is :info by default"
    (let ((violations (check-missing-package-docstring
                       "(defpackage #:my-pkg)")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-severity (first violations)) :info)))))

;;; Violation message format

(deftest missing-package-docstring-message-format
  (testing "defpackage violation message includes DEFPACKAGE and package name"
    (let ((violations (check-missing-package-docstring
                       "(defpackage #:my-pkg (:use #:cl))")))
      (ok (= (length violations) 1))
      (let ((msg (violation:violation-message (first violations))))
        (ok (stringp msg))
        (ok (search "DEFPACKAGE" msg))
        (ok (search "my-pkg" msg)))))

  (testing "define-package violation message includes DEFINE-PACKAGE and package name"
    (let ((violations (check-missing-package-docstring
                       "(define-package #:other-pkg (:use #:cl))")))
      (ok (= (length violations) 1))
      (let ((msg (violation:violation-message (first violations))))
        (ok (search "DEFINE-PACKAGE" msg))
        (ok (search "other-pkg" msg))))))

;;; Location reporting

(deftest missing-package-docstring-location
  (testing "violation reports line 1 for single-line form"
    (let ((violations (check-missing-package-docstring
                       "(defpackage #:my-pkg)")))
      (ok (= (length violations) 1))
      (ok (= (violation:violation-line (first violations)) 1))))

  (testing "violation reports correct line for multi-line code"
    (let ((violations (check-missing-package-docstring
                       "(defun foo () 1)
(defpackage #:my-pkg)")))
      (ok (= (length violations) 1))
      (ok (= (violation:violation-line (first violations)) 2)))))

;;; Mixed forms

(deftest missing-package-docstring-mixed-forms
  (testing "documented package not flagged, undocumented one flagged"
    (let ((violations (check-missing-package-docstring
                       "(defpackage #:documented (:documentation \"Yes.\"))
(defpackage #:undocumented (:use #:cl))")))
      (ok (= (length violations) 1))
      (ok (search "undocumented" (violation:violation-message (first violations))))))

  (testing "multiple packages without docs both flagged"
    (let ((violations (check-missing-package-docstring
                       "(defpackage #:foo)
(defpackage #:bar)")))
      (ok (= (length violations) 2)))))
