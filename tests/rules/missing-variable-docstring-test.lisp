(defpackage #:mallet/tests/rules/missing-variable-docstring
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/missing-variable-docstring)

(defun check-missing-variable-docstring (code)
  "Check CODE for missing-variable-docstring violations."
  (let ((forms (parser:parse-forms code #p"test.lisp"))
        (rule (make-instance 'rules:missing-variable-docstring-rule)))
    (mapcan (lambda (form)
              (rules:check-form rule form #p"test.lisp"))
            forms)))

;;; Valid cases (no violations)

(deftest missing-variable-docstring-valid
  (testing "defvar with docstring is not flagged"
    (ok (null (check-missing-variable-docstring
               "(defvar *x* 42 \"The answer.\")"))))

  (testing "defparameter with docstring is not flagged"
    (ok (null (check-missing-variable-docstring
               "(defparameter *max-retries* 5 \"Maximum retry count.\")"))))

  (testing "defvar without init value is not flagged (not checkable)"
    (ok (null (check-missing-variable-docstring
               "(defvar *x*)"))))

  (testing "defun without docstring is not flagged by this rule"
    (ok (null (check-missing-variable-docstring
               "(defun foo (x) x)"))))

  (testing "defclass without docstring is not flagged by this rule"
    (ok (null (check-missing-variable-docstring
               "(defclass point () ())")))))

;;; Invalid cases (violations expected)

(deftest missing-variable-docstring-invalid
  (testing "defvar with init value but no docstring is flagged"
    (let ((violations (check-missing-variable-docstring "(defvar *x* 42)")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :missing-variable-docstring))))

  (testing "defparameter without docstring is flagged"
    (let ((violations (check-missing-variable-docstring
                       "(defparameter *max-retries* 5)")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :missing-variable-docstring))
      (ok (eq (violation:violation-severity (first violations)) :info))))

  (testing "defvar with nil init value but no docstring is flagged"
    (let ((violations (check-missing-variable-docstring "(defvar *state* nil)")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :missing-variable-docstring)))))

;;; Violation message format

(deftest missing-variable-docstring-message-format
  (testing "defvar violation message includes DEFVAR and variable name"
    (let ((violations (check-missing-variable-docstring "(defvar *counter* 0)")))
      (ok (= (length violations) 1))
      (let ((msg (violation:violation-message (first violations))))
        (ok (stringp msg))
        (ok (search "DEFVAR" msg))
        (ok (search "*counter*" msg)))))

  (testing "defparameter violation message includes DEFPARAMETER and variable name"
    (let ((violations (check-missing-variable-docstring "(defparameter *limit* 100)")))
      (ok (= (length violations) 1))
      (let ((msg (violation:violation-message (first violations))))
        (ok (search "DEFPARAMETER" msg))
        (ok (search "*limit*" msg))))))

;;; Location reporting

(deftest missing-variable-docstring-location
  (testing "violation reports line 1 for single-line form"
    (let ((violations (check-missing-variable-docstring "(defvar *x* 0)")))
      (ok (= (length violations) 1))
      (ok (= (violation:violation-line (first violations)) 1))))

  (testing "violation reports correct line for multi-line code"
    (let ((violations (check-missing-variable-docstring
                       "(defvar *x* 0)
(defparameter *y* 1)")))
      (ok (= (length violations) 2))
      (ok (= (violation:violation-line (first violations)) 1))
      (ok (= (violation:violation-line (second violations)) 2)))))

;;; Multiple forms

(deftest missing-variable-docstring-multiple-forms
  (testing "multiple missing docstrings are all flagged"
    (let ((violations (check-missing-variable-docstring
                       "(defvar *a* 1)
(defparameter *b* 2)
(defvar *c* 3)")))
      (ok (= (length violations) 3))))

  (testing "mix of documented and undocumented variables"
    (let ((violations (check-missing-variable-docstring
                       "(defvar *a* 1 \"Documented.\")
(defparameter *b* 2)")))
      (ok (= (length violations) 1))
      (ok (search "*b*" (violation:violation-message (first violations)))))))
