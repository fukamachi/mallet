(defpackage #:mallet/tests/cli-parsing
  (:use #:cl #:rove)
  (:import-from #:mallet
                #:parse-option-value
                #:parse-rule-options
                #:parse-rule-name
                #:parse-rule-spec
                #:parse-group-name))
(in-package #:mallet/tests/cli-parsing)

;;; Tests for parse-option-value

(deftest parse-option-value-integer
  (testing "Parse integer values"
    (ok (= 15 (parse-option-value "15")))
    (ok (= 100 (parse-option-value "100")))
    (ok (= 0 (parse-option-value "0")))))

(deftest parse-option-value-keyword
  (testing "Parse keyword values"
    (ok (eq :modified (parse-option-value "modified")))
    (ok (eq :standard (parse-option-value "standard")))
    (ok (eq :foo (parse-option-value "foo")))))

(deftest parse-option-value-string
  (testing "Parse string values (fallback)"
    ;; Strings starting with alpha become keywords
    (ok (eq :foo-bar (parse-option-value "foo-bar")))
    ;; Negative numbers and special chars stay as strings
    (ok (string= "-123" (parse-option-value "-123")))))

;;; Tests for parse-rule-options

(deftest parse-rule-options-single
  (testing "Parse single option"
    (let ((result (parse-rule-options "max=15")))
      (ok (equal '(:max 15) result)))))

(deftest parse-rule-options-multiple
  (testing "Parse multiple options"
    (let ((result (parse-rule-options "max=15,variant=modified")))
      (ok (equal '(:max 15 :variant :modified) result)))))

(deftest parse-rule-options-mixed-types
  (testing "Parse mixed option types"
    (let ((result (parse-rule-options "max=100,variant=standard,foo=bar")))
      (ok (equal '(:max 100 :variant :standard :foo :bar) result)))))

(deftest parse-rule-options-invalid
  (testing "Invalid option syntax"
    (ok (signals (parse-rule-options "invalid")
            'mallet/errors:invalid-rule-option))
    (ok (signals (parse-rule-options "max")
            'mallet/errors:invalid-rule-option))))

;;; Tests for parse-rule-name

(deftest parse-rule-name-valid
  (testing "Parse valid rule names"
    (ok (eq :cyclomatic-complexity (parse-rule-name "cyclomatic-complexity")))
    (ok (eq :line-length (parse-rule-name "line-length")))
    (ok (eq :trailing-whitespace (parse-rule-name "trailing-whitespace")))))

(deftest parse-rule-name-invalid
  (testing "Invalid rule name"
    (ok (signals (parse-rule-name "nonexistent-rule")
            'mallet/errors:unknown-rule))))

;;; Tests for parse-rule-spec

(deftest parse-rule-spec-no-options
  (testing "Parse rule spec without options"
    (let ((result (parse-rule-spec "cyclomatic-complexity")))
      (ok (eq :cyclomatic-complexity (car result)))
      (ok (null (cdr result))))))

(deftest parse-rule-spec-with-options
  (testing "Parse rule spec with options"
    (let ((result (parse-rule-spec "cyclomatic-complexity:max=15")))
      (ok (eq :cyclomatic-complexity (car result)))
      (ok (equal '(:max 15) (cdr result))))))

(deftest parse-rule-spec-multiple-options
  (testing "Parse rule spec with multiple options"
    (let ((result (parse-rule-spec "cyclomatic-complexity:max=15,variant=modified")))
      (ok (eq :cyclomatic-complexity (car result)))
      (ok (equal '(:max 15 :variant :modified) (cdr result))))))

;;; Tests for parse-group-name

(deftest parse-group-name-valid
  (testing "Parse valid group names"
    (ok (eq :error (parse-group-name "error")))
    (ok (eq :warning (parse-group-name "warning")))
    (ok (eq :convention (parse-group-name "convention")))
    (ok (eq :format (parse-group-name "format")))
    (ok (eq :info (parse-group-name "info")))
    (ok (eq :metrics (parse-group-name "metrics")))))

(deftest parse-group-name-invalid
  (testing "Invalid group name"
    (ok (signals (parse-group-name "invalid")
            'mallet/errors:invalid-group))
    (ok (signals (parse-group-name "foo")
            'mallet/errors:invalid-group))))
