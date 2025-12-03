(defpackage #:mallet/tests/rules/bare-float-literal
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/bare-float-literal)

;;; Helper to create tokens from text and run rule
(defun check-bare-floats (text)
  "Tokenize TEXT and check for bare float literals."
  (let* ((file #p"/tmp/test.lisp")
         (rule (make-instance 'rules:bare-float-literal-rule))
         (tokens (parser:tokenize text file)))
    (rules:check-tokens rule tokens file)))

;;; Valid cases - no violations

(deftest explicit-type-markers-valid
  (testing "Explicit single-float (f marker)"
    (ok (null (check-bare-floats "1.0f0")))
    (ok (null (check-bare-floats "0.5f0")))
    (ok (null (check-bare-floats "-3.14f0")))
    (ok (null (check-bare-floats "1f0")))
    (ok (null (check-bare-floats "1.0f5")))
    (ok (null (check-bare-floats "1.0f-3"))))

  (testing "Explicit double-float (d marker)"
    (ok (null (check-bare-floats "1.0d0")))
    (ok (null (check-bare-floats "0.5d0")))
    (ok (null (check-bare-floats "-3.14d0")))
    (ok (null (check-bare-floats "1d0")))
    (ok (null (check-bare-floats "3.14159d-10")))
    (ok (null (check-bare-floats "1d10"))))

  (testing "Explicit short-float (s marker)"
    (ok (null (check-bare-floats "1.0s0")))
    (ok (null (check-bare-floats "0.5s0")))
    (ok (null (check-bare-floats "1s0"))))

  (testing "Explicit long-float (l marker)"
    (ok (null (check-bare-floats "1.0l0")))
    (ok (null (check-bare-floats "0.5l0")))
    (ok (null (check-bare-floats "1l0"))))

  (testing "Uppercase markers"
    (ok (null (check-bare-floats "1.0F0")))
    (ok (null (check-bare-floats "1.0D0")))
    (ok (null (check-bare-floats "1.0S0")))
    (ok (null (check-bare-floats "1.0L0")))))

(deftest non-floats-valid
  (testing "Integers"
    (ok (null (check-bare-floats "1")))
    (ok (null (check-bare-floats "100")))
    (ok (null (check-bare-floats "-42")))
    (ok (null (check-bare-floats "+123"))))

  (testing "Ratios"
    (ok (null (check-bare-floats "1/2")))
    (ok (null (check-bare-floats "22/7")))
    (ok (null (check-bare-floats "-3/4")))))

;;; Invalid cases - violations

(deftest bare-decimal-floats-violation
  (testing "Simple bare decimals"
    (let ((violations (check-bare-floats "1.0")))
      (ok (= 1 (length violations)))
      (ok (eq :bare-float-literal (violation:violation-rule (first violations))))))

  (testing "Various bare decimal forms"
    (ok (= 1 (length (check-bare-floats "0.5"))))
    (ok (= 1 (length (check-bare-floats "-3.14"))))
    (ok (= 1 (length (check-bare-floats "0.2356"))))
    (ok (= 1 (length (check-bare-floats "100.0")))))

  (testing "Leading decimal point"
    (ok (= 1 (length (check-bare-floats ".5")))))

  (testing "Trailing decimal point - integer, not float"
    ;; Note: 1. is read as integer 1 in Common Lisp, not as a float
    (ok (null (check-bare-floats "1.")))))

(deftest e-marker-floats-violation
  (testing "Floats with e marker (ambiguous type)"
    (ok (= 1 (length (check-bare-floats "1e10"))))
    (ok (= 1 (length (check-bare-floats "1.0e0"))))
    (ok (= 1 (length (check-bare-floats "0.5e5"))))
    (ok (= 1 (length (check-bare-floats "1.0e-3"))))
    (ok (= 1 (length (check-bare-floats "1E10"))))
    (ok (= 1 (length (check-bare-floats "1.0E0"))))))

;;; False positive prevention

(deftest false-positive-prevention
  (testing "Misclassified symbols (no digits)"
    (ok (null (check-bare-floats "feed")))
    (ok (null (check-bare-floats "deed")))
    (ok (null (check-bare-floats "deeds"))))

  (testing "Version strings (multiple decimal points)"
    (ok (null (check-bare-floats "1.0.0")))
    (ok (null (check-bare-floats "2.1.3")))
    (ok (null (check-bare-floats "10.0.1"))))

  (testing "Invalid sign placement"
    (ok (null (check-bare-floats "-1.0-1")))
    (ok (null (check-bare-floats "1.0+2"))))

  (testing "Incomplete exponents"
    (ok (null (check-bare-floats "1e")))
    (ok (null (check-bare-floats "1.0e")))
    (ok (null (check-bare-floats "1E")))))

;;; Context tests

(deftest multiple-floats-in-code
  (testing "Multiple floats in expression"
    (let ((violations (check-bare-floats "(+ 1.0 2.0 3.0d0)")))
      (ok (= 2 (length violations)))))

  (testing "Mixed valid and invalid"
    (let ((violations (check-bare-floats "(list 1.0f0 2.0 3.0d0 4.0)")))
      (ok (= 2 (length violations))))))

(deftest floats-in-strings-and-comments
  (testing "Float in string - no violation"
    (ok (null (check-bare-floats "\"1.0\""))))

  (testing "Float in comment - no violation"
    (ok (null (check-bare-floats "; 1.0")))))
