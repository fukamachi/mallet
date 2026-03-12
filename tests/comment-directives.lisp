(defpackage #:mallet/tests/comment-directives
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:suppression #:mallet/suppression)))
(in-package #:mallet/tests/comment-directives)

(deftest parse-comment-directives-single-rule
  (testing "Single suppress rule"
    (let ((result (suppression:parse-comment-directives
                    "; mallet:suppress needless-let*")))
      (ok (= 1 (length result)))
      (let ((directive (first result)))
        (ok (= 1 (first directive)))
        (ok (eq :suppress (second directive)))
        (ok (equal '(:needless-let*) (third directive)))
        (ok (null (fourth directive)))))))

(deftest parse-comment-directives-multiple-rules
  (testing "Multiple suppress rules"
    (let ((result (suppression:parse-comment-directives
                    "; mallet:suppress rule1 rule2")))
      (ok (= 1 (length result)))
      (let ((directive (first result)))
        (ok (= 1 (first directive)))
        (ok (eq :suppress (second directive)))
        (ok (equal '(:rule1 :rule2) (third directive)))
        (ok (null (fourth directive)))))))

(deftest parse-comment-directives-reason-string
  (testing "Suppress with reason after --"
    (let ((result (suppression:parse-comment-directives
                    "; mallet:suppress rule1 -- intentional")))
      (ok (= 1 (length result)))
      (let ((directive (first result)))
        (ok (= 1 (first directive)))
        (ok (eq :suppress (second directive)))
        (ok (equal '(:rule1) (third directive)))
        (ok (string= "intentional" (fourth directive)))))))

(deftest parse-comment-directives-multiple-semicolons
  (testing "Multiple semicolons like ;;; mallet:disable"
    (let ((result (suppression:parse-comment-directives
                    ";;; mallet:disable rule1")))
      (ok (= 1 (length result)))
      (let ((directive (first result)))
        (ok (= 1 (first directive)))
        (ok (eq :disable (second directive)))
        (ok (equal '(:rule1) (third directive)))
        (ok (null (fourth directive)))))))

(deftest parse-comment-directives-indented
  (testing "Indented comment directive"
    (let ((result (suppression:parse-comment-directives
                    "   ; mallet:suppress rule1")))
      (ok (= 1 (length result)))
      (let ((directive (first result)))
        (ok (= 1 (first directive)))
        (ok (eq :suppress (second directive)))
        (ok (equal '(:rule1) (third directive)))
        (ok (null (fourth directive)))))))

(deftest parse-comment-directives-non-mallet-comment
  (testing "Non-mallet comment produces no match"
    (let ((result (suppression:parse-comment-directives
                    "; just a regular comment")))
      (ok (null result)))))

(deftest parse-comment-directives-empty-rules
  (testing "Empty rules list produces no match"
    (let ((result (suppression:parse-comment-directives
                    "; mallet:suppress")))
      (ok (null result)))))

(deftest parse-comment-directives-disable-type
  (testing "Disable type directive"
    (let ((result (suppression:parse-comment-directives
                    "; mallet:disable rule1 rule2")))
      (ok (= 1 (length result)))
      (let ((directive (first result)))
        (ok (eq :disable (second directive)))
        (ok (equal '(:rule1 :rule2) (third directive)))))))

(deftest parse-comment-directives-enable-type
  (testing "Enable type directive"
    (let ((result (suppression:parse-comment-directives
                    "; mallet:enable rule1")))
      (ok (= 1 (length result)))
      (let ((directive (first result)))
        (ok (eq :enable (second directive)))
        (ok (equal '(:rule1) (third directive)))))))

(deftest parse-comment-directives-multiline-sorted
  (testing "Multiple directives sorted by line number"
    (let ((result (suppression:parse-comment-directives
                    (format nil "~%~%; mallet:disable rule-a~%; mallet:enable rule-b"))))
      (ok (= 2 (length result)))
      ;; First directive is on line 3
      (ok (= 3 (first (first result))))
      (ok (eq :disable (second (first result))))
      (ok (equal '(:rule-a) (third (first result))))
      ;; Second directive is on line 4
      (ok (= 4 (first (second result))))
      (ok (eq :enable (second (second result))))
      (ok (equal '(:rule-b) (third (second result)))))))

(deftest parse-comment-directives-trailing-same-line
  (testing "Trailing comment on same line as code is matched"
    (let ((result (suppression:parse-comment-directives
                    "(let* ((x (foo))) ; mallet:suppress needless-let*")))
      (ok (= 1 (length result)))
      (let ((directive (first result)))
        (ok (= 1 (first directive)))
        (ok (eq :suppress (second directive)))
        (ok (equal '(:needless-let*) (third directive)))
        (ok (null (fourth directive))))))

  (testing "Trailing comment with reason is matched"
    (let ((result (suppression:parse-comment-directives
                    "(foo x) ; mallet:suppress rule1 -- legacy")))
      (ok (= 1 (length result)))
      (let ((directive (first result)))
        (ok (eq :suppress (second directive)))
        (ok (equal '(:rule1) (third directive)))
        (ok (string= "legacy" (fourth directive)))))))

(deftest parse-comment-directives-rule-normalization
  (testing "Rule names are normalized to uppercase keywords"
    (let ((result (suppression:parse-comment-directives
                    "; mallet:suppress Needless-Let* LINE-LENGTH")))
      (ok (= 1 (length result)))
      (let ((rules (third (first result))))
        (ok (member :needless-let* rules :test #'eq))
        (ok (member :line-length rules :test #'eq))))))
