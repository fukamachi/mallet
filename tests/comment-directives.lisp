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

(deftest parse-comment-directives-block-comment
  (testing "Directive inside #| |# block comment is not matched"
    ;; A ; mallet:suppress inside a block comment should be ignored
    (let ((result (suppression:parse-comment-directives
                    (format nil "#|~%; mallet:suppress needless-let*~%|#~%(defun foo () nil)"))))
      (ok (null result) "no directives matched inside block comment")))

  (testing "Multi-line block comment spanning directive is skipped"
    (let ((result (suppression:parse-comment-directives
                    (format nil "#| opened~%; mallet:disable line-length~%still inside |#~%real code"))))
      (ok (null result) "directive inside multi-line block comment is ignored")))

  (testing "Directive after block comment is matched"
    (let ((result (suppression:parse-comment-directives
                    (format nil "#| comment |#~%; mallet:suppress needless-let*~%(defun foo () nil)"))))
      (ok (= 1 (length result)))
      (ok (= 2 (first (first result))) "directive on line 2 is matched"))))

(deftest parse-comment-directives-string-literal
  (testing "Directive-like pattern inside a string literal is not matched"
    ;; The semicolon is preceded by an odd number of double-quotes, so it is
    ;; inside a string and should not be treated as a comment directive.
    (let ((result (suppression:parse-comment-directives
                    "(format t \"; mallet:suppress needless-let*\")")))
      (ok (null result) "directive inside string literal is not matched")))

  (testing "Directive after a closed string on the same line is matched"
    (let ((result (suppression:parse-comment-directives
                    "(foo \"bar\") ; mallet:suppress rule1")))
      (ok (= 1 (length result)) "directive after closed string is matched")
      (ok (eq :suppress (second (first result)))))))

(deftest parse-comment-directives-multiline-string
  (testing "Directive-like text on a continuation line starting with no quote is not matched"
    ;; The string opens on line 1. Line 2 has NO quote before the semicolon,
    ;; so %semicolon-in-string-p would return NIL without cross-line tracking.
    ;; This is the real bug: line 2 looks like a real directive.
    (let ((result (suppression:parse-comment-directives
                    (format nil "(defun foo ()~%  \"This is a docstring~%; mallet:suppress rule1~%  end of docstring\")~%"))))
      (ok (null result) "directive on continuation line (no quote before semi) is not matched")))

  (testing "Directive-like text mid-string where opening quote is on previous line is not matched"
    ;; The string opens with a quote on line 2 and the directive-like line (line 3)
    ;; starts with no quotes — only cross-line string tracking detects this.
    (let ((result (suppression:parse-comment-directives
                    (format nil "(defun foo (x)~%  \"docstring text~%; mallet:suppress needless-let*~%  more text\")~%"))))
      (ok (null result) "no directive matched when semicolon line has no preceding quote")))

  (testing "Real directive after the multi-line string is matched"
    (let ((result (suppression:parse-comment-directives
                    (format nil "(defun foo ()~%  \"docstring with~%  some text\")~%; mallet:suppress rule1~%"))))
      (ok (= 1 (length result)) "real directive after string is matched")
      (ok (= 4 (first (first result))) "directive is on line 4")))

  (testing "Directive-like text on same line as opening quote is still protected by single-line check"
    ;; This already worked before: the quote is on the same line as the semicolon.
    (let ((result (suppression:parse-comment-directives
                    (format nil "(defun foo ()~%  \"docstring with ; mallet:suppress rule1~%  and more text\")~%"))))
      (ok (null result) "directive inside single-line string open is not matched")))

  (testing "Character literal #\\\" does not confuse string tracking outside a string"
    ;; Code like (char= x #\") contains a #\" which is NOT a string opener.
    ;; The line after it should not be considered inside a string.
    (let ((result (suppression:parse-comment-directives
                    (format nil "(cond ((char= x #\\\")~%; mallet:suppress rule1~%  (do-something)))~%"))))
      (ok (= 1 (length result)) "directive after #\\\" character literal is matched")
      (ok (= 2 (first (first result))) "directive is on line 2"))))
