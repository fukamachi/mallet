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
        (ok (string= "intentional" (fourth directive))))))
  (testing "Suppress with reason after em dash —"
    (let ((result (suppression:parse-comment-directives
                    "; mallet:suppress rule1 — em dash reason")))
      (ok (= 1 (length result)))
      (let ((directive (first result)))
        (ok (= 1 (first directive)))
        (ok (eq :suppress (second directive)))
        (ok (equal '(:rule1) (third directive)))
        (ok (string= "em dash reason" (fourth directive))))))
  (testing "Suppress with rule only, em dash with no reason text"
    (let ((result (suppression:parse-comment-directives
                    "; mallet:suppress rule1 —")))
      (ok (= 1 (length result)))
      (let ((directive (first result)))
        (ok (equal '(:rule1) (third directive)))
        (ok (null (fourth directive)))))))

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

(deftest parse-comment-directives-semicolon-in-comment-prose
  (testing "Directive-like ; mallet:suppress embedded in ;; comment prose is not matched"
    ;; A semicolon that is already inside a comment (started by an earlier ;) must
    ;; not be treated as a fresh directive starter.
    (ok (null (suppression:parse-comment-directives
                ";; prose text ; mallet:suppress rule1"))
        "embedded ; mallet:suppress in ;; comment is ignored"))

  (testing "Directive-like ; mallet:suppress embedded in ;;; comment prose is not matched"
    (ok (null (suppression:parse-comment-directives
                ";;; Test: inner-form ; mallet:suppress :needless-let*"))
        "embedded ; mallet:suppress in ;;; comment is ignored"))

  (testing "Real ; mallet:suppress after code on the same line IS matched"
    ;; This is a trailing comment, not a prose-embedded one.  The first
    ;; unquoted semicolon on the line starts the directive.
    (let ((result (suppression:parse-comment-directives
                    "(let* ((x 1)) x) ; mallet:suppress :needless-let*")))
      (ok (= 1 (length result)) "trailing ; mallet:suppress is matched")
      (ok (eq :suppress (second (first result)))))))

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

(deftest parse-comment-directives-block-comment-with-quote
  (testing "Multi-line block comment with quote does not corrupt string state"
    (let ((result (suppression:parse-comment-directives
                    (format nil "#| start~%has \" quote~%|#~%; mallet:suppress rule1~%"))))
      (ok (= 1 (length result)) "directive after multi-line block comment with quote is matched")
      (ok (= 4 (first (first result))) "directive is on line 4")
      (ok (eq :suppress (second (first result))))))

  (testing "Multiple lines inside block comment with quotes do not affect string state"
    (let ((result (suppression:parse-comment-directives
                    (format nil "#|~%\"first quote~%\"second quote~%|#~%; mallet:suppress rule1~%"))))
      (ok (= 1 (length result)) "directive after block comment with multiple quotes is matched")
      (ok (= 5 (first (first result))) "directive is on line 5")))

  (testing "Same-line block comment with quote does not corrupt string state"
    ;; #| " |# on a single line where depth-at-start is 0 — the quote inside
    ;; should not toggle in-string-p.
    (let ((result (suppression:parse-comment-directives
                    (format nil "#| \" |#~%; mallet:suppress rule1~%"))))
      (ok (= 1 (length result)) "directive after same-line block comment with quote is matched")
      (ok (= 2 (first (first result))) "directive is on line 2")))

  (testing "Block comment closing on line that opens a string tracks string state"
    ;; |# closes the block comment, then " opens a string. The directive-like
    ;; text on line 3 is inside that string and must NOT be matched.
    (let ((result (suppression:parse-comment-directives
                    (format nil "#| comment~%|# (foo \"text~%; mallet:suppress rule1~%end\")~%"))))
      (ok (null result) "directive inside string opened after |# is not matched"))))

(deftest parse-comment-directives-line-comment-with-quote
  (testing "Quote inside a line comment does not corrupt string state"
    ;; A comment like ; Don't use "eval" has quotes that are NOT string openers.
    ;; Without line-comment awareness, %string-state-after-line would toggle
    ;; in-string-p and miss the directive on the next line.
    (let ((result (suppression:parse-comment-directives
                    (format nil "; Don't use \"eval\"~%; mallet:suppress rule1~%"))))
      (ok (= 1 (length result)) "directive after comment with quotes is matched")
      (ok (= 2 (first (first result))) "directive is on line 2")))

  (testing "Odd number of quotes in line comment does not corrupt string state"
    (let ((result (suppression:parse-comment-directives
                    (format nil "; contains \" one quote~%; mallet:suppress rule1~%"))))
      (ok (= 1 (length result)) "directive after comment with odd quotes is matched")
      (ok (= 2 (first (first result))) "directive is on line 2"))))

(deftest parse-comment-directives-char-literal-at-end
  (testing "Incomplete #\\ at end of line does not crash or mismatch"
    (let ((result (suppression:parse-comment-directives
                    (format nil "(foo #\\~%; mallet:suppress rule1~%"))))
      (ok (= 1 (length result)) "directive after line ending with #\\\\ is matched")
      (ok (= 2 (first (first result))) "directive is on line 2")))

  (testing "#\\; character literal does not expose semicolon as a comment"
    ;; The semicolon in #\; is a character literal, not a comment starter.
    ;; A directive-like pattern after #\; on the same line must not be matched.
    (let ((result (suppression:parse-comment-directives
                    (format nil "(foo #\\; mallet:suppress rule1)~%"))))
      (ok (null result) "#\\; semicolon is not treated as a comment")))

  (testing "Trailing ; mallet:suppress after #\\; character literal is still recognized"
    ;; (format t "~C" #\;) ; mallet:suppress :needless-let*
    ;; The #\; is a character literal (consumes the ; at that position).
    ;; The real directive ; mallet:suppress appears later on the same line —
    ;; %semicolon-is-first-on-line-p must detect #\; as a char literal (not an
    ;; earlier unquoted semicolon) and return T so the trailing directive is matched.
    (let ((result (suppression:parse-comment-directives
                    "(format t \"~C\" #\\;) ; mallet:suppress :needless-let*")))
      (ok (= 1 (length result)) "trailing directive after #\\; char literal is matched")
      (when (= 1 (length result))
        (ok (eq :suppress (second (first result))) "type is :suppress")
        (ok (equal '(:needless-let*) (third (first result))) "rule is :needless-let*")))))

(deftest parse-comment-directives-block-comment-in-line-comment
  (testing "#| inside ;;; line comment does not start a block comment"
    ;; Without fix, the #| in ;;; increments depth, causing the directive on
    ;; line 2 to be seen as inside a block comment and ignored.
    (let ((result (suppression:parse-comment-directives
                    (format nil ";;; note about #| delimiters~%; mallet:suppress rule1~%"))))
      (ok (= 1 (length result)) "directive after ;;; #| line is matched")
      (ok (= 2 (first (first result))) "directive is on line 2")))

  (testing "|# inside ;;; line comment does not close a real block comment prematurely"
    ;; Without fix, the |# in ;;; decrements depth to 0, causing the directive
    ;; on line 3 to be wrongly matched (should be inside the block comment).
    (let ((result (suppression:parse-comment-directives
                    (format nil "#| open block~%;;; |# fake closer~%; mallet:suppress rule1~%|#~%; mallet:suppress rule2~%"))))
      (ok (= 1 (length result)) "only directive after real block comment close is matched")
      (ok (= 5 (first (first result))) "directive is on line 5")))

  (testing "#| inside a string literal does not start a block comment"
    ;; Without fix, #| inside a string increments depth, causing the directive
    ;; on line 2 to be seen as inside a block comment.
    (let ((result (suppression:parse-comment-directives
                    (format nil "(foo \"#|\")~%; mallet:suppress rule1~%"))))
      (ok (= 1 (length result)) "directive after string with #| is matched")
      (ok (= 2 (first (first result))) "directive is on line 2")))

  (testing "#\\| character literal before #| does not create a false |# close"
    ;; #\|#| contains char literal #\| (for |) followed by #| (block comment opener).
    ;; Without fix, |# is counted as a closer (depth stays 0) and #| is missed,
    ;; so the directive on line 2 is wrongly matched instead of being suppressed.
    (let ((result (suppression:parse-comment-directives
                    (format nil "(code #\\|#| block comment~%; mallet:suppress rule1~%|#~%; mallet:suppress rule2~%"))))
      (ok (= 1 (length result)) "only directive after block comment close is matched")
      (ok (= 4 (first (first result))) "directive is on line 4")))

  (testing "Real mid-line #| opener (not in string or comment) is still counted"
    ;; Verifies the fix doesn't over-correct: a genuine #| not preceded by ;
    ;; or inside a string must still open a block comment so the next line is
    ;; treated as inside it.  A stub returning (values 0 0) would fail this.
    (let ((result (suppression:parse-comment-directives
                    (format nil "(foo) #| block starts here~%; mallet:suppress rule1~%|#~%; mallet:suppress rule2~%"))))
      (ok (= 1 (length result)) "directive inside block comment is not matched")
      (ok (= 4 (first (first result))) "only directive after |# is matched")))

  (testing "|# inside string literal does not close a real block comment"
    ;; Block comment is open from line 1.  Line 2 has |# inside a string,
    ;; which the fix must ignore.  Without the fix to %count-block-comment-delimiters
    ;; treating strings, |# in the string would decrement depth to 0 and the
    ;; directive on line 3 would be wrongly matched.
    ;; A stub returning (values 0 0) would also fail this: depth never increases
    ;; so both line-3 and line-5 directives get matched.
    (let ((result (suppression:parse-comment-directives
                    (format nil "#| open block~%(foo \"|#\")~%; mallet:suppress rule1~%|#~%; mallet:suppress rule2~%"))))
      (ok (= 1 (length result)) "only directive after real |# close is matched")
      (ok (= 5 (first (first result))) "directive is on line 5")))

  (testing "Same-line #| ... |# counts one opener and one closer correctly"
    ;; Line 1 has two openers and one closer: net depth +1.
    ;; The directive-like text on line 2 is inside the second (unclosed) block comment.
    ;; With a correct implementation only the line-4 directive is matched.
    ;; A stub (values 0 0) would leave depth at 0 and match both directives.
    (let ((result (suppression:parse-comment-directives
                    (format nil "#| first |# #| second~%; mallet:suppress rule1~%|#~%; mallet:suppress rule2~%"))))
      (ok (= 1 (length result)) "only directive after second block comment close is matched")
      (ok (= 4 (first (first result))) "directive is on line 4"))))
