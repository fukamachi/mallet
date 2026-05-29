;; Security tests: *read-eval* must be nil during all fallback cl:read calls
;; inside parse-forms to prevent read-time code execution from input files.
(defpackage #:mallet/tests/parser/read-eval-security
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:parser #:mallet/parser)
   (#:rules  #:mallet/rules)))
(in-package #:mallet/tests/parser/read-eval-security)

;; Sentinel for detecting read-time evaluation.
;; A #.(setf mallet/tests/parser/read-eval-security::*rce-sentinel* t) form
;; embedded in an input file sets this variable if *read-eval* is unguarded
;; during any fallback cl:read call inside parse-forms.
(defvar *rce-sentinel* nil)

;;; Path 1: try-skip-unknown-macro fallback
;;
;; When Eclector encounters an unknown dispatch macro (#!), it raises
;; unknown-macro-sub-character and try-skip-unknown-macro calls cl:read to
;; consume the next form from the stream.  The stream is positioned right after
;; the sub-dispatch character, so the #. form is what cl:read reads next.
;; Without an explicit (let ((*read-eval* nil)) ...) guard, that form executes.

(deftest read-eval-disabled-in-unknown-macro-fallback
  (testing "try-skip-unknown-macro: #! dispatch does not evaluate an immediate #. form"
    ;; The stream position after Eclector raises unknown-macro-sub-character
    ;; is right after the '#!' characters, so #.(setf ...) is the next form
    ;; that the fallback cl:read will read.
    (let ((*rce-sentinel* nil))
      (parser:parse-forms
       "#!#.(setf mallet/tests/parser/read-eval-security::*rce-sentinel* t)"
       #p"/tmp/test-rce-path1.lisp")
      (ok (not *rce-sentinel*)
          "sentinel must remain nil: the #. form must not execute via #! fallback"))))

;;; Path 2: eclector.base:stream-position-reader-error fallback
;;
;; When Eclector encounters a trailing double-colon (foo::), it raises
;; stream-position-reader-error.  The handler in parse-forms calls cl:read to
;; skip the malformed construct.  Without the *read-eval* guard a #. form that
;; follows is evaluated.

(deftest read-eval-disabled-in-stream-position-fallback
  (testing "stream-position-reader-error: foo:: does not evaluate following #. form"
    (let ((*rce-sentinel* nil))
      (parser:parse-forms
       (concatenate 'string
                    "foo::" (string #\Newline)
                    "#.(setf mallet/tests/parser/read-eval-security::*rce-sentinel* t)")
       #p"/tmp/test-rce-path2.lisp")
      (ok (not *rce-sentinel*)
          "sentinel must remain nil: the #. form must not execute via foo:: fallback"))))

;;; Regression: parse-forms and text-formatting rules must still work normally
;;
;; Binding *read-eval* nil around the fallback cl:read calls must not break
;; normal parse-forms behaviour or text-based rule checks.  A file with
;; trailing whitespace and no final newline must parse without errors and
;; still produce violations for both text rules.

(deftest normal-text-rules-unaffected-by-read-eval-guard
  (testing "parse-forms works on normal input and text rules still report violations"
    (let* ((text (concatenate 'string "(defun f () nil)" "   "))
           ;; Exercise parse-forms directly: a guard that breaks ordinary parsing
           ;; would error here or return the wrong number of top-level forms.
           (forms (nth-value 0 (parser:parse-forms text #p"test.lisp")))
           (tw-rule (make-instance 'rules:trailing-whitespace-rule))
           (fn-rule (make-instance 'rules:final-newline-rule))
           (tw-violations (rules:check-text tw-rule text #p"test.lisp"))
           (fn-violations (rules:check-text fn-rule text #p"test.lisp")))
      (ok (= 1 (length forms))
          "parse-forms must return exactly one top-level form on normal input")
      (ok (>= (length tw-violations) 1)
          "trailing-whitespace rule must still report a violation")
      (ok (>= (length fn-violations) 1)
          "missing-final-newline rule must still report a violation"))))
