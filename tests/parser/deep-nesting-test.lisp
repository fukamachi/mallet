(defpackage #:mallet/tests/parser/deep-nesting
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:parser #:mallet/parser)
   (#:engine #:mallet/engine)
   (#:config #:mallet/config)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/parser/deep-nesting)

(defun make-deeply-nested-text (depth)
  "Generate a string with DEPTH levels of nested parentheses."
  (concatenate 'string
               (make-string depth :initial-element #\()
               (make-string depth :initial-element #\))))

;;; AC1: 2000-level nesting must not let storage-condition escape parse-forms.
;;; SB-KERNEL:CONTROL-STACK-EXHAUSTED is a STORAGE-CONDITION, not an ERROR,
;;; so the existing (error ...) handler does not catch it.  Before the fix,
;;; it propagates out and SBCL prints a ~182KB backtrace.

(deftest deeply-nested-form-no-unhandled-condition
  (testing "parse-forms does not let storage-condition escape for 2000-level nesting"
    (let* ((text (make-deeply-nested-text 2000))
           (condition-escaped nil))
      (handler-case
          (parser:parse-forms text #P"deep.lisp")
        (storage-condition ()
          (setf condition-escaped t)))
      ;; FAILS before fix: CONTROL-STACK-EXHAUSTED propagates out of parse-forms
      (ng condition-escaped
          "storage-condition must not escape from parse-forms on deeply nested input"))))

;;; AC1 (tightened): directly assert that *error-output* contains no Lisp backtrace
;;; strings.  An outer handler-case is used so the test fails gracefully (not via
;;; process crash) before the fix: when storage-condition escapes parse-forms, the
;;; handler branch executes (ok nil ...) which is a deliberate failing assertion.
;;; After the fix, parse-forms returns normally and the assertions on the captured
;;; *error-output* content are checked.

(deftest deeply-nested-form-no-backtrace-in-stderr
  (testing "parse-forms with 2000-level nesting writes no Lisp backtrace to *error-output*"
    (let* ((text (make-deeply-nested-text 2000)))
      (handler-case
          (let ((stderr-output
                  (with-output-to-string (*error-output*)
                    (parser:parse-forms text #P"deep.lisp"))))
            ;; PASSES after fix: parse-forms catches the condition internally and
            ;; returns normally; no Lisp-level backtrace is written.
            (ng (search "Backtrace for:" stderr-output)
                "Lisp backtrace header must not appear in *error-output* for deeply nested input")
            (ng (search "CONTROL-STACK-EXHAUSTED" stderr-output)
                "CONTROL-STACK-EXHAUSTED must not appear in *error-output*"))
        (storage-condition ()
          ;; FAILS before fix: storage-condition escaped parse-forms.
          ;; When the condition escapes, SBCL would print a ~182KB backtrace to stderr.
          (ok nil "STORAGE-CONDITION escaped parse-forms — backtrace would be printed to stderr"))))))

;;; AC2: a single clean parse-error-info is returned; no backtrace is printed.

(deftest deeply-nested-form-single-parse-error
  (testing "parse-forms returns exactly one parse error for 2000-level nesting"
    (let* ((text (make-deeply-nested-text 2000))
           (errors nil))
      (handler-case
          (multiple-value-bind (forms parse-errors)
              (parser:parse-forms text #P"deep.lisp")
            (declare (ignore forms))
            (setf errors parse-errors))
        ;; If the condition still escapes we catch it here; errors stays nil and
        ;; the (= 1 …) assertion below fails, which is the correct failure mode.
        (storage-condition ()))
      ;; FAILS before fix: errors is nil because the condition escaped
      (ok (= 1 (length errors))
          "Exactly one parse error for 2000-level deeply nested form")))

  (testing "parse error message for 2000-level nesting mentions nesting depth"
    (let* ((text (make-deeply-nested-text 2000))
           (errors nil))
      (handler-case
          (multiple-value-bind (forms parse-errors)
              (parser:parse-forms text #P"deep.lisp")
            (declare (ignore forms))
            (setf errors parse-errors))
        (storage-condition ()))
      ;; FAILS before fix: errors is nil
      (ok (and errors
               (search "nest" (string-downcase
                               (parser:parse-error-info-message (first errors)))))
          "Parse error message must indicate the expression is too deeply nested"))))

;;; AC2 (exit-code coverage): verify that a deeply nested file produces an :error-severity
;;; violation through the linting engine.  Violations at :error severity cause the CLI to
;;; exit non-zero (should-fail-p returns T for the default :warning threshold).
;;;
;;; An outer handler-case ensures the test fails gracefully before the fix: when
;;; CONTROL-STACK-EXHAUSTED escapes engine:lint-file, (ok nil ...) fires rather
;;; than crashing the process.  After the fix, lint-file returns a parse-error
;;; violation with :severity :error and the ok assertion passes.

(deftest deeply-nested-form-causes-error-exit
  (testing "engine:lint-file on a 2000-level nested file returns an :error-severity violation"
    (let* ((text (make-deeply-nested-text 2000))
           (tmp-path (merge-pathnames "mallet-deep-nesting-test.lisp"
                                      (uiop:temporary-directory))))
      (unwind-protect
           (progn
             (with-open-file (stream tmp-path
                                     :direction :output
                                     :if-does-not-exist :create
                                     :if-exists :supersede)
               (write-string text stream))
             (handler-case
                 ;; Use the default built-in config to ensure form-level processing
                 ;; (and thus parse-forms) is invoked; only the parse-error violation
                 ;; matters for this test.
                 (let ((violations (engine:lint-file tmp-path
                                                     :config (config:get-built-in-config))))
                   ;; PASSES after fix: parse-error-info is converted to an :error violation.
                   (ok (some (lambda (v)
                               (eq :error (violation:violation-severity v)))
                             violations)
                       "At least one :error-severity violation must be present (causes non-zero CLI exit)"))
               (storage-condition ()
                 ;; FAILS before fix: CONTROL-STACK-EXHAUSTED propagates out of lint-file.
                 (ok nil "STORAGE-CONDITION escaped engine:lint-file — parse error not reported as a violation"))))
        (when (probe-file tmp-path)
          (delete-file tmp-path))))))

;;; AC3: normally-nested input (~20 levels) must be completely unaffected.

(deftest normally-nested-form-parses-without-error
  (testing "parse-forms handles 20-level nesting without parse error"
    (let* ((text (make-deeply-nested-text 20)))
      (multiple-value-bind (forms errors)
          (parser:parse-forms text #P"normal.lisp")
        (ok (null errors) "20-level nesting produces no parse errors")
        (ok (= 1 (length forms)) "20-level nesting parses as one complete form")))))
