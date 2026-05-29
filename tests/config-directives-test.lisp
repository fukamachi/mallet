(defpackage #:mallet/tests/config-directives
  (:use #:cl #:rove)
  (:local-nicknames
   (#:config #:mallet/config)
   (#:errors #:mallet/errors)
   (#:ppcre #:cl-ppcre)
   (#:engine #:mallet/engine)
   (#:rules #:mallet/rules)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/config-directives)

(defmacro with-temporary-config ((content pathname) &body body)
  `(uiop:with-temporary-file (:stream out
                              :pathname ,pathname
                              :direction :output
                              :prefix "mallet-config"
                              :type "lisp")
     (write-string ,content out)
     (force-output out)
     ,@body))

;;; AC1: parse-config must reject unknown top-level config directives.
;;;
;;; Currently the `case` form in parse-config (src/config.lisp:327) has no
;;; default/otherwise branch, so unknown keys like :preset are silently
;;; ignored. The linter then runs with no rules loaded and reports clean,
;;; giving a false green result.

(deftest parse-config-rejects-unknown-directives
  (testing ":preset is not a valid config directive — must signal an error"
    ;; Currently returns a config silently (no rules loaded).
    ;; After the fix: an error is signaled for the unknown directive.
    (ok (signals (config:parse-config '(:mallet-config (:preset :default)))
                 'error)
        ":preset is silently ignored today — must be rejected after the fix"))

  (testing "error message for :preset names the offending directive"
    (handler-case
        (progn
          (config:parse-config '(:mallet-config (:preset :default)))
          (ok nil "No error was signaled for unknown directive :preset"))
      (error (e)
        (let ((msg (format nil "~A" e)))
          (ok (or (search "PRESET" msg)
                  (search "preset" msg))
              "Error message must identify the unknown directive :preset")))))

  (testing "other unknown directives are also rejected"
    ;; Ensures the fix handles unknown keys in general, not just :preset.
    (ok (signals (config:parse-config '(:mallet-config (:bad-directive)))
                 'error)
        "Unknown directives in general must be rejected")))

;;; AC3: A malformed .mallet.lisp must surface as errors:config-parse-failed
;;; whose formatted report names the file path and contains no SBCL object
;;; representations (e.g. #<SB-SYS:FD-STREAM ... {hex}>).
;;;
;;; Currently load-config does not catch reader errors; they propagate to
;;; main's generic handler which formats them with "Fatal error: ~A", leaking
;;; SBCL internals such as the FD-STREAM object.

(deftest load-config-malformed-signals-config-parse-failed
  (testing "unbalanced parens → config-parse-failed (not raw reader-error)"
    (with-temporary-config ("(:mallet-config (" path)
      (handler-case
          (progn
            (config:load-config path)
            (ok nil "load-config must signal an error for malformed input"))
        (errors:config-parse-failed ()
          (ok t "Correctly signals config-parse-failed for unbalanced parens"))
        (error (e)
          (ok nil
              (format nil "Expected config-parse-failed; got ~A: ~A"
                      (type-of e) e))))))

  (testing "#. form → config-parse-failed (not raw reader-error)"
    ;; *read-eval* is nil in read-mallet-forms, so #. triggers a reader-error.
    ;; That error must be wrapped into config-parse-failed, not propagated raw.
    (with-temporary-config ("(:mallet-config #.(error \"x\"))" path)
      (handler-case
          (progn
            (config:load-config path)
            (ok nil "load-config must signal an error for #. in config"))
        (errors:config-parse-failed ()
          (ok t "Correctly signals config-parse-failed for #. form"))
        (error (e)
          (ok nil
              (format nil "Expected config-parse-failed; got ~A: ~A"
                      (type-of e) e)))))))

(deftest load-config-malformed-error-message-format
  (testing "error message contains the config file path"
    (with-temporary-config ("(:mallet-config (" path)
      (handler-case
          (progn
            (config:load-config path)
            (ok nil "Must have signaled an error"))
        (errors:config-parse-failed (e)
          (ok (search (namestring path) (format nil "~A" e))
              "Error message must name the config file path"))
        (error ()
          (ok nil "Expected config-parse-failed but got a different error")))))

  (testing "error message contains no SBCL FD-STREAM object repr (#<SB-)"
    (with-temporary-config ("(:mallet-config (" path)
      (handler-case
          (progn
            (config:load-config path)
            (ok nil "Must have signaled an error"))
        (errors:config-parse-failed (e)
          (ok (null (search "#<SB-" (format nil "~A" e)))
              "Error message must not contain SBCL internal object representations"))
        (error ()
          (ok nil "Expected config-parse-failed but got a different error")))))

  (testing "error message contains no brace-delimited hex address"
    ;; SBCL formats object addresses as {XXXXXXXXXX} in condition reports.
    (with-temporary-config ("(:mallet-config (" path)
      (handler-case
          (progn
            (config:load-config path)
            (ok nil "Must have signaled an error"))
        (errors:config-parse-failed (e)
          (ok (null (ppcre:scan "\\{[0-9A-Fa-f]{6,}\\}" (format nil "~A" e)))
              "Error message must not contain brace-delimited hex addresses"))
        (error ()
          (ok nil "Expected config-parse-failed but got a different error"))))))

;;; AC1 — load-config path: unknown directive must surface as config-parse-failed.
;;;
;;; parse-config-rejects-unknown-directives (above) tests at the parse-config
;;; API level.  Here we test the full load-config call path — the condition
;;; that main actually encounters — so that main uses "Error: ..." (the
;;; cli-error handler) rather than "Fatal error: ..." (the generic handler).

(deftest load-config-unknown-directive-signals-config-parse-failed
  (testing "unknown directive :preset via load-config → config-parse-failed"
    ;; Currently load-config returns a config with no rules (silently), so
    ;; this test FAILS until the fix wraps the unknown-directive error.
    (with-temporary-config ("(:mallet-config (:preset :default))" path)
      (handler-case
          (progn
            (config:load-config path)
            (ok nil "load-config must signal an error for unknown directive :preset"))
        (errors:config-parse-failed ()
          (ok t "Correctly signals config-parse-failed for unknown directive :preset"))
        (error (e)
          (ok nil
              (format nil "Expected config-parse-failed; got ~A: ~A"
                      (type-of e) e))))))

  (testing "config-parse-failed for unknown directive names the config file path"
    (with-temporary-config ("(:mallet-config (:preset :default))" path)
      (handler-case
          (progn
            (config:load-config path)
            (ok nil "Must have signaled an error for unknown directive :preset"))
        (errors:config-parse-failed (e)
          (ok (search (namestring path) (format nil "~A" e))
              "Error message must contain the config file path"))
        (error ()
          (ok nil "Expected config-parse-failed but got a different error")))))

  (testing "config-parse-failed for unknown directive names the offending directive :preset"
    ;; AC1 requires that the error message names the directive, not just the path.
    ;; The cause text from unknown-config-directive formats as
    ;;   "Unknown config directive: :PRESET"
    ;; which must appear in the config-parse-failed report.
    (with-temporary-config ("(:mallet-config (:preset :default))" path)
      (handler-case
          (progn
            (config:load-config path)
            (ok nil "Must have signaled an error for unknown directive :preset"))
        (errors:config-parse-failed (e)
          (let ((msg (format nil "~A" e)))
            (ok (or (search "PRESET" msg)
                    (search "preset" msg))
                "Error message must name the unknown directive :preset")))
        (error ()
          (ok nil "Expected config-parse-failed but got a different error"))))))

;;; AC1 — "lint does not run" guard: the unknown-directive error must prevent
;;; the engine from running at all, not just change the exit code.
;;;
;;; Before the fix, parse-config silently ignored :preset, returning an empty
;;; config (0 rules).  load-config succeeded, lint-file ran with 0 rules, found
;;; 0 violations, and the caller printed "✓ No problems found." — a false clean.
;;; After the fix, load-config raises config-parse-failed and the lint never runs.

(deftest unknown-directive-prevents-lint
  (testing ":preset config must raise an error, not silently produce a clean result"
    (with-temporary-config ("(:mallet-config (:preset :default))" cfg-path)
      (uiop:with-temporary-file (:stream code-out :pathname code-file
                                 :direction :output :type "lisp")
        ;; File has trailing whitespace — detected when default rules are active,
        ;; but invisible when lint runs with 0 rules (the pre-fix silent-clean case).
        (write-string "(defun foo () nil)   " code-out)
        (terpri code-out)
        (force-output code-out)
        (handler-case
            (let ((cfg (config:load-config cfg-path)))
              ;; Reached only in the broken (pre-fix) case where load-config
              ;; returns an empty config instead of signaling an error.
              (let ((violations (engine:lint-file code-file :config cfg)))
                (ok nil
                    (format nil
                            "Silent clean run: load-config returned a config with ~A rules; ~
lint found ~A violations — expected a config-parse-failed error, not a clean result"
                            (length (config:config-rules cfg))
                            (length violations)))))
          (errors:config-parse-failed ()
            (ok t "config-parse-failed raised — lint never ran, no silent clean result")))))))

;;; AC2 — regression guard: a valid config with (:extends :default) must still
;;; load the default preset and actually catch trailing whitespace.
;;;
;;; The config-extends tests in config-test.lisp verify rule membership, but
;;; do not exercise the engine; this test closes that gap.

(deftest extends-default-config-detects-trailing-whitespace
  (testing "(:extends :default) config loads default preset"
    (with-temporary-config ("(:mallet-config (:extends :default))" cfg-path)
      (let ((cfg (config:load-config cfg-path)))
        (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
          (ok (member :trailing-whitespace rule-names)
              ":trailing-whitespace must be present in the default preset")))))

  (testing "(:extends :default) config detects trailing whitespace via engine"
    (with-temporary-config ("(:mallet-config (:extends :default))" cfg-path)
      (let ((cfg (config:load-config cfg-path)))
        (uiop:with-temporary-file (:stream code-out :pathname code-file
                                   :direction :output :type "lisp")
          (write-string "(defun foo () nil)   " code-out)
          (terpri code-out)
          (force-output code-out)
          (let ((violations (engine:lint-file code-file :config cfg)))
            (ok (some (lambda (v)
                        (eq :trailing-whitespace (violation:violation-rule v)))
                      violations)
                "File with trailing whitespace must produce a :trailing-whitespace violation")))))))
