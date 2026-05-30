(defpackage #:mallet/tests/format-stream-separation
  (:use #:cl #:rove))
(in-package #:mallet/tests/format-stream-separation)

(defun read-remaining-from-stream (stream)
  "Return all characters left in STREAM as a string."
  (let ((out (make-string-output-stream)))
    (loop for c = (read-char stream nil nil)
          while c do (write-char c out))
    (get-output-stream-string out)))

(defun json-consumed-fully-p (string)
  "Return T iff STRING is fully consumed by the JSON parser with no trailing non-whitespace."
  (let ((stream (make-string-input-stream string)))
    (handler-case
      (progn
        (cl-json:decode-json stream)
        (uiop:emptyp (string-trim '(#\Space #\Newline #\Tab #\Return)
                                  (read-remaining-from-stream stream))))
      (error () nil))))

;;; Tests that --format line and --format json keep stdout clean.
;;; The human summary belongs on stderr; only structured records belong on stdout.

(defun project-root ()
  (asdf:system-source-directory :mallet))

(defun binary ()
  (namestring (merge-pathnames "bin/mallet" (project-root))))

(defun fixture (relative-path)
  (namestring (merge-pathnames relative-path (project-root))))

(defun run-cli (args)
  "Run mallet CLI with ARGS, returning (values stdout-string stderr-string)."
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (uiop:run-program args
                      :output out
                      :error-output err
                      :ignore-error-status t)
    (values (get-output-stream-string out)
            (get-output-stream-string err))))

;;; The violations directory is the canonical argument for directory-mode tests.
;;; Using a single .lisp file is not permitted by the acceptance criteria because
;;; a violating implementation can produce clean stdout for a single file while
;;; still emitting a summary line or blank lines for directory input.
(defparameter +violations-dir+ "tests/fixtures/violations/"
  "Directory fixture for directory-mode tests.")

(defparameter +clean-file+ "tests/fixtures/clean/basic.lisp"
  "Fixture that produces no violations with any preset.")

;;; --format line (directory invocation)

(deftest line-format-stdout-clean-with-violations
  "With --format line (directory input), stdout must contain only file:line:col: records — no summary."
  (multiple-value-bind (stdout stderr)
      (run-cli (list (binary)
                     "--format" "line"
                     (fixture +violations-dir+)))

    ;; Sanity: at least one violation line must be present on stdout.
    (ok (cl-ppcre:scan ":[0-9]+:[0-9]+:" stdout)
        "stdout should contain at least one violation record")

    ;; Summary text must NOT appear on stdout — catches an implementation that embeds
    ;; the count in a line-shaped record such as "summary:0:0: info: 47 problems found".
    (ng (cl-ppcre:scan "[0-9]+ problem" stdout)
        "summary must not appear on stdout")

    ;; Summary text MUST appear on stderr with the count.
    (ok (cl-ppcre:scan "[0-9]+ problem" stderr)
        "summary on stderr must include the violation count")))

(deftest line-format-every-stdout-line-is-record
  "With --format line (directory input), every stdout line must match file:line:col: severity: message shape — no blank lines."
  (multiple-value-bind (stdout _stderr)
      (run-cli (list (binary)
                     "--format" "line"
                     (fixture +violations-dir+)))
    (declare (ignore _stderr))

    ;; Split on newlines, then remove only the single trailing empty token that
    ;; results from the natural terminal newline of the last record.  Do NOT
    ;; remove any other empty strings: a leading or mid-stream blank line from
    ;; a violating implementation produces an empty string that will not match
    ;; the record regex below, so the test correctly catches it.
    (let* ((all-lines (uiop:split-string stdout :separator '(#\Newline)))
           (lines (if (and all-lines (string= (car (last all-lines)) ""))
                      (butlast all-lines)
                      all-lines)))
      ;; Sanity: directory input must produce at least one record.
      (ok lines "stdout must contain at least one violation line")

      ;; The regex requires severity AND message after the prefix.
      ;; Empty strings (blank lines) also fail this check.
      (ok (every (lambda (line)
                   (cl-ppcre:scan "^[^:]+:[0-9]+:[0-9]+: (warning|error|info): .+" line))
                 lines)
          "every stdout line must match file:line:col: severity: message shape"))))

(deftest line-format-no-violations-summary-on-stderr
  "With --format line and no violations, the no-problems message goes to stderr."
  (multiple-value-bind (stdout stderr)
      (run-cli (list (binary)
                     "--format" "line"
                     "--no-color"
                     (fixture +clean-file+)))

    ;; stdout must be empty — currently it has "No problems found.".
    (ok (uiop:emptyp (string-trim '(#\Space #\Newline) stdout))
        "stdout must be empty when there are no violations")

    ;; The no-problems message must appear on stderr.
    (ok (search "No problems" stderr)
        "no-problems message must appear on stderr")))

;;; --format json (directory invocation)

(deftest json-format-stdout-is-valid-json-with-violations
  "With --format json (directory input), stdout must be a single self-contained JSON document."
  (multiple-value-bind (stdout stderr)
      (run-cli (list (binary)
                     "--format" "json"
                     (fixture +violations-dir+)))

    ;; Summary text must NOT appear on stdout — catches an implementation that embeds
    ;; the count as a JSON field such as {"summary": "47 problems found"}.
    (ng (cl-ppcre:scan "[0-9]+ problem" stdout)
        "summary must not appear on stdout")

    ;; Parse the complete stdout as JSON and verify nothing trails the document.
    ;; cl-json:decode-json-from-string alone silently ignores trailing garbage such as
    ;; "[] trailing"; json-consumed-fully-p reads from a stream and asserts nothing
    ;; non-whitespace remains after the JSON document ends.
    (ok (json-consumed-fully-p stdout)
        "stdout must be a complete JSON document with no trailing content")

    ;; Summary on stderr MUST include the violation count.
    (ok (cl-ppcre:scan "[0-9]+ problem" stderr)
        "summary on stderr must include the violation count")))

(deftest json-format-no-violations-summary-on-stderr
  "With --format json and no violations, the no-problems message goes to stderr."
  (multiple-value-bind (stdout stderr)
      (run-cli (list (binary)
                     "--format" "json"
                     "--no-color"
                     (fixture +clean-file+)))

    ;; stdout must still be a valid JSON array (empty).
    (ok (handler-case (progn (cl-json:decode-json-from-string stdout) t)
          (error () nil))
        "stdout must be parseable as a JSON document (empty array)")

    ;; No-problems text must NOT appear on stdout.
    (ng (search "No problems" stdout)
        "no-problems message must not appear on stdout")

    ;; No-problems text MUST appear on stderr — fails before the fix.
    (ok (search "No problems" stderr)
        "no-problems message must appear on stderr")))
