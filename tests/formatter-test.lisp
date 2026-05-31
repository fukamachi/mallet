(defpackage #:mallet/tests/formatter
  (:use #:cl #:rove)
  (:local-nicknames
   (#:formatter #:mallet/formatter)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/formatter)

(deftest format-line-file-test
  (testing "format-line-file with violations"
    (let* ((file (pathname "/path/to/file.lisp"))
           (v1 (make-instance 'violation:violation
                              :rule :unused-variables
                              :severity :warning
                              :line 10
                              :column 5
                              :message "Variable 'x' is unused"
                              :file file))
           (v2 (make-instance 'violation:violation
                              :rule :wrong-otherwise
                              :severity :error
                              :line 15
                              :column 2
                              :message "'ecase' should not have 'otherwise' clause"
                              :file file))
           (output (with-output-to-string (stream)
                     (formatter:format-line-file
                      file
                      (list v1 v2)
                      :stream stream))))
      ;; Check format: path:line:col: severity: message [rule]
      (ok (search "/path/to/file.lisp:10:5:" output))
      (ok (search "warning:" output))
      (ok (search "Variable 'x' is unused" output))
      (ok (search "[unused-variables]" output))

      (ok (search "/path/to/file.lisp:15:2:" output))
      (ok (search "error:" output))
      (ok (search "'ecase' should not have 'otherwise' clause" output))
      (ok (search "[wrong-otherwise]" output))))

  (testing "format-line-file with no violations"
    (let* ((file (pathname "/path/to/file.lisp"))
           (output (with-output-to-string (stream)
                     (formatter:format-line-file
                      file
                      '()
                      :stream stream))))
      ;; No output for files without violations
      (ok (string= output ""))))

  (testing "format-line-file returns severity counts"
    (let* ((file (pathname "/path/to/file.lisp"))
           (v1 (make-instance 'violation:violation
                              :rule :unused-variables
                              :severity :warning
                              :line 10
                              :column 5
                              :message "Warning message"
                              :file file))
           (v2 (make-instance 'violation:violation
                              :rule :wrong-otherwise
                              :severity :error
                              :line 15
                              :column 2
                              :message "Error message"
                              :file file))
           (v3 (make-instance 'violation:violation
                              :rule :missing-else
                              :severity :warning
                              :line 20
                              :column 0
                              :message "Warning message"
                              :file file))
           (counts (formatter:format-line-file
                    file
                    (list v1 v2 v3)
                    :stream (make-string-output-stream))))
      ;; Check severity counts in returned plist
      (ok (= (getf counts :warning) 2))
      (ok (= (getf counts :error) 1))))

  (testing "format-line-file with fixed violations"
    (let* ((file (pathname "/path/to/file.lisp"))
           (v1 (make-instance 'violation:violation
                              :rule :trailing-whitespace
                              :severity :warning
                              :line 10
                              :column 5
                              :message "Trailing whitespace"
                              :file file))
           (output (with-output-to-string (stream)
                     (formatter:format-line-file
                      file
                      (list v1)
                      :stream stream
                      :fixed-violations (list v1)))))
      ;; Check that fixed violations are marked as [FIXED]
      (ok (search "[FIXED]" output))
      (ok (not (search "format:" output))))))

(deftest no-color-test
  (testing "use-colors-p returns nil when *no-color* is true"
    (let ((formatter:*no-color* t))
      (ok (null (formatter:use-colors-p *standard-output*)))))

  (testing "*no-color* defaults to nil"
    (ok (null formatter:*no-color*))))

(deftest format-text-file-test
  (testing "format-text-file groups violations by file"
    (let* ((file (pathname "/path/to/file.lisp"))
           (v1 (make-instance 'violation:violation
                              :rule :unused-variables
                              :severity :warning
                              :line 10
                              :column 5
                              :message "Variable 'x' is unused"
                              :file file))
           (output (with-output-to-string (stream)
                     (formatter:format-text-file
                      file
                      (list v1)
                      :stream stream))))
      ;; Check that file name is printed as header
      (ok (search "/path/to/file.lisp" output))
      ;; Check that violation is printed with location
      (ok (search "10:5" output))
      (ok (search "warning" output))
      (ok (search "Variable 'x' is unused" output)))))

(defun json-field (object field-name)
  "Return FIELD-NAME from a CL-JSON decoded alist."
  (cdr (assoc field-name object
              :test (lambda (expected actual)
                      (string= expected (string-downcase (string actual)))))))

(deftest format-json-file-test
  (testing "format-json-file outputs JSON structure"
    (let* ((file (pathname "/path/to/file.lisp"))
           (v1 (make-instance 'violation:violation
                              :rule :unused-variables
                              :severity :warning
                              :line 10
                              :column 5
                              :message "Variable 'x' is unused"
                              :file file))
           (output (with-output-to-string (stream)
                     (formatter:format-json-file
                      file
                      (list v1)
                      t  ; first file
                      :stream stream))))
      (let* ((decoded (cl-json:decode-json-from-string output))
             (violation (first (json-field decoded "violations"))))
        (ok (string= "/path/to/file.lisp" (json-field decoded "file")))
        (ok (string= "unused-variables" (json-field violation "rule")))
        (ok (string= "warning" (json-field violation "severity")))
        (ok (= 10 (json-field violation "line")))
        (ok (= 5 (json-field violation "column")))
        (ok (string= "Variable 'x' is unused" (json-field violation "message")))
        (ok (null (json-field violation "category"))))))

  (testing "format-json-file includes category when present"
    (let* ((file (pathname "/path/to/file.lisp"))
           (v1 (make-instance 'violation:violation
                              :rule :line-length
                              :severity :warning
                              :line 5
                              :column 0
                              :category :style
                              :message "Line too long"
                              :file file))
           (output (with-output-to-string (stream)
                     (formatter:format-json-file
                      file
                      (list v1)
                      t
                      :stream stream))))
      (let* ((decoded (cl-json:decode-json-from-string output))
             (violation (first (json-field decoded "violations"))))
        (ok (string= "style" (json-field violation "category")))))))

(deftest format-with-info-severity
  (testing "format-line-file with :info severity"
    (let* ((file (pathname "/path/to/file.lisp"))
           (v1 (make-instance 'violation:violation
                              :rule :cyclomatic-complexity
                              :severity :info
                              :line 42
                              :column 0
                              :message "Function is too complex"
                              :file file))
           (output (with-output-to-string (stream)
                     (formatter:format-line-file
                      file
                      (list v1)
                      :stream stream))))
      (ok (search "info:" output))
      (ok (search "Function is too complex" output))
      (ok (search "[cyclomatic-complexity]" output))))

  (testing "format-line-file returns :info count"
    (let* ((file (pathname "/path/to/file.lisp"))
           (v1 (make-instance 'violation:violation
                              :rule :line-length
                              :severity :info
                              :line 10
                              :column 0
                              :message "Line too long"
                              :file file))
           (counts (formatter:format-line-file
                    file
                    (list v1)
                    :stream (make-string-output-stream))))
      (ok (= (getf counts :info) 1))
      (ok (null (getf counts :error)))
      (ok (null (getf counts :warning)))))

  (testing "format-text-file with :info severity"
    (let* ((file (pathname "/path/to/file.lisp"))
           (v1 (make-instance 'violation:violation
                              :rule :line-length
                              :severity :info
                              :line 99
                              :column 0
                              :message "Line too long"
                              :file file))
           (output (with-output-to-string (stream)
                     (formatter:format-text-file
                      file
                      (list v1)
                      :stream stream))))
      (ok (search "info" output))
      (ok (search "Line too long" output)))))
