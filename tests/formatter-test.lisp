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
                              :rule :if-without-else
                              :severity :convention
                              :line 20
                              :column 0
                              :message "Convention message"
                              :file file))
           (counts (formatter:format-line-file
                    file
                    (list v1 v2 v3)
                    :stream (make-string-output-stream))))
      ;; Check severity counts in returned plist
      (ok (= (getf counts :warning) 1))
      (ok (= (getf counts :error) 1))
      (ok (= (getf counts :convention) 1))))

  (testing "format-line-file with fixed violations"
    (let* ((file (pathname "/path/to/file.lisp"))
           (v1 (make-instance 'violation:violation
                              :rule :trailing-whitespace
                              :severity :format
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
      ;; Check JSON structure
      (ok (search "\"file\":" output))
      (ok (search "\"/path/to/file.lisp\"" output))
      (ok (search "\"violations\":" output))
      (ok (search "\"rule\": \"unused-variables\"" output))
      (ok (search "\"severity\": \"warning\"" output))
      (ok (search "\"line\": 10" output))
      (ok (search "\"column\": 5" output))
      (ok (search "\"message\": \"Variable 'x' is unused\"" output)))))
