(defpackage #:malo/tests/errors
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:errors #:malo/errors)))
(in-package #:malo/tests/errors)

(deftest error-condition-hierarchy
  (testing "CLI errors are of type cli-error"
    (ok (typep (make-condition 'errors:unknown-option :option "--foo")
               'errors:cli-error))
    (ok (typep (make-condition 'errors:missing-option-value :option "--foo")
               'errors:cli-error))
    (ok (typep (make-condition 'errors:invalid-option-value
                               :option "--foo"
                               :value "bar"
                               :expected "baz")
               'errors:cli-error))
    (ok (typep (make-condition 'errors:file-not-found :path "foo.lisp")
               'errors:cli-error))
    (ok (typep (make-condition 'errors:no-files-specified)
               'errors:cli-error)))

  (testing "CLI errors are of type malo-error"
    (ok (typep (make-condition 'errors:unknown-option :option "--foo")
               'errors:malo-error)))

  (testing "All CLI errors are of type error"
    (ok (typep (make-condition 'errors:unknown-option :option "--foo")
               'error))))

(deftest error-reporting
  (testing "unknown-option has helpful message"
    (let ((err (make-condition 'errors:unknown-option :option "--foo")))
      (ok (search "Unknown option: --foo" (format nil "~A" err)))
      (ok (search "malo --help" (format nil "~A" err)))))

  (testing "missing-option-value has clear message"
    (let ((err (make-condition 'errors:missing-option-value :option "--format")))
      (ok (search "Missing value for option: --format" (format nil "~A" err)))))

  (testing "invalid-format has specific message"
    (let ((err (make-condition 'errors:invalid-format
                               :option "--format"
                               :value "xml"
                               :expected "text or json")))
      (ok (search "Invalid format: xml" (format nil "~A" err)))
      (ok (search "text or json" (format nil "~A" err)))))

  (testing "invalid-preset has specific message"
    (let ((err (make-condition 'errors:invalid-preset
                               :option "--preset"
                               :value "bad"
                               :expected "default or all")))
      (ok (search "Invalid preset: bad" (format nil "~A" err)))
      (ok (search "default or all" (format nil "~A" err)))))

  (testing "file-not-found has clear message"
    (let ((err (make-condition 'errors:file-not-found :path "missing.lisp")))
      (ok (search "File not found: missing.lisp" (format nil "~A" err)))))

  (testing "no-files-specified has helpful message"
    (let ((err (make-condition 'errors:no-files-specified)))
      (ok (search "No files specified" (format nil "~A" err)))
      (ok (search "malo --help" (format nil "~A" err))))))

(deftest error-accessors
  (testing "unknown-option has option accessor"
    (let ((err (make-condition 'errors:unknown-option :option "--test")))
      (ok (equal (errors:unknown-option-option err) "--test"))))

  (testing "missing-option-value has option accessor"
    (let ((err (make-condition 'errors:missing-option-value :option "--config")))
      (ok (equal (errors:missing-option-value-option err) "--config"))))

  (testing "invalid-option-value has accessors"
    (let ((err (make-condition 'errors:invalid-option-value
                               :option "--format"
                               :value "bad"
                               :expected "good")))
      (ok (equal (errors:invalid-option-value-option err) "--format"))
      (ok (equal (errors:invalid-option-value-value err) "bad"))
      (ok (equal (errors:invalid-option-value-expected err) "good"))))

  (testing "file-not-found has path accessor"
    (let ((err (make-condition 'errors:file-not-found :path "test.lisp")))
      (ok (equal (errors:file-not-found-path err) "test.lisp"))))

  (testing "config-not-found has path accessor"
    (let ((err (make-condition 'errors:config-not-found :path ".malo.lisp")))
      (ok (equal (errors:config-not-found-path err) ".malo.lisp"))))

  (testing "config-parse-failed has accessors"
    (let ((err (make-condition 'errors:config-parse-failed
                               :path ".malo.lisp"
                               :cause "Invalid syntax")))
      (ok (equal (errors:config-parse-failed-path err) ".malo.lisp"))
      (ok (equal (errors:config-parse-failed-cause err) "Invalid syntax")))))
