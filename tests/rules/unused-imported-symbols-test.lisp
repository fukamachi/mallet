(defpackage #:mallet/tests/rules/unused-imported-symbols
  (:use #:cl #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/unused-imported-symbols)

(defmacro with-test-file ((tmpfile-var code) &body body)
  "Helper macro to create temporary file with CODE and clean up after."
  `(uiop:with-temporary-file (:stream stream :pathname ,tmpfile-var
                              :type "lisp" :keep t)
     (write-string ,code stream)
     (finish-output stream)
     ,@body))

(deftest unused-imported-symbols-valid
  (testing "Valid: all imported symbols are used"
    (with-test-file (tmpfile "(defpackage #:test-package
                                 (:use #:cl)
                                 (:import-from #:some-package
                                  #:imported-symbol
                                  #:another-symbol)
                                 (:export #:foo))
                               (in-package #:test-package)
                               (defun foo ()
                                 (imported-symbol))
                               (defun bar ()
                                 (another-symbol))")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-imported-symbols-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        (ok (null violations)))))

  (testing "Valid: imported symbol is re-exported"
    (with-test-file (tmpfile "(defpackage #:test-package
                                 (:use #:cl)
                                 (:import-from #:some-package
                                  #:imported-symbol
                                  #:another-symbol)
                                 (:export #:imported-symbol))
                               (in-package #:test-package)
                               (defun foo ()
                                 (another-symbol))")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-imported-symbols-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        ;; Should not report 'imported-symbol' as unused because it's re-exported
        (ok (null violations)))))

  (testing "Valid: package-only file (no code after in-package)"
    (with-test-file (tmpfile "(defpackage #:test-package
                                 (:use #:cl)
                                 (:import-from #:some-package
                                  #:imported-symbol
                                  #:another-symbol)
                                 (:export #:foo))
                               (in-package #:test-package)")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-imported-symbols-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        ;; Should not report violations for package-only files
        (ok (null violations)))))

  (testing "Valid: no imported symbols"
    (with-test-file (tmpfile "(defpackage #:test-package
                                 (:use #:cl)
                                 (:export #:foo))
                               (in-package #:test-package)
                               (defun foo () 42)")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-imported-symbols-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        (ok (null violations))))))

(deftest unused-imported-symbols-invalid
  (testing "Invalid: unused imported symbol"
    (with-test-file (tmpfile "(defpackage #:test-package
                                 (:use #:cl)
                                 (:import-from #:some-package
                                  #:imported-symbol
                                  #:another-symbol)
                                 (:export #:foo))
                               (in-package #:test-package)
                               (defun foo ()
                                 (imported-symbol))")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-imported-symbols-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        ;; Should report 'another-symbol' as unused
        (ok (= (length violations) 1))
        (ok (eq (violation:violation-rule (first violations)) :unused-imported-symbols))
        (ok (search "Imported symbol 'another-symbol'" (violation:violation-message (first violations))))
        (ok (search "some-package" (violation:violation-message (first violations)))))))

  (testing "Invalid: multiple unused imported symbols from one package"
    (with-test-file (tmpfile "(defpackage #:test-package
                                 (:use #:cl)
                                 (:import-from #:some-package
                                  #:imported-symbol
                                  #:another-symbol
                                  #:third-symbol)
                                 (:export #:foo))
                               (in-package #:test-package)
                               (defun foo ()
                                 (imported-symbol))")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-imported-symbols-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        ;; Should report 'another-symbol' and 'third-symbol' as unused
        (ok (= (length violations) 2))
        (ok (every (lambda (v) (eq (violation:violation-rule v) :unused-imported-symbols))
                   violations)))))

  (testing "Invalid: unused imports from multiple packages"
    (with-test-file (tmpfile "(defpackage #:test-package
                                 (:use #:cl)
                                 (:import-from #:some-package
                                  #:imported-symbol
                                  #:another-symbol)
                                 (:import-from #:other-package
                                  #:other-symbol)
                                 (:export #:foo))
                               (in-package #:test-package)
                               (defun foo ()
                                 (imported-symbol))")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-imported-symbols-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        ;; Should report 'another-symbol' and 'other-symbol' as unused
        (ok (= (length violations) 2))
        (ok (every (lambda (v) (eq (violation:violation-rule v) :unused-imported-symbols))
                   violations)))))

  (testing "Invalid: all imported symbols unused (none re-exported)"
    (with-test-file (tmpfile "(defpackage #:test-package
                                 (:use #:cl)
                                 (:import-from #:some-package
                                  #:imported-symbol
                                  #:another-symbol)
                                 (:export #:foo))
                               (in-package #:test-package)
                               (defun foo ()
                                 42)")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-imported-symbols-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        ;; Should report both symbols as unused
        (ok (= (length violations) 2))))))

(deftest unused-imported-symbols-severity
  (testing "Rule has :info severity"
    (let ((rule (make-instance 'rules:unused-imported-symbols-rule)))
      (ok (eq (rules:rule-severity rule) :info)))))
