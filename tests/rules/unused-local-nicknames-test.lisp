(defpackage #:malo/tests/rules/unused-local-nicknames
  (:use #:cl #:rove)
  (:local-nicknames
   (#:rules #:malo/rules)
   (#:parser #:malo/parser)
   (#:violation #:malo/violation)))
(in-package #:malo/tests/rules/unused-local-nicknames)

(defmacro with-test-file ((tmpfile-var code) &body body)
  "Helper macro to create temporary file with CODE and clean up after."
  `(uiop:with-temporary-file (:stream stream :pathname ,tmpfile-var
                                      :type "lisp" :keep t)
     (write-string ,code stream)
     (finish-output stream)
     ,@body))

(deftest unused-local-nicknames-valid
  (testing "Valid: all local nicknames are used"
    (with-test-file (tmpfile "(defpackage #:test-package
                                 (:use #:cl)
                                 (:local-nicknames
                                  (#:a #:alexandria)
                                  (#:p #:parser))
                                 (:export #:foo))
                               (in-package #:test-package)
                               (defun foo (x)
                                 (a:if-let ((y (+ x 1)))
                                   (p:parse y)
                                   0))")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-local-nicknames-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        (ok (null violations)))))

  (testing "Valid: package-only file (no code after in-package)"
    (with-test-file (tmpfile "(defpackage #:test-package
                                 (:use #:cl)
                                 (:local-nicknames
                                  (#:a #:alexandria)
                                  (#:p #:parser))
                                 (:export #:foo))
                               (in-package #:test-package)")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-local-nicknames-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        ;; Should not report violations for package-only files
        (ok (null violations)))))

  (testing "Valid: no local nicknames defined"
    (with-test-file (tmpfile "(defpackage #:test-package
                                 (:use #:cl)
                                 (:export #:foo))
                               (in-package #:test-package)
                               (defun foo () 42)")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-local-nicknames-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        (ok (null violations))))))

(deftest unused-local-nicknames-invalid
  (testing "Invalid: unused local nickname"
    (with-test-file (tmpfile "(defpackage #:test-package
                                 (:use #:cl)
                                 (:local-nicknames
                                  (#:a #:alexandria)
                                  (#:p #:parser))
                                 (:export #:foo))
                               (in-package #:test-package)
                               (defun foo (x)
                                 (a:if-let ((y (+ x 1)))
                                   y
                                   0))")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-local-nicknames-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        ;; Should report 'p' as unused
        (ok (= (length violations) 1))
        (ok (eq (violation:violation-rule (first violations)) :unused-local-nicknames))
        (ok (search "Local nickname 'p'" (violation:violation-message (first violations))))
        (ok (search "parser" (violation:violation-message (first violations)))))))

  (testing "Invalid: multiple unused local nicknames"
    (with-test-file (tmpfile "(defpackage #:test-package
                                 (:use #:cl)
                                 (:local-nicknames
                                  (#:a #:alexandria)
                                  (#:p #:parser)
                                  (#:u #:uiop))
                                 (:export #:foo))
                               (in-package #:test-package)
                               (defun foo (x)
                                 (a:if-let ((y (+ x 1)))
                                   y
                                   0))")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-local-nicknames-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        ;; Should report 'p' and 'u' as unused
        (ok (= (length violations) 2))
        (ok (every (lambda (v) (eq (violation:violation-rule v) :unused-local-nicknames))
                   violations)))))

  (testing "Invalid: all local nicknames unused"
    (with-test-file (tmpfile "(defpackage #:test-package
                                 (:use #:cl)
                                 (:local-nicknames
                                  (#:a #:alexandria)
                                  (#:p #:parser))
                                 (:export #:foo))
                               (in-package #:test-package)
                               (defun foo (x)
                                 (+ x 1))")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-local-nicknames-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        ;; Should report both 'a' and 'p' as unused
        (ok (= (length violations) 2))))))

(deftest unused-local-nicknames-severity
  (testing "Rule has :info severity"
    (let ((rule (make-instance 'rules:unused-local-nicknames-rule)))
      (ok (eq (rules:rule-severity rule) :info)))))
