(defpackage #:mallet/tests/rules/one-package-per-file
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/one-package-per-file)

(defun check-one-package-per-file (code &optional (filename "test.lisp"))
  "Run one-package-per-file-rule on CODE parsed as FILENAME."
  (let ((file (pathname filename))
        (rule (make-instance 'rules:one-package-per-file-rule)))
    (mapcan (lambda (form)
              (rules:check-form rule form file))
            (parser:parse-forms code file))))

;;; Valid cases (no violations)

(deftest one-package-per-file-valid
  (testing "file with defpackage and in-package is not flagged"
    (ok (null (check-one-package-per-file
               "(defpackage #:my-pkg (:use #:cl))
(in-package #:my-pkg)"))))

  (testing "file with only defpackage and no in-package is not flagged"
    (ok (null (check-one-package-per-file
               "(defpackage #:my-pkg (:use #:cl))"))))

  (testing "file with no forms is not flagged"
    (ok (null (check-one-package-per-file ""))))

  (testing "file with uiop:define-package and in-package is not flagged"
    (ok (null (check-one-package-per-file
               "(uiop:define-package #:my-pkg (:use #:cl))
(in-package #:my-pkg)"))))

  (testing "file with defpackage before in-package and code is not flagged"
    (ok (null (check-one-package-per-file
               "(defpackage #:my-pkg (:use #:cl))
(in-package #:my-pkg)
(defun foo () 42)")))))

;;; Invalid cases (violations expected)

(deftest one-package-per-file-invalid
  (testing "file with in-package but no defpackage is flagged"
    (let ((violations (check-one-package-per-file
                       "(in-package #:cl-user)")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :one-package-per-file))
      (ok (eq (violation:violation-severity (first violations)) :warning))))

  (testing "violation message mentions defpackage"
    (let ((violations (check-one-package-per-file
                       "(in-package #:cl-user)")))
      (ok (search "defpackage" (violation:violation-message (first violations))))))

  (testing "violation is on the in-package line"
    (let ((violations (check-one-package-per-file
                       "; comment
(in-package #:cl-user)")))
      (ok (= (violation:violation-line (first violations)) 2))))

  (testing "multiple in-package forms without defpackage each produce a violation"
    (let ((violations (check-one-package-per-file
                       "(in-package #:cl-user)
(in-package #:cl)")))
      (ok (= (length violations) 2)))))

;;; Caching behavior

(deftest one-package-per-file-caching
  (testing "rule instance is reusable across invocations"
    ;; Verify that the cached-file mechanism works: create one rule,
    ;; run it on two different virtual filenames.
    (let* ((rule (make-instance 'rules:one-package-per-file-rule))
           (file1 #p"test1.lisp")
           (file2 #p"test2.lisp")
           (forms1 (parser:parse-forms "(in-package #:cl-user)" file1))
           (forms2 (parser:parse-forms "(in-package #:cl-user)" file2))
           ;; First file: no defpackage → violation
           (v1 (mapcan (lambda (f) (rules:check-form rule f file1)) forms1))
           ;; Second file: same code, different filename → should also be detected
           (v2 (mapcan (lambda (f) (rules:check-form rule f file2)) forms2)))
      (ok (= (length v1) 1))
      (ok (= (length v2) 1)))))
