(defpackage #:mallet/tests/rules/missing-variable-docstring
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:doc-util #:mallet/tests/docstring-test-utils)
   (#:pkg-exports #:mallet/rules/forms/package-exports)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/missing-variable-docstring)

(defun check-missing-variable-docstring (code)
  "Check CODE for missing-variable-docstring violations."
  (let ((forms (parser:parse-forms code #p"test.lisp"))
        (rule (make-instance 'rules:missing-variable-docstring-rule)))
    (mapcan (lambda (form)
              (rules:check-form rule form #p"test.lisp"))
            forms)))

(defun check-exported-only (dir code)
  "Run missing-variable-docstring-rule with :exported-only t on CODE from file in DIR."
  (let ((test-file (merge-pathnames "test.lisp" dir))
        (rule (make-instance 'rules:missing-variable-docstring-rule :exported-only t)))
    (mapcan (lambda (form)
              (rules:check-form rule form test-file))
            (parser:parse-forms code test-file))))

;;; Valid cases (no violations)

(deftest missing-variable-docstring-valid
  (testing "defvar with docstring is not flagged"
    (ok (null (check-missing-variable-docstring
               "(defvar *x* 42 \"The answer.\")"))))

  (testing "defparameter with docstring is not flagged"
    (ok (null (check-missing-variable-docstring
               "(defparameter *max-retries* 5 \"Maximum retry count.\")"))))

  (testing "defvar without init value is not flagged (not checkable)"
    (ok (null (check-missing-variable-docstring
               "(defvar *x*)"))))

  (testing "defun without docstring is not flagged by this rule"
    (ok (null (check-missing-variable-docstring
               "(defun foo (x) x)"))))

  (testing "defclass without docstring is not flagged by this rule"
    (ok (null (check-missing-variable-docstring
               "(defclass point () ())")))))

;;; Invalid cases (violations expected)

(deftest missing-variable-docstring-invalid
  (testing "defvar with init value but no docstring is flagged"
    (let ((violations (check-missing-variable-docstring "(defvar *x* 42)")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :missing-variable-docstring))))

  (testing "defparameter without docstring is flagged"
    (let ((violations (check-missing-variable-docstring
                       "(defparameter *max-retries* 5)")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :missing-variable-docstring))
      (ok (eq (violation:violation-severity (first violations)) :info))))

  (testing "defvar with nil init value but no docstring is flagged"
    (let ((violations (check-missing-variable-docstring "(defvar *state* nil)")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :missing-variable-docstring)))))

;;; exported-only mode

(deftest missing-variable-docstring-exported-only
  (testing "with :exported-only t — exported defvar without docstring is flagged"
    (let ((dir (doc-util:make-temp-dir "variable-docstring-test")))
      (unwind-protect
           (progn
             (doc-util:write-temp-file dir "package.lisp"
                                       "(defpackage #:my-pkg (:export #:*my-var*))")
             (pkg-exports:clear-package-export-cache)
             (let ((violations (check-exported-only
                                dir
                                "(in-package :my-pkg)
(defvar *my-var* 42)")))
               (ok (= 1 (length violations)))
               (ok (search "*my-var*" (violation:violation-message (first violations))))
               (ok (search "Exported" (violation:violation-message (first violations))))))
        (pkg-exports:clear-package-export-cache)
        (doc-util:cleanup-temp-dir dir))))

  (testing "with :exported-only t — non-exported defvar is not flagged"
    (let ((dir (doc-util:make-temp-dir "variable-docstring-test")))
      (unwind-protect
           (progn
             (doc-util:write-temp-file dir "package.lisp"
                                       "(defpackage #:my-pkg (:export #:*exported-var*))")
             (pkg-exports:clear-package-export-cache)
             (let ((violations (check-exported-only
                                dir
                                "(in-package :my-pkg)
(defvar *internal-var* 0)")))
               (ok (null violations))))
        (pkg-exports:clear-package-export-cache)
        (doc-util:cleanup-temp-dir dir))))

  (testing "with :exported-only t — violation message includes exported DEFVAR and name"
    (let ((dir (doc-util:make-temp-dir "variable-docstring-test")))
      (unwind-protect
           (progn
             (doc-util:write-temp-file dir "package.lisp"
                                       "(defpackage #:fmt-pkg (:export #:*my-var*))")
             (pkg-exports:clear-package-export-cache)
             (let ((violations (check-exported-only
                                dir
                                "(in-package :fmt-pkg)
(defvar *my-var* 0)")))
               (ok (= 1 (length violations)))
               (let ((msg (violation:violation-message (first violations))))
                 (ok (search "Exported" msg))
                 (ok (search "DEFVAR" msg))
                 (ok (search "*my-var*" msg)))))
        (pkg-exports:clear-package-export-cache)
        (doc-util:cleanup-temp-dir dir))))

  (testing "with :exported-only t — defvar without init value is still skipped"
    (let ((dir (doc-util:make-temp-dir "variable-docstring-test")))
      (unwind-protect
           (progn
             (doc-util:write-temp-file dir "package.lisp"
                                       "(defpackage #:my-pkg (:export #:*my-var*))")
             (pkg-exports:clear-package-export-cache)
             (let ((violations (check-exported-only
                                dir
                                "(in-package :my-pkg)
(defvar *my-var*)")))
               (ok (null violations))))
        (pkg-exports:clear-package-export-cache)
        (doc-util:cleanup-temp-dir dir))))

  (testing "with :exported-only t — severity auto-upgrades to :warning"
    (let ((rule (make-instance 'rules:missing-variable-docstring-rule :exported-only t)))
      (ok (eq :warning (rules:rule-severity rule))))))
