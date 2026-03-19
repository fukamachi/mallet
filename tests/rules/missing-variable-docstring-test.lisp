(defpackage #:mallet/tests/rules/missing-variable-docstring
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
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

(defun make-temp-dir ()
  "Create a fresh temporary directory."
  (let ((path (uiop:ensure-directory-pathname
               (pathname (format nil "/tmp/mallet-missing-var-doc-test-~A/" (random 1000000))))))
    (ensure-directories-exist path)
    path))

(defun write-temp-file (dir name content)
  "Write CONTENT to NAME under DIR."
  (let ((path (merge-pathnames name dir)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string content out))
    path))

(defun cleanup-temp-dir (dir)
  "Remove temporary test directory."
  (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))

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

;;; Violation message format

(deftest missing-variable-docstring-message-format
  (testing "defvar violation message includes DEFVAR and variable name"
    (let ((violations (check-missing-variable-docstring "(defvar *counter* 0)")))
      (ok (= (length violations) 1))
      (let ((msg (violation:violation-message (first violations))))
        (ok (stringp msg))
        (ok (search "DEFVAR" msg))
        (ok (search "*counter*" msg)))))

  (testing "defparameter violation message includes DEFPARAMETER and variable name"
    (let ((violations (check-missing-variable-docstring "(defparameter *limit* 100)")))
      (ok (= (length violations) 1))
      (let ((msg (violation:violation-message (first violations))))
        (ok (search "DEFPARAMETER" msg))
        (ok (search "*limit*" msg))))))

;;; Location reporting

(deftest missing-variable-docstring-location
  (testing "violation reports line 1 for single-line form"
    (let ((violations (check-missing-variable-docstring "(defvar *x* 0)")))
      (ok (= (length violations) 1))
      (ok (= (violation:violation-line (first violations)) 1))))

  (testing "violation reports correct line for multi-line code"
    (let ((violations (check-missing-variable-docstring
                       "(defvar *x* 0)
(defparameter *y* 1)")))
      (ok (= (length violations) 2))
      (ok (= (violation:violation-line (first violations)) 1))
      (ok (= (violation:violation-line (second violations)) 2)))))

;;; Multiple forms

(deftest missing-variable-docstring-multiple-forms
  (testing "multiple missing docstrings are all flagged"
    (let ((violations (check-missing-variable-docstring
                       "(defvar *a* 1)
(defparameter *b* 2)
(defvar *c* 3)")))
      (ok (= (length violations) 3))))

  (testing "mix of documented and undocumented variables"
    (let ((violations (check-missing-variable-docstring
                       "(defvar *a* 1 \"Documented.\")
(defparameter *b* 2)")))
      (ok (= (length violations) 1))
      (ok (search "*b*" (violation:violation-message (first violations)))))))

;;; exported-only mode

(deftest missing-variable-docstring-exported-only
  (testing "with :exported-only t — exported defvar without docstring is flagged"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "package.lisp"
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
        (cleanup-temp-dir dir))))

  (testing "with :exported-only t — non-exported defvar is not flagged"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "package.lisp"
                              "(defpackage #:my-pkg (:export #:*exported-var*))")
             (pkg-exports:clear-package-export-cache)
             (let ((violations (check-exported-only
                                dir
                                "(in-package :my-pkg)
(defvar *internal-var* 0)")))
               (ok (null violations))))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir))))

  (testing "with :exported-only t — violation message starts with 'Exported'"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "package.lisp"
                              "(defpackage #:fmt-pkg (:export #:*my-var*))")
             (pkg-exports:clear-package-export-cache)
             (let ((violations (check-exported-only
                                dir
                                "(in-package :fmt-pkg)
(defvar *my-var* 0)")))
               (ok (= 1 (length violations)))
               (ok (string= "Exported DEFVAR *my-var* is missing a docstring"
                            (violation:violation-message (first violations))))))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir))))

  (testing "with :exported-only t — defvar without init value is still skipped"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "package.lisp"
                              "(defpackage #:my-pkg (:export #:*my-var*))")
             (pkg-exports:clear-package-export-cache)
             (let ((violations (check-exported-only
                                dir
                                "(in-package :my-pkg)
(defvar *my-var*)")))
               (ok (null violations))))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir))))

  (testing "with :exported-only t — exported-only is set on rule instance"
    (let ((rule (make-instance 'rules:missing-variable-docstring-rule :exported-only t)))
      (ok (rules:rule-exported-only-p rule)))))
