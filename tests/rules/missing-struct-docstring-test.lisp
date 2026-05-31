(defpackage #:mallet/tests/rules/missing-struct-docstring
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:docstring #:mallet/rules/forms/docstring)
   (#:doc-util #:mallet/tests/docstring-test-utils)
   (#:pkg-exports #:mallet/rules/forms/package-exports)
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/missing-struct-docstring)

;;; Helpers

(defun check-struct-docstring (code)
  "Check CODE for missing-struct-docstring violations."
  (let ((forms (parser:parse-forms code #p"test.lisp"))
        (rule (make-instance 'docstring:missing-struct-docstring-rule)))
    (mapcan (lambda (form)
              (rules:check-form rule form #p"test.lisp"))
            forms)))

(defun check-struct-exported-only (dir code)
  "Run missing-struct-docstring-rule with :exported-only t on CODE in DIR."
  (let ((test-file (merge-pathnames "test.lisp" dir))
        (rule (make-instance 'docstring:missing-struct-docstring-rule :exported-only t)))
    (mapcan (lambda (form)
              (rules:check-form rule form test-file))
            (parser:parse-forms code test-file))))

;;; Valid cases (no violations)

(deftest missing-struct-docstring-valid
  (testing "defstruct with body docstring is not flagged"
    (ok (null (check-struct-docstring
               "(defstruct point \"A 2D point.\" x y)"))))

  (testing "defstruct with :documentation option in name-and-options is not flagged"
    (ok (null (check-struct-docstring
               "(defstruct (point (:documentation \"A 2D point.\")) x y)"))))

  (testing "defstruct with both body string and :documentation is not flagged"
    (ok (null (check-struct-docstring
               "(defstruct (point (:documentation \"A 2D point.\")) \"A 2D point.\" x y)"))))

  (testing "non-defstruct forms are not flagged"
    (ok (null (check-struct-docstring
               "(defun foo (x) \"Doc.\" x)"))))

  (testing "defclass is not flagged by struct rule"
    (ok (null (check-struct-docstring
               "(defclass point () ())")))))

;;; Invalid cases (violations expected)

(deftest missing-struct-docstring-invalid
  (testing "defstruct without docstring is flagged (simple symbol name)"
    (let ((violations (check-struct-docstring "(defstruct point x y)")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :missing-struct-docstring))))

  (testing "defstruct with name-and-options list but no :documentation is flagged"
    (let ((violations (check-struct-docstring "(defstruct (point (:conc-name pt-)) x y)")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :missing-struct-docstring))))

  (testing "defstruct with no slots and no docstring is flagged"
    (let ((violations (check-struct-docstring "(defstruct empty-struct)")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :missing-struct-docstring))))

  (testing "default severity is :info"
    (let ((violations (check-struct-docstring "(defstruct point x y)")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-severity (first violations)) :info)))))

;;; Exported-only mode

(deftest missing-struct-docstring-exported-only
  (testing "exported-only: exported defstruct without docstring is flagged"
    (let ((dir (doc-util:make-temp-dir "struct-docstring-test")))
      (unwind-protect
           (progn
             (doc-util:write-temp-file dir "package.lisp"
                                       "(defpackage #:my-pkg (:export #:my-struct))")
             (pkg-exports:clear-package-export-cache)
             (let ((violations (check-struct-exported-only
                                dir
                                "(in-package :my-pkg)
(defstruct my-struct x y)")))
               (ok (= 1 (length violations)))
               (ok (search "my-struct" (violation:violation-message (first violations))))))
        (pkg-exports:clear-package-export-cache)
        (doc-util:cleanup-temp-dir dir))))

  (testing "exported-only: non-exported defstruct without docstring is NOT flagged"
    (let ((dir (doc-util:make-temp-dir "struct-docstring-test")))
      (unwind-protect
           (progn
             (doc-util:write-temp-file dir "package.lisp"
                                       "(defpackage #:my-pkg (:export #:other-struct))")
             (pkg-exports:clear-package-export-cache)
             (let ((violations (check-struct-exported-only
                                dir
                                "(in-package :my-pkg)
(defstruct internal-struct x y)")))
               (ok (null violations))))
        (pkg-exports:clear-package-export-cache)
        (doc-util:cleanup-temp-dir dir))))

  (testing "exported-only: exported defstruct WITH docstring is NOT flagged"
    (let ((dir (doc-util:make-temp-dir "struct-docstring-test")))
      (unwind-protect
           (progn
             (doc-util:write-temp-file dir "package.lisp"
                                       "(defpackage #:my-pkg (:export #:my-struct))")
             (pkg-exports:clear-package-export-cache)
             (let ((violations (check-struct-exported-only
                                dir
                                "(in-package :my-pkg)
(defstruct my-struct \"A documented struct.\" x y)")))
               (ok (null violations))))
        (pkg-exports:clear-package-export-cache)
        (doc-util:cleanup-temp-dir dir))))

  (testing "exported-only: message includes exported DEFSTRUCT and struct name"
    (let ((dir (doc-util:make-temp-dir "struct-docstring-test")))
      (unwind-protect
           (progn
             (doc-util:write-temp-file dir "package.lisp"
                                       "(defpackage #:my-pkg (:export #:my-struct))")
             (pkg-exports:clear-package-export-cache)
             (let ((violations (check-struct-exported-only
                                dir
                                "(in-package :my-pkg)
(defstruct my-struct x y)")))
               (ok (= 1 (length violations)))
               (let ((msg (violation:violation-message (first violations))))
                 (ok (search "Exported" msg))
                 (ok (search "DEFSTRUCT" msg))
                 (ok (search "my-struct" msg)))))
        (pkg-exports:clear-package-export-cache)
        (doc-util:cleanup-temp-dir dir))))

  (testing "exported-only: severity auto-upgrades to :warning"
    (let ((rule (make-instance 'docstring:missing-struct-docstring-rule :exported-only t)))
      (ok (eq :warning (rules:rule-severity rule))))))
