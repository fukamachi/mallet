(defpackage #:mallet/tests/rules/missing-struct-docstring
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:docstring #:mallet/rules/forms/docstring)
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

(defun make-temp-dir ()
  "Create a fresh temporary directory."
  (let ((path (uiop:ensure-directory-pathname
               (pathname (format nil "/tmp/mallet-struct-docstring-test-~A/" (random 1000000))))))
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

;;; Violation message format

(deftest missing-struct-docstring-message-format
  (testing "violation message includes DEFSTRUCT and struct name (simple name)"
    (let ((violations (check-struct-docstring "(defstruct my-struct x y)")))
      (ok (= (length violations) 1))
      (let ((msg (violation:violation-message (first violations))))
        (ok (stringp msg))
        (ok (search "DEFSTRUCT" msg))
        (ok (search "my-struct" msg)))))

  (testing "violation message includes struct name from name-and-options list"
    (let ((violations (check-struct-docstring "(defstruct (my-struct (:conc-name ms-)) x)")))
      (ok (= (length violations) 1))
      (let ((msg (violation:violation-message (first violations))))
        (ok (search "DEFSTRUCT" msg))
        (ok (search "my-struct" msg)))))

  (testing "violation message format: 'DEFSTRUCT name is missing a docstring'"
    (let ((violations (check-struct-docstring "(defstruct point x y)")))
      (ok (= (length violations) 1))
      (ok (string= "DEFSTRUCT point is missing a docstring"
                   (violation:violation-message (first violations)))))))

;;; Location reporting

(deftest missing-struct-docstring-location
  (testing "violation reports correct line for single-line form"
    (let ((violations (check-struct-docstring "(defstruct point x y)")))
      (ok (= (length violations) 1))
      (ok (= (violation:violation-line (first violations)) 1))))

  (testing "violation reports correct line for second form"
    (let ((violations (check-struct-docstring
                       "(defstruct first-struct x)
(defstruct second-struct y)")))
      (ok (= (length violations) 2))
      (ok (= (violation:violation-line (first violations)) 1))
      (ok (= (violation:violation-line (second violations)) 2)))))

;;; Exported-only mode

(deftest missing-struct-docstring-exported-only
  (testing "exported-only: exported defstruct without docstring is flagged"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "package.lisp"
                              "(defpackage #:my-pkg (:export #:my-struct))")
             (pkg-exports:clear-package-export-cache)
             (let ((violations (check-struct-exported-only
                                dir
                                "(in-package :my-pkg)
(defstruct my-struct x y)")))
               (ok (= 1 (length violations)))
               (ok (search "my-struct" (violation:violation-message (first violations))))))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir))))

  (testing "exported-only: non-exported defstruct without docstring is NOT flagged"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "package.lisp"
                              "(defpackage #:my-pkg (:export #:other-struct))")
             (pkg-exports:clear-package-export-cache)
             (let ((violations (check-struct-exported-only
                                dir
                                "(in-package :my-pkg)
(defstruct internal-struct x y)")))
               (ok (null violations))))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir))))

  (testing "exported-only: exported defstruct WITH docstring is NOT flagged"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "package.lisp"
                              "(defpackage #:my-pkg (:export #:my-struct))")
             (pkg-exports:clear-package-export-cache)
             (let ((violations (check-struct-exported-only
                                dir
                                "(in-package :my-pkg)
(defstruct my-struct \"A documented struct.\" x y)")))
               (ok (null violations))))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir))))

  (testing "exported-only: message format includes 'Exported'"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "package.lisp"
                              "(defpackage #:my-pkg (:export #:my-struct))")
             (pkg-exports:clear-package-export-cache)
             (let ((violations (check-struct-exported-only
                                dir
                                "(in-package :my-pkg)
(defstruct my-struct x y)")))
               (ok (= 1 (length violations)))
               (ok (string= "Exported DEFSTRUCT my-struct is missing a docstring"
                            (violation:violation-message (first violations))))))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir))))

  (testing "exported-only: severity auto-upgrades to :warning"
    (let ((rule (make-instance 'docstring:missing-struct-docstring-rule :exported-only t)))
      (ok (eq :warning (rules:rule-severity rule))))))
