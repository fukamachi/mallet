(defpackage #:mallet/tests/rules/missing-exported-docstring
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:docstring #:mallet/rules/forms/docstring)
   (#:pkg-exports #:mallet/rules/forms/package-exports)
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/missing-exported-docstring)

;;; Helpers

(defun make-temp-dir ()
  "Create a fresh temporary directory."
  (let ((path (uiop:ensure-directory-pathname
               (pathname (format nil "/tmp/mallet-exported-docstring-test-~A/" (random 1000000))))))
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

(defun check-exported-docstring (dir code)
  "Run missing-exported-docstring-rule on CODE, with DIR as project root.
Parses CODE as if it comes from a file named 'test.lisp' inside DIR.
Returns list of violations."
  (let ((test-file (merge-pathnames "test.lisp" dir))
        (rule (make-instance 'docstring:missing-exported-docstring-rule)))
    (mapcan (lambda (form)
              (rules:check-form rule form test-file))
            (parser:parse-forms code test-file))))

;;; Basic export lookup

(deftest missing-exported-docstring-basic
  (testing "exported defun missing docstring emits violation"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "package.lisp"
                              "(defpackage #:my-pkg (:export #:my-fn))")
             (pkg-exports:clear-package-export-cache)
             (let ((violations (check-exported-docstring
                                dir
                                "(in-package :my-pkg)
(defun my-fn (x) x)")))
               (ok (= 1 (length violations)))
               (ok (search "my-fn" (violation:violation-message (first violations))))
               (ok (search "DEFUN" (violation:violation-message (first violations))))))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir))))

  (testing "exported defun WITH docstring emits no violation"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "package.lisp"
                              "(defpackage #:my-pkg (:export #:my-fn))")
             (pkg-exports:clear-package-export-cache)
             (let ((violations (check-exported-docstring
                                dir
                                "(in-package :my-pkg)
(defun my-fn (x)
  \"Does something.\"
  x)")))
               (ok (null violations))))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir))))

  (testing "non-exported defun missing docstring emits no violation"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "package.lisp"
                              "(defpackage #:my-pkg (:export #:exported-fn))")
             (pkg-exports:clear-package-export-cache)
             (let ((violations (check-exported-docstring
                                dir
                                "(in-package :my-pkg)
(defun internal-fn (x) x)")))
               (ok (null violations))))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir)))))

(deftest missing-exported-docstring-defmethod-skip
  (testing "defmethod is always skipped even if exported"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "package.lisp"
                              "(defpackage #:my-pkg (:export #:my-method))")
             (pkg-exports:clear-package-export-cache)
             (let ((violations (check-exported-docstring
                                dir
                                "(in-package :my-pkg)
(defmethod my-method ((x integer)) x)")))
               (ok (null violations))))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir)))))

(deftest missing-exported-docstring-form-types
  (testing "exported defmacro missing docstring emits violation"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "package.lisp"
                              "(defpackage #:my-pkg (:export #:my-macro))")
             (pkg-exports:clear-package-export-cache)
             (let ((violations (check-exported-docstring
                                dir
                                "(in-package :my-pkg)
(defmacro my-macro (x) x)")))
               (ok (= 1 (length violations)))
               (ok (search "DEFMACRO" (violation:violation-message (first violations))))))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir))))

  (testing "exported defgeneric missing docstring emits violation"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "package.lisp"
                              "(defpackage #:my-pkg (:export #:my-generic))")
             (pkg-exports:clear-package-export-cache)
             (let ((violations (check-exported-docstring
                                dir
                                "(in-package :my-pkg)
(defgeneric my-generic (x))")))
               (ok (= 1 (length violations)))
               (ok (search "DEFGENERIC" (violation:violation-message (first violations))))))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir))))

  (testing "exported defclass missing docstring emits violation"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "package.lisp"
                              "(defpackage #:my-pkg (:export #:my-class))")
             (pkg-exports:clear-package-export-cache)
             (let ((violations (check-exported-docstring
                                dir
                                "(in-package :my-pkg)
(defclass my-class () ())")))
               (ok (= 1 (length violations)))
               (ok (search "DEFCLASS" (violation:violation-message (first violations))))))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir))))

  (testing "exported defgeneric WITH documentation option emits no violation"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "package.lisp"
                              "(defpackage #:my-pkg (:export #:my-generic))")
             (pkg-exports:clear-package-export-cache)
             (let ((violations (check-exported-docstring
                                dir
                                "(in-package :my-pkg)
(defgeneric my-generic (x)
  (:documentation \"Does generic things.\"))")))
               (ok (null violations))))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir)))))

(deftest missing-exported-docstring-no-in-package
  (testing "no in-package form → no violations (can't determine package)"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "package.lisp"
                              "(defpackage #:my-pkg (:export #:my-fn))")
             (pkg-exports:clear-package-export-cache)
             (let ((violations (check-exported-docstring
                                dir
                                "(defun my-fn (x) x)")))
               (ok (null violations))))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir)))))

(deftest missing-exported-docstring-cross-file
  (testing "defpackage in package.lisp, defun in test.lisp — cross-file export lookup"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             ;; package.lisp has the defpackage with exports
             (write-temp-file dir "package.lisp"
                              "(defpackage #:cross-pkg
  (:export #:exported-fn #:another-exported))")
             (pkg-exports:clear-package-export-cache)
             ;; test.lisp has in-package + defun (no defpackage)
             (let ((violations (check-exported-docstring
                                dir
                                "(in-package :cross-pkg)
(defun exported-fn () nil)
(defun internal-fn () nil)
(defun another-exported () nil)")))
               (ok (= 2 (length violations)))
               (ok (some (lambda (v)
                           (search "exported-fn" (violation:violation-message v)))
                         violations))
               (ok (some (lambda (v)
                           (search "another-exported" (violation:violation-message v)))
                         violations))
               ;; internal-fn should not appear
               (ok (notany (lambda (v)
                             (search "internal-fn" (violation:violation-message v)))
                           violations))))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir))))

  (testing "case-insensitive: exported as #:UPPER, defined as lower"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "package.lisp"
                              "(defpackage #:CASE-PKG (:export #:MY-FN))")
             (pkg-exports:clear-package-export-cache)
             (let ((violations (check-exported-docstring
                                dir
                                "(in-package :case-pkg)
(defun my-fn () nil)")))
               (ok (= 1 (length violations)))))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir)))))

(deftest missing-exported-docstring-multiple-packages
  (testing "multiple packages in same project, each checked correctly"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "package.lisp"
                              "(defpackage #:pkg-a (:export #:fn-a))
(defpackage #:pkg-b (:export #:fn-b))")
             (pkg-exports:clear-package-export-cache)
             (let ((violations (check-exported-docstring
                                dir
                                "(in-package :pkg-a)
(defun fn-a () nil)
(in-package :pkg-b)
(defun fn-b () nil)
(defun fn-internal () nil)")))
               (ok (= 2 (length violations)))
               (ok (some (lambda (v)
                           (search "fn-a" (violation:violation-message v)))
                         violations))
               (ok (some (lambda (v)
                           (search "fn-b" (violation:violation-message v)))
                         violations))))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir)))))

(deftest missing-exported-docstring-violation-format
  (testing "violation message format: 'Exported DEFUN foo is missing a docstring'"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "package.lisp"
                              "(defpackage #:fmt-pkg (:export #:my-fn))")
             (pkg-exports:clear-package-export-cache)
             (let ((violations (check-exported-docstring
                                dir
                                "(in-package :fmt-pkg)
(defun my-fn (x) x)")))
               (ok (= 1 (length violations)))
               (ok (string= "Exported DEFUN my-fn is missing a docstring"
                            (violation:violation-message (first violations))))))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir))))

  (testing "violation rule name is :missing-exported-docstring"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "package.lisp"
                              "(defpackage #:fmt-pkg2 (:export #:my-fn))")
             (pkg-exports:clear-package-export-cache)
             (let ((violations (check-exported-docstring
                                dir
                                "(in-package :fmt-pkg2)
(defun my-fn (x) x)")))
               (ok (= 1 (length violations)))
               (ok (eq :missing-exported-docstring
                       (violation:violation-rule (first violations))))))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir)))))
