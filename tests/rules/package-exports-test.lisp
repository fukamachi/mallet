(defpackage #:mallet/tests/rules/package-exports
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:pkg-exports #:mallet/rules/forms/package-exports)))
(in-package #:mallet/tests/rules/package-exports)

;;; Helpers

(defun make-temp-dir ()
  "Create a temporary directory for test files."
  (let ((path (uiop:ensure-directory-pathname
               (pathname (format nil "/tmp/mallet-pkg-exports-test-~A/" (random 1000000))))))
    (ensure-directories-exist path)
    path))

(defun write-temp-file (dir name content)
  "Write CONTENT to a file NAME under DIR."
  (let ((path (merge-pathnames name dir)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string content out))
    path))

(defun cleanup-temp-dir (dir)
  "Remove temporary directory and its contents."
  (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))

;;; Tests for exported-symbol-p with temp directories

(deftest package-exports-basic
  (testing "exported symbol is found"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "packages.lisp"
                              "(defpackage #:my-package (:export #:my-function #:my-variable))")
             (pkg-exports:clear-package-export-cache)
             (ok (pkg-exports:exported-symbol-p dir "my-package" "my-function")))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir))))

  (testing "non-exported symbol returns NIL"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "packages.lisp"
                              "(defpackage #:my-package (:export #:exported-fn))")
             (pkg-exports:clear-package-export-cache)
             (ok (null (pkg-exports:exported-symbol-p dir "my-package" "not-exported"))))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir))))

  (testing "unknown package returns NIL"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "packages.lisp"
                              "(defpackage #:known-package (:export #:foo))")
             (pkg-exports:clear-package-export-cache)
             (ok (null (pkg-exports:exported-symbol-p dir "unknown-package" "foo"))))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir)))))

(deftest package-exports-case-insensitive
  (testing "exported with #:UPPER-CASE, looked up lowercase"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "packages.lisp"
                              "(defpackage #:MY-PKG (:export #:HELLO-WORLD))")
             (pkg-exports:clear-package-export-cache)
             (ok (pkg-exports:exported-symbol-p dir "my-pkg" "hello-world"))
             (ok (pkg-exports:exported-symbol-p dir "MY-PKG" "HELLO-WORLD"))
             (ok (pkg-exports:exported-symbol-p dir "My-Pkg" "Hello-World")))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir))))

  (testing "keyword-style package name :my-pkg"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "packages.lisp"
                              "(defpackage :my-pkg (:export :my-sym))")
             (pkg-exports:clear-package-export-cache)
             (ok (pkg-exports:exported-symbol-p dir "my-pkg" "my-sym")))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir)))))

(deftest package-exports-multiple-clauses
  (testing "multiple :export clauses in one defpackage"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "packages.lisp"
                              "(defpackage #:multi
  (:export #:foo)
  (:export #:bar)
  (:export #:baz))")
             (pkg-exports:clear-package-export-cache)
             (ok (pkg-exports:exported-symbol-p dir "multi" "foo"))
             (ok (pkg-exports:exported-symbol-p dir "multi" "bar"))
             (ok (pkg-exports:exported-symbol-p dir "multi" "baz")))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir)))))

(deftest package-exports-uiop-define-package
  (testing "uiop:define-package is recognized"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "packages.lisp"
                              "(uiop:define-package #:uiop-pkg (:export #:exported-sym))")
             (pkg-exports:clear-package-export-cache)
             (ok (pkg-exports:exported-symbol-p dir "uiop-pkg" "exported-sym")))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir)))))

(deftest package-exports-cross-file
  (testing "package defined in one file, symbol looked up via project root"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             ;; Package definition in package.lisp
             (write-temp-file dir "package.lisp"
                              "(defpackage #:cross-file-pkg (:export #:exported-fn #:another-fn))")
             ;; Another file with in-package (simulates a source file)
             (write-temp-file dir "impl.lisp"
                              "(in-package #:cross-file-pkg)
(defun exported-fn () nil)")
             (pkg-exports:clear-package-export-cache)
             (ok (pkg-exports:exported-symbol-p dir "cross-file-pkg" "exported-fn"))
             (ok (pkg-exports:exported-symbol-p dir "cross-file-pkg" "another-fn"))
             (ok (null (pkg-exports:exported-symbol-p dir "cross-file-pkg" "not-exported"))))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir)))))

(deftest package-exports-caching
  (testing "cache is used on second call (mutating file does not affect cached result)"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "packages.lisp"
                              "(defpackage #:cached-pkg (:export #:original-sym))")
             (pkg-exports:clear-package-export-cache)
             ;; First call builds cache
             (ok (pkg-exports:exported-symbol-p dir "cached-pkg" "original-sym"))
             ;; Overwrite file with different content
             (write-temp-file dir "packages.lisp"
                              "(defpackage #:cached-pkg (:export #:new-sym))")
             ;; Cache still returns result from first scan
             (ok (pkg-exports:exported-symbol-p dir "cached-pkg" "original-sym"))
             ;; After clearing cache, re-scans file
             (pkg-exports:clear-package-export-cache)
             (ok (null (pkg-exports:exported-symbol-p dir "cached-pkg" "original-sym")))
             (ok (pkg-exports:exported-symbol-p dir "cached-pkg" "new-sym")))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir))))

  (testing "clear-package-export-cache allows re-scanning"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "packages.lisp"
                              "(defpackage #:reload-pkg (:export #:sym-a))")
             (pkg-exports:clear-package-export-cache)
             (ok (pkg-exports:exported-symbol-p dir "reload-pkg" "sym-a"))
             (ok (null (pkg-exports:exported-symbol-p dir "reload-pkg" "sym-b")))
             ;; Add second export to file
             (write-temp-file dir "packages.lisp"
                              "(defpackage #:reload-pkg (:export #:sym-a #:sym-b))")
             (pkg-exports:clear-package-export-cache)
             (ok (pkg-exports:exported-symbol-p dir "reload-pkg" "sym-a"))
             (ok (pkg-exports:exported-symbol-p dir "reload-pkg" "sym-b")))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir)))))

(deftest package-exports-multiple-packages
  (testing "multiple packages in multiple files"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "pkg-a.lisp"
                              "(defpackage #:pkg-a (:export #:func-a #:var-a))")
             (write-temp-file dir "pkg-b.lisp"
                              "(defpackage #:pkg-b (:export #:func-b))")
             (pkg-exports:clear-package-export-cache)
             (ok (pkg-exports:exported-symbol-p dir "pkg-a" "func-a"))
             (ok (pkg-exports:exported-symbol-p dir "pkg-a" "var-a"))
             (ok (null (pkg-exports:exported-symbol-p dir "pkg-a" "func-b")))
             (ok (pkg-exports:exported-symbol-p dir "pkg-b" "func-b"))
             (ok (null (pkg-exports:exported-symbol-p dir "pkg-b" "func-a"))))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir)))))

;;; Tests for test-package-p

(deftest test-package-detection
  (testing "package with (:use #:rove) is a test package"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "tests.lisp"
                              "(defpackage #:my-tests (:use #:cl #:rove))")
             (pkg-exports:clear-package-export-cache)
             (ok (pkg-exports:test-package-p dir "my-tests")))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir))))

  (testing "package with (:import-from #:fiveam ...) is a test package"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "tests.lisp"
                              "(defpackage #:my-5am-tests (:import-from #:fiveam #:def-suite))")
             (pkg-exports:clear-package-export-cache)
             (ok (pkg-exports:test-package-p dir "my-5am-tests")))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir))))

  (testing "package with (:use #:cl) only is not a test package"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "src.lisp"
                              "(defpackage #:my-lib (:use #:cl))")
             (pkg-exports:clear-package-export-cache)
             (ok (null (pkg-exports:test-package-p dir "my-lib"))))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir))))

  (testing "unknown package name returns NIL"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "src.lisp"
                              "(defpackage #:known-pkg (:use #:rove))")
             (pkg-exports:clear-package-export-cache)
             (ok (null (pkg-exports:test-package-p dir "unknown-pkg"))))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir))))

  (testing "lookup is case-insensitive"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "tests.lisp"
                              "(defpackage #:MY-TESTS (:use #:cl #:rove))")
             (pkg-exports:clear-package-export-cache)
             (ok (pkg-exports:test-package-p dir "my-tests"))
             (ok (pkg-exports:test-package-p dir "MY-TESTS"))
             (ok (pkg-exports:test-package-p dir "My-Tests")))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir)))))

(deftest test-package-p-exported-symbol-p-still-works
  (testing "exported-symbol-p is unaffected after refactor"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (write-temp-file dir "pkg.lisp"
                              "(defpackage #:my-pkg (:use #:rove) (:export #:my-fn))")
             (pkg-exports:clear-package-export-cache)
             (ok (pkg-exports:exported-symbol-p dir "my-pkg" "my-fn"))
             (ok (null (pkg-exports:exported-symbol-p dir "my-pkg" "other-fn")))
             (ok (pkg-exports:test-package-p dir "my-pkg")))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir)))))
