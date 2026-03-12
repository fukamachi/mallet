(defpackage #:mallet/tests/engine-cross-file-integration
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:engine #:mallet/engine)
   (#:config #:mallet/config)
   (#:rules #:mallet/rules)
   (#:violation #:mallet/violation)
   (#:pkg-exports #:mallet/rules/forms/package-exports)))
(in-package #:mallet/tests/engine-cross-file-integration)

;;; Helpers

(defun make-temp-dir ()
  "Create a temporary directory for test files."
  (let ((path (uiop:ensure-directory-pathname
               (pathname (format nil "/tmp/mallet-crossfile-test-~A/" (random 1000000))))))
    (ensure-directories-exist path)
    path))

(defun write-temp-file (dir name content)
  "Write CONTENT to a file NAME under DIR, returning the pathname."
  (let ((path (merge-pathnames name dir)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string content out))
    path))

(defun cleanup-temp-dir (dir)
  "Remove temporary directory and its contents."
  (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))

(defun make-dca-config ()
  "Create a config with only the double-colon-access rule enabled."
  (config:make-config
   :rules (list (rules:make-rule :double-colon-access))))

;;; End-to-end cross-file detection via engine:lint-file

(deftest cross-file-test-package-end-to-end
  (testing "package.lisp defines test package; tests.lisp :: access reports no violations"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             ;; Project root marker so find-project-root-for-file stays in the temp dir
             (ensure-directories-exist (merge-pathnames ".git/" dir))
             (write-temp-file dir "package.lisp"
                              "(defpackage #:my-project/tests
  (:use #:cl #:rove))")
             (let* ((tests-path
                      (write-temp-file dir "tests.lisp"
                                       "(in-package #:my-project/tests)
(deftest my-test
  (testing \"internal access is fine in test files\"
    (ok (some-lib::internal-fn 42))))"))
                    (dca-violations
                      (remove-if-not
                       (lambda (v) (eq :double-colon-access (violation:violation-rule v)))
                       (engine:lint-file tests-path :config (make-dca-config)))))
               (ok (null dca-violations)
                   "No double-colon-access violations in cross-file test package")))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir))))

  (testing "package.lisp defines non-test package; :: access still reports violations"
    (let ((dir (make-temp-dir)))
      (unwind-protect
           (progn
             (ensure-directories-exist (merge-pathnames ".git/" dir))
             (write-temp-file dir "package.lisp"
                              "(defpackage #:my-project/impl
  (:use #:cl))")
             (let* ((src-path
                      (write-temp-file dir "src.lisp"
                                       "(in-package #:my-project/impl)
(defun bad () some-lib::internal-fn)"))
                    (dca-violations
                      (remove-if-not
                       (lambda (v) (eq :double-colon-access (violation:violation-rule v)))
                       (engine:lint-file src-path :config (make-dca-config)))))
               (ok (= 1 (length dca-violations))
                   "One double-colon-access violation in non-test package")))
        (pkg-exports:clear-package-export-cache)
        (cleanup-temp-dir dir)))))
