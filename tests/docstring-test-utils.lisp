(defpackage #:mallet/tests/docstring-test-utils
  (:use #:cl)
  (:export #:make-temp-dir
           #:write-temp-file
           #:cleanup-temp-dir))
(in-package #:mallet/tests/docstring-test-utils)

(defun make-temp-dir (prefix)
  "Create a fresh temporary directory under /tmp with PREFIX in its name."
  (let ((path (uiop:ensure-directory-pathname
               (pathname (format nil "/tmp/mallet-~A-~A/"
                                 prefix
                                 (random 1000000))))))
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
