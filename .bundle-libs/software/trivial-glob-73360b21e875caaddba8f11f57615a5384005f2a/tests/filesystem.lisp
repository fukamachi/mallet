(defpackage #:trivial-glob/tests/filesystem
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:glob #:trivial-glob)))
(in-package #:trivial-glob/tests/filesystem)

(defvar *test-dir*
  (merge-pathnames "test-fixtures/"
                   (asdf:system-source-directory :trivial-glob)))

(deftest literal-path-matching
  (testing "Matching literal filenames (no wildcards)"
    (let ((results (glob:glob (merge-pathnames "README.md" *test-dir*))))
      (ok (= (length results) 1))
      (ok (search "README.md" (namestring (first results))))))

  (testing "Non-existent file returns empty list"
    (let ((results (glob:glob (merge-pathnames "nonexistent.txt" *test-dir*))))
      (ok (null results)))))

(deftest wildcard-filename-matching
  (testing "* matches files in directory"
    (let ((results (glob:glob (merge-pathnames "*.txt" *test-dir*))))
      (ok (>= (length results) 2))
      (ok (every (lambda (p) (string= (pathname-type p) "txt")) results))))

  (testing "* matches all files"
    (let ((results (glob:glob (merge-pathnames "*" *test-dir*))))
      (ok (> (length results) 0))
      ;; Should include README.md, *.txt, *.log files
      (ok (some (lambda (p) (search "README" (namestring p))) results))))

  (testing "Pattern with prefix"
    (let ((results (glob:glob (merge-pathnames "file*.txt" *test-dir*))))
      (ok (>= (length results) 2))
      (ok (every (lambda (p)
                   (and (string= (pathname-type p) "txt")
                        (let ((name (pathname-name p)))
                          (and (>= (length name) 4)
                               (string= "file" name :end2 4)))))
                 results)))))

(deftest directory-pattern-matching
  (testing "Match files in subdirectory"
    (let ((results (glob:glob (merge-pathnames "src/*.lisp" *test-dir*))))
      (ok (>= (length results) 1))
      (ok (every (lambda (p)
                   (and (string= (pathname-type p) "lisp")
                        (search "src" (namestring p))))
                 results))))

  (testing "Match files two levels deep"
    (let ((results (glob:glob (merge-pathnames "src/core/*.lisp" *test-dir*))))
      (ok (>= (length results) 2))
      (ok (every (lambda (p)
                   (and (string= (pathname-type p) "lisp")
                        (search "core" (namestring p))))
                 results)))))

(deftest recursive-globbing
  (testing "** matches all subdirectories recursively"
    (let ((results (glob:glob (merge-pathnames "**/*.lisp" *test-dir*))))
      ;; Should find .lisp files in src/, src/core/, src/utils/
      (ok (>= (length results) 4))
      (ok (every (lambda (p) (string= (pathname-type p) "lisp")) results))
      ;; Should include files from different subdirectory levels
      (ok (some (lambda (p) (search "src/main.lisp" (namestring p))) results))
      (ok (some (lambda (p) (search "src/core/" (namestring p))) results))))

  (testing "**/ in middle of pattern"
    (let ((results (glob:glob (merge-pathnames "src/**/*.lisp" *test-dir*))))
      ;; Should find .lisp files in src/ and all its subdirectories
      (ok (>= (length results) 4))
      (ok (every (lambda (p)
                   (and (string= (pathname-type p) "lisp")
                        (search "src" (namestring p))))
                 results))))

  (testing "** matches zero directories"
    (let ((results (glob:glob (merge-pathnames "src/**/*.lisp" *test-dir*))))
      ;; Should include src/main.lisp (zero directories between src and file)
      (ok (some (lambda (p) (search "src/main.lisp" (namestring p))) results)))))

(deftest brace-expansion
  (testing "Simple brace expansion with filenames"
    (let ((results (glob:glob (merge-pathnames "file{1,3}.txt" *test-dir*))))
      (ok (= (length results) 2))
      (ok (some (lambda (p) (search "file1.txt" (namestring p))) results))
      (ok (some (lambda (p) (search "file3.txt" (namestring p))) results))))

  (testing "Brace expansion with file extensions"
    (let ((results (glob:glob (merge-pathnames "*.{txt,log}" *test-dir*))))
      (ok (>= (length results) 3))
      (ok (some (lambda (p) (string= (pathname-type p) "txt")) results))
      (ok (some (lambda (p) (string= (pathname-type p) "log")) results))))

  (testing "Multiple braces in pattern"
    (let ((results (glob:glob (merge-pathnames "file{1,2}.{txt,log}" *test-dir*))))
      (ok (>= (length results) 2))
      (ok (some (lambda (p) (search "file1.txt" (namestring p))) results))
      (ok (some (lambda (p) (search "file2.log" (namestring p))) results))))

  (testing "Brace with single option behaves like literal"
    (let ((results (glob:glob (merge-pathnames "file{1}.txt" *test-dir*))))
      (ok (= (length results) 1))
      (ok (some (lambda (p) (search "file1.txt" (namestring p))) results)))))

(deftest symlink-handling
  (testing "By default, symlinks are not followed"
    (let ((results (glob:glob (merge-pathnames "link-to-src/*.lisp" *test-dir*))))
      ;; Without following symlinks, should not traverse into the symlinked directory
      (ok (null results))))

  (testing "With :follow-symlinks t, symlinks are followed"
    (let ((results (glob:glob (merge-pathnames "link-to-src/*.lisp" *test-dir*)
                              :follow-symlinks t)))
      ;; With symlink following, should find files in the linked directory
      (ok (>= (length results) 1))
      (ok (every (lambda (p) (string= (pathname-type p) "lisp")) results))))

  (testing "Symlink to file is resolved"
    (let ((results (glob:glob (merge-pathnames "link-to-file.txt" *test-dir*)
                              :follow-symlinks t)))
      (ok (= (length results) 1))))

  (testing "Circular symlinks don't cause infinite loops"
    ;; This should complete without hanging
    (let ((results (glob:glob (merge-pathnames "circular-link/**/*.lisp" *test-dir*)
                              :follow-symlinks t)))
      ;; Should find some files but not loop infinitely
      (ok (>= (length results) 0)))))
