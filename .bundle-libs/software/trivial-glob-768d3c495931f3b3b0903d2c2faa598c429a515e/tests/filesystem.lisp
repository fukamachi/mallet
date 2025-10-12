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

(deftest exclude-patterns
  (testing "Exclude single pattern string"
    (let ((results (glob:glob (merge-pathnames "*.txt" *test-dir*)
                              :exclude "file1.txt")))
      ;; Should include file2.txt, file3.txt but not file1.txt
      (ok (not (some (lambda (p) (search "file1.txt" (namestring p))) results)))
      (ok (some (lambda (p) (search "file2.txt" (namestring p))) results))))

  (testing "Exclude with wildcard pattern"
    (let ((results (glob:glob (merge-pathnames "*.txt" *test-dir*)
                              :exclude "file[12].txt")))
      ;; Should exclude file1.txt and file2.txt
      (ok (not (some (lambda (p) (search "file1.txt" (namestring p))) results)))
      (ok (not (some (lambda (p) (search "file2.txt" (namestring p))) results)))
      (ok (some (lambda (p) (search "file3.txt" (namestring p))) results))))

  (testing "Exclude multiple patterns as list"
    (let ((results (glob:glob (merge-pathnames "**/*.lisp" *test-dir*)
                              :exclude '("*/core/*.lisp" "*/utils/*.lisp"))))
      ;; Should exclude files in core/ and utils/ subdirectories
      (ok (not (some (lambda (p) (search "/core/" (namestring p))) results)))
      (ok (not (some (lambda (p) (search "/utils/" (namestring p))) results)))
      ;; Should still include files in src/ root
      (ok (some (lambda (p) (search "src/main.lisp" (namestring p))) results))))

  (testing "Exclude with brace expansion"
    (let ((results (glob:glob (merge-pathnames "*.{txt,log}" *test-dir*)
                              :exclude "*.log")))
      ;; Should include .txt files but exclude .log files
      (ok (some (lambda (p) (string= (pathname-type p) "txt")) results))
      (ok (not (some (lambda (p) (string= (pathname-type p) "log")) results)))))

  (testing "Exclude pattern matches nothing"
    (let* ((all-results (glob:glob (merge-pathnames "*.txt" *test-dir*)))
           (filtered-results (glob:glob (merge-pathnames "*.txt" *test-dir*)
                                        :exclude "nonexistent.xyz")))
      ;; Should return same results when exclude pattern matches nothing
      (ok (= (length all-results) (length filtered-results)))))

  (testing "Exclude all results"
    (let ((results (glob:glob (merge-pathnames "*.txt" *test-dir*)
                              :exclude "*.txt")))
      ;; Should return empty list when all results are excluded
      (ok (null results))))

  (testing "Exclude directory with trailing slash"
    (let ((results (glob:glob (merge-pathnames "**/*.lisp" *test-dir*)
                              :exclude "core/")))
      ;; Should exclude all files under core/ directory
      (ok (not (some (lambda (p) (search "/core/" (namestring p))) results)))
      ;; Should still include files in src/ root and utils/
      (ok (some (lambda (p) (search "src/main.lisp" (namestring p))) results))
      (ok (some (lambda (p) (search "/utils/" (namestring p))) results))))

  (testing "Exclude multiple directories with trailing slash"
    (let ((results (glob:glob (merge-pathnames "**/*.lisp" *test-dir*)
                              :exclude '("core/" "utils/"))))
      ;; Should exclude files in both core/ and utils/
      (ok (not (some (lambda (p) (search "/core/" (namestring p))) results)))
      (ok (not (some (lambda (p) (search "/utils/" (namestring p))) results)))
      ;; Should still include files in src/ root
      (ok (some (lambda (p) (search "src/main.lisp" (namestring p))) results))))

  (testing "Exclude directory path with trailing slash"
    (let ((results (glob:glob (merge-pathnames "**/*" *test-dir*)
                              :exclude "src/core/")))
      ;; Should exclude all files under src/core/ specifically
      (ok (not (some (lambda (p) (search "src/core/" (namestring p))) results)))
      ;; Should still include files in src/ root and src/utils/
      (ok (some (lambda (p) (search "src/main.lisp" (namestring p))) results))
      (ok (some (lambda (p) (search "src/utils/" (namestring p))) results))))

  (testing "Exclude absolute directory path with trailing slash"
    (let* ((absolute-core-dir (merge-pathnames "src/core/" *test-dir*))
           (absolute-core-str (namestring absolute-core-dir))
           (results (glob:glob (merge-pathnames "**/*.lisp" *test-dir*)
                               :exclude absolute-core-str)))
      ;; Should exclude files under the absolute path
      (ok (not (some (lambda (p) (search "src/core/" (namestring p))) results)))
      ;; Should include other files
      (ok (some (lambda (p) (search "src/main.lisp" (namestring p))) results))))

  (testing "Exclude relative path pattern (auto-prefixed with **/)"
    (let ((results (glob:glob (merge-pathnames "**/*.lisp" *test-dir*)
                              :exclude "core/*.lisp")))
      ;; Should exclude files matching core/*.lisp at any depth
      (ok (not (some (lambda (p) (search "/core/" (namestring p))) results)))
      ;; Should include files in other directories
      (ok (some (lambda (p) (search "src/main.lisp" (namestring p))) results))
      (ok (some (lambda (p) (search "/utils/" (namestring p))) results))))

  (testing "Exclude pattern with explicit **/"
    (let ((results (glob:glob (merge-pathnames "**/*.lisp" *test-dir*)
                              :exclude "**/core/*.lisp")))
      ;; Should exclude files in core/ at any depth
      (ok (not (some (lambda (p) (search "/core/" (namestring p))) results)))
      ;; Should include other files
      (ok (some (lambda (p) (search "src/main.lisp" (namestring p))) results))))

  (testing "Exclude with leading */ (converted to **/)"
    (let ((results (glob:glob (merge-pathnames "**/*.lisp" *test-dir*)
                              :exclude "*/core/*.lisp")))
      ;; Should exclude files matching */core/*.lisp at any depth
      (ok (not (some (lambda (p) (search "/core/" (namestring p))) results)))
      ;; Should include files in src/ root
      (ok (some (lambda (p) (search "src/main.lisp" (namestring p))) results))))

  (testing "Exclude absolute path (literal match)"
    (let* ((absolute-file (merge-pathnames "src/main.lisp" *test-dir*))
           (absolute-file-str (namestring absolute-file))
           (results (glob:glob (merge-pathnames "**/*.lisp" *test-dir*)
                               :exclude absolute-file-str)))
      ;; Should exclude only the specific absolute path
      (ok (not (some (lambda (p) (equal (namestring p) absolute-file-str)) results)))
      ;; Should include other files
      (ok (some (lambda (p) (search "/core/" (namestring p))) results))
      (ok (some (lambda (p) (search "/utils/" (namestring p))) results))))

  (testing "Exclude pattern ending with /** (recursive)"
    (let ((results (glob:glob (merge-pathnames "**/*.lisp" *test-dir*)
                              :exclude "**/core/**")))
      ;; Should exclude ALL files under core/ recursively
      (ok (not (some (lambda (p) (search "/core/" (namestring p))) results)))
      ;; Should include other files
      (ok (some (lambda (p) (search "src/main.lisp" (namestring p))) results))
      (ok (some (lambda (p) (search "/utils/" (namestring p))) results))))

  (testing "Exclude dir/ should match same as dir/**"
    (let ((results-with-slash (glob:glob (merge-pathnames "**/*.lisp" *test-dir*)
                                         :exclude "core/"))
          (results-with-doublestar (glob:glob (merge-pathnames "**/*.lisp" *test-dir*)
                                              :exclude "**/core/**")))
      ;; Both should exclude all files in core/ recursively
      (ok (not (some (lambda (p) (search "/core/" (namestring p))) results-with-slash)))
      (ok (not (some (lambda (p) (search "/core/" (namestring p))) results-with-doublestar)))
      ;; Both should have the same results
      (ok (= (length results-with-slash) (length results-with-doublestar)))))

  (testing "Exclude **/dir/** should match same as **/dir/**/* (Case 1: nested **/ with wildcards)"
    (let ((results-without-star (glob:glob (merge-pathnames "**/*.lisp" *test-dir*)
                                           :exclude "**/core/**"))
          (results-with-star (glob:glob (merge-pathnames "**/*.lisp" *test-dir*)
                                        :exclude "**/core/**/*")))
      ;; Both should exclude all files in core/ recursively
      (ok (not (some (lambda (p) (search "/core/" (namestring p))) results-without-star)))
      (ok (not (some (lambda (p) (search "/core/" (namestring p))) results-with-star)))
      ;; Both should produce identical results
      (ok (= (length results-without-star) (length results-with-star))))))
