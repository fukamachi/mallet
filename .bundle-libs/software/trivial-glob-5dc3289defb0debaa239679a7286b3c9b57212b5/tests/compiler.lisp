(defpackage #:trivial-glob/tests/compiler
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:compiler #:trivial-glob/compiler)))
(in-package #:trivial-glob/tests/compiler)

(deftest compile-literal-pattern
  (testing "Compile simple literal pattern"
    (let ((matcher (compiler:compile-pattern "hello")))
      (ok (functionp matcher))
      (ok (funcall matcher "hello"))
      (ng (funcall matcher "world"))
      (ng (funcall matcher "hello world"))))

  (testing "Compile literal with extension"
    (let ((matcher (compiler:compile-pattern "test.txt")))
      (ok (functionp matcher))
      (ok (funcall matcher "test.txt"))
      (ng (funcall matcher "test.log"))
      (ng (funcall matcher "test")))))

(deftest compile-star-wildcard
  (testing "Compile * wildcard"
    (let ((matcher (compiler:compile-pattern "*.txt")))
      (ok (functionp matcher))
      (ok (funcall matcher "file.txt"))
      (ok (funcall matcher "test.txt"))
      (ok (funcall matcher ".txt"))
      (ng (funcall matcher "file.log"))
      (ng (funcall matcher "txt"))))

  (testing "Compile pattern with * in middle"
    (let ((matcher (compiler:compile-pattern "test*file")))
      (ok (functionp matcher))
      (ok (funcall matcher "testfile"))
      (ok (funcall matcher "test123file"))
      (ok (funcall matcher "test_my_file"))
      (ng (funcall matcher "test"))
      (ng (funcall matcher "file"))
      (ng (funcall matcher "mytest123file"))))

  (testing "Multiple * wildcards"
    (let ((matcher (compiler:compile-pattern "*.*")))
      (ok (functionp matcher))
      (ok (funcall matcher "file.txt"))
      (ok (funcall matcher "a.b"))
      (ng (funcall matcher "noextension"))
      ;; Note: *.* DOES match "." (empty name + dot + empty ext)
      ;; This matches bash glob and original trivial-glob behavior
      (ok (funcall matcher ".")))))

(deftest compile-question-wildcard
  (testing "Compile ? wildcard"
    (let ((matcher (compiler:compile-pattern "test?.txt")))
      (ok (functionp matcher))
      (ok (funcall matcher "test1.txt"))
      (ok (funcall matcher "testa.txt"))
      (ng (funcall matcher "test.txt"))
      (ng (funcall matcher "test12.txt"))))

  (testing "Multiple ? wildcards"
    (let ((matcher (compiler:compile-pattern "???")))
      (ok (functionp matcher))
      (ok (funcall matcher "abc"))
      (ok (funcall matcher "123"))
      (ng (funcall matcher "ab"))
      (ng (funcall matcher "abcd")))))

(deftest compile-bracket-expression
  (testing "Simple character set [abc]"
    (let ((matcher (compiler:compile-pattern "[abc]")))
      (ok (functionp matcher))
      (ok (funcall matcher "a"))
      (ok (funcall matcher "b"))
      (ok (funcall matcher "c"))
      (ng (funcall matcher "d"))
      (ng (funcall matcher "ab"))))

  (testing "Character range [a-z]"
    (let ((matcher (compiler:compile-pattern "[a-z]")))
      (ok (functionp matcher))
      (ok (funcall matcher "a"))
      (ok (funcall matcher "m"))
      (ok (funcall matcher "z"))
      (ng (funcall matcher "A"))
      (ng (funcall matcher "0"))))

  (testing "Negated character class [!abc]"
    (let ((matcher (compiler:compile-pattern "[!abc]")))
      (ok (functionp matcher))
      (ok (funcall matcher "d"))
      (ok (funcall matcher "x"))
      (ok (funcall matcher "1"))
      (ng (funcall matcher "a"))
      (ng (funcall matcher "b"))))

  (testing "POSIX character class [[:digit:]]"
    (let ((matcher (compiler:compile-pattern "[[:digit:]]")))
      (ok (functionp matcher))
      (ok (funcall matcher "0"))
      (ok (funcall matcher "5"))
      (ok (funcall matcher "9"))
      (ng (funcall matcher "a"))
      (ng (funcall matcher "!"))))

  (testing "POSIX alpha class [[:alpha:]]"
    (let ((matcher (compiler:compile-pattern "[[:alpha:]]")))
      (ok (functionp matcher))
      (ok (funcall matcher "a"))
      (ok (funcall matcher "Z"))
      (ng (funcall matcher "0"))
      (ng (funcall matcher "!"))))

  (testing "Mixed bracket expression [a-z0-9_]"
    (let ((matcher (compiler:compile-pattern "[a-z0-9_]")))
      (ok (functionp matcher))
      (ok (funcall matcher "a"))
      (ok (funcall matcher "z"))
      (ok (funcall matcher "0"))
      (ok (funcall matcher "9"))
      (ok (funcall matcher "_"))
      (ng (funcall matcher "A"))
      (ng (funcall matcher "!")))))

(deftest compile-brace-expansion
  (testing "Simple brace expansion {a,b}"
    (let ((matcher (compiler:compile-pattern "file{1,2}.txt")))
      (ok (functionp matcher))
      (ok (funcall matcher "file1.txt"))
      (ok (funcall matcher "file2.txt"))
      (ng (funcall matcher "file3.txt"))
      (ng (funcall matcher "file.txt"))))

  (testing "Brace with extensions {txt,log}"
    (let ((matcher (compiler:compile-pattern "*.{txt,log}")))
      (ok (functionp matcher))
      (ok (funcall matcher "file.txt"))
      (ok (funcall matcher "file.log"))
      (ng (funcall matcher "file.asd"))
      (ng (funcall matcher "file"))))

  (testing "Multiple brace groups {a,b}.{c,d}"
    (let ((matcher (compiler:compile-pattern "file{1,2}.{txt,log}")))
      (ok (functionp matcher))
      (ok (funcall matcher "file1.txt"))
      (ok (funcall matcher "file1.log"))
      (ok (funcall matcher "file2.txt"))
      (ok (funcall matcher "file2.log"))
      (ng (funcall matcher "file3.txt"))
      (ng (funcall matcher "file1.asd"))))

  (testing "Single option brace {a}"
    (let ((matcher (compiler:compile-pattern "file{1}.txt")))
      (ok (functionp matcher))
      (ok (funcall matcher "file1.txt"))
      (ng (funcall matcher "file2.txt")))))

(deftest compile-escape-sequences
  (testing "Escape special characters"
    (let ((matcher (compiler:compile-pattern "test\\*.txt")))
      (ok (functionp matcher))
      (ok (funcall matcher "test*.txt"))
      (ng (funcall matcher "test1.txt"))
      (ng (funcall matcher "testany.txt"))))

  (testing "Escape backslash"
    (let ((matcher (compiler:compile-pattern "test\\\\.txt")))
      (ok (functionp matcher))
      (ok (funcall matcher "test\\.txt"))
      (ng (funcall matcher "test.txt")))))

(deftest compile-complex-patterns
  (testing "Complex pattern with wildcards and braces"
    (let ((matcher (compiler:compile-pattern "*.{lisp,asd}")))
      (ok (functionp matcher))
      (ok (funcall matcher "file.lisp"))
      (ok (funcall matcher "file.asd"))
      (ok (funcall matcher "my-project.asd"))
      (ng (funcall matcher "file.txt"))))

  (testing "Pattern with character class and wildcard"
    (let ((matcher (compiler:compile-pattern "test[0-9]*")))
      (ok (functionp matcher))
      (ok (funcall matcher "test0"))
      (ok (funcall matcher "test5abc"))
      (ok (funcall matcher "test9xyz"))
      (ng (funcall matcher "test"))
      (ng (funcall matcher "testa123"))))

  (testing "Pattern with everything"
    (let ((matcher (compiler:compile-pattern "test[0-9]?*.{txt,log}")))
      (ok (functionp matcher))
      (ok (funcall matcher "test0a.txt"))
      (ok (funcall matcher "test5xyz.log"))
      (ng (funcall matcher "test.txt"))
      (ng (funcall matcher "test01.asd")))))

(deftest compile-with-options
  (testing "Default behavior (shell-like): * should not match /"
    (let ((matcher (compiler:compile-pattern "*.txt")))
      (ok (functionp matcher))
      (ok (funcall matcher "file.txt"))
      ;; By default, * should not match / (shell-like behavior)
      (ng (funcall matcher "dir/file.txt"))))

  (testing "Compile with match-slash option"
    (let ((matcher (compiler:compile-pattern "*.txt" :match-slash t)))
      (ok (functionp matcher))
      (ok (funcall matcher "file.txt"))
      ;; With match-slash t, * can match /
      (ok (funcall matcher "dir/file.txt"))))

  (testing "Compile with period option"
    (let ((matcher (compiler:compile-pattern "*.txt" :period t)))
      (ok (functionp matcher))
      (ok (funcall matcher "file.txt"))
      ;; In period mode, * should not match leading .
      (ng (funcall matcher ".hidden.txt"))))

  (testing "Compile with casefold option"
    (let ((matcher (compiler:compile-pattern "*.txt" :casefold t)))
      (ok (functionp matcher))
      (ok (funcall matcher "file.txt"))
      (ok (funcall matcher "file.TXT"))
      (ok (funcall matcher "FILE.txt"))
      (ok (funcall matcher "FILE.TXT")))))

(deftest compile-doublestar-patterns
  (testing "** matches zero or more directory levels"
    (let ((matcher (compiler:compile-pattern "**/file.txt")))
      (ok (functionp matcher))
      ;; Zero directories
      (ok (funcall matcher "file.txt"))
      ;; One directory
      (ok (funcall matcher "a/file.txt"))
      ;; Multiple directories
      (ok (funcall matcher "a/b/file.txt"))
      (ok (funcall matcher "a/b/c/file.txt"))))

  (testing "**/*.txt matches .txt files at any depth"
    (let ((matcher (compiler:compile-pattern "**/*.txt")))
      (ok (functionp matcher))
      (ok (funcall matcher "file.txt"))
      (ok (funcall matcher "dir/file.txt"))
      (ok (funcall matcher "a/b/c/file.txt"))
      (ng (funcall matcher "file.log"))))

  (testing "dir/**/*.txt matches .txt files under dir at any depth"
    (let ((matcher (compiler:compile-pattern "src/**/*.lisp")))
      (ok (functionp matcher))
      ;; Direct child
      (ok (funcall matcher "src/main.lisp"))
      ;; Nested children
      (ok (funcall matcher "src/core/parser.lisp"))
      (ok (funcall matcher "src/core/types.lisp"))
      (ok (funcall matcher "src/utils/helpers.lisp"))
      ;; Wrong directory
      (ng (funcall matcher "tests/main.lisp"))
      ;; Wrong extension
      (ng (funcall matcher "src/main.txt"))))

  (testing "**/dir/**/* matches all files recursively in any dir subdirectory"
    (let ((matcher (compiler:compile-pattern "**/core/**/*")))
      (ok (functionp matcher))
      ;; Direct children of core
      (ok (funcall matcher "core/file.txt"))
      (ok (funcall matcher "src/core/parser.lisp"))
      ;; Nested children
      (ok (funcall matcher "core/sub/file.txt"))
      (ok (funcall matcher "src/core/sub/parser.lisp"))
      (ok (funcall matcher "a/b/core/c/d/file.txt"))
      ;; Not in core directory
      (ng (funcall matcher "src/main.lisp"))
      (ng (funcall matcher "src/utils/helpers.lisp"))))

  (testing "**/dir/** matches all files recursively in any dir subdirectory"
    (let ((matcher (compiler:compile-pattern "**/core/**")))
      (ok (functionp matcher))
      ;; Direct children of core
      (ok (funcall matcher "core/file.txt"))
      (ok (funcall matcher "src/core/parser.lisp"))
      ;; Nested children
      (ok (funcall matcher "core/sub/file.txt"))
      (ok (funcall matcher "src/core/sub/parser.lisp"))
      ;; Not in core directory
      (ng (funcall matcher "src/main.lisp")))))

(deftest normalize-path-pattern
  (testing "Filename-only patterns unchanged"
    (ok (string= "**/*.log" (compiler::normalize-path-pattern "*.log")))
    (ok (string= "**/test*.txt" (compiler::normalize-path-pattern "test*.txt"))))

  (testing "Trailing slash converted to /** and prefixed with **/"
    ;; Trailing / becomes /**, then relative path gets **/ prefix
    (ok (string= "**/build/**" (compiler::normalize-path-pattern "build/")))
    (ok (string= "**/src/core/**" (compiler::normalize-path-pattern "src/core/"))))

  (testing "Relative paths prefixed with **/"
    (ok (string= "**/build/*.log" (compiler::normalize-path-pattern "build/*.log")))
    (ok (string= "**/src/core/*.lisp" (compiler::normalize-path-pattern "src/core/*.lisp"))))

  (testing "Leading */ converted to **/"
    (ok (string= "**/core/*.lisp" (compiler::normalize-path-pattern "*/core/*.lisp")))
    (ok (string= "**/utils/**" (compiler::normalize-path-pattern "*/utils/**"))))

  (testing "Absolute paths unchanged"
    (ok (string= "/tmp/out.txt" (compiler::normalize-path-pattern "/tmp/out.txt")))
    (ok (string= "/var/log/*.log" (compiler::normalize-path-pattern "/var/log/*.log"))))

  (testing "Patterns already starting with **/ unchanged"
    (ok (string= "**/build/**" (compiler::normalize-path-pattern "**/build/**")))
    (ok (string= "**/core/*.lisp" (compiler::normalize-path-pattern "**/core/*.lisp"))))

  (testing "Combined transformations"
    ;; Trailing / + relative path
    (ok (string= "**/build/**" (compiler::normalize-path-pattern "build/")))
    ;; */ + trailing /
    (ok (string= "**/core/**" (compiler::normalize-path-pattern "*/core/")))))
