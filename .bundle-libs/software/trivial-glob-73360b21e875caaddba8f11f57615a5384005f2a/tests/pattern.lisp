(defpackage #:trivial-glob/tests/pattern
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:pattern #:trivial-glob/pattern)))
(in-package #:trivial-glob/tests/pattern)

(deftest literal-matching
  (testing "Exact string matching"
    (ok (pattern:match-pattern "hello" "hello"))
    (ok (pattern:match-pattern "test.txt" "test.txt"))
    (ok (not (pattern:match-pattern "hello" "world")))
    (ok (not (pattern:match-pattern "test.txt" "test.log")))))

(deftest star-wildcard
  (testing "* matches zero or more characters"
    (ok (pattern:match-pattern "*.txt" "file.txt"))
    (ok (pattern:match-pattern "*.txt" "test.txt"))
    (ok (pattern:match-pattern "test*" "test"))
    (ok (pattern:match-pattern "test*" "test123"))
    (ok (pattern:match-pattern "*test*" "mytest123"))
    (ok (not (pattern:match-pattern "*.txt" "file.log")))
    (ok (not (pattern:match-pattern "test*" "tes")))))

(deftest question-wildcard
  (testing "? matches exactly one character"
    (ok (pattern:match-pattern "test?.txt" "test1.txt"))
    (ok (pattern:match-pattern "test?.txt" "testa.txt"))
    (ok (pattern:match-pattern "???" "abc"))
    (ok (not (pattern:match-pattern "test?.txt" "test.txt")))
    (ok (not (pattern:match-pattern "test?.txt" "test12.txt")))
    (ok (not (pattern:match-pattern "???" "ab")))))

(deftest bracket-expressions
  (testing "[ ] matches character sets"
    (ok (pattern:match-pattern "[abc]" "a"))
    (ok (pattern:match-pattern "[abc]" "b"))
    (ok (pattern:match-pattern "[abc]" "c"))
    (ok (not (pattern:match-pattern "[abc]" "d")))
    (ok (pattern:match-pattern "test[123].txt" "test1.txt"))
    (ok (pattern:match-pattern "test[123].txt" "test2.txt"))
    (ok (not (pattern:match-pattern "test[123].txt" "test4.txt"))))

  (testing "Character ranges"
    (ok (pattern:match-pattern "[a-z]" "m"))
    (ok (pattern:match-pattern "[0-9]" "5"))
    (ok (pattern:match-pattern "[A-Z]" "X"))
    (ok (not (pattern:match-pattern "[a-z]" "A")))
    (ok (not (pattern:match-pattern "[0-9]" "a"))))

  (testing "Negation with [!] or [^]"
    (ok (pattern:match-pattern "[!abc]" "d"))
    (ok (pattern:match-pattern "[!abc]" "z"))
    (ok (not (pattern:match-pattern "[!abc]" "a")))
    (ok (not (pattern:match-pattern "[!abc]" "b")))
    (ok (pattern:match-pattern "[^0-9]" "a"))
    (ok (not (pattern:match-pattern "[^0-9]" "5")))))

(deftest character-classes
  (testing "[:alnum:] matches alphanumeric"
    (ok (pattern:match-pattern "[[:alnum:]]" "a"))
    (ok (pattern:match-pattern "[[:alnum:]]" "Z"))
    (ok (pattern:match-pattern "[[:alnum:]]" "5"))
    (ok (not (pattern:match-pattern "[[:alnum:]]" "!")))
    (ok (not (pattern:match-pattern "[[:alnum:]]" " "))))

  (testing "[:alpha:] matches letters only"
    (ok (pattern:match-pattern "[[:alpha:]]" "a"))
    (ok (pattern:match-pattern "[[:alpha:]]" "Z"))
    (ok (not (pattern:match-pattern "[[:alpha:]]" "5")))
    (ok (not (pattern:match-pattern "[[:alpha:]]" "!"))))

  (testing "[:digit:] matches digits only"
    (ok (pattern:match-pattern "[[:digit:]]" "0"))
    (ok (pattern:match-pattern "[[:digit:]]" "9"))
    (ok (not (pattern:match-pattern "[[:digit:]]" "a")))
    (ok (not (pattern:match-pattern "[[:digit:]]" "!"))))

  (testing "[:lower:] and [:upper:]"
    (ok (pattern:match-pattern "[[:lower:]]" "a"))
    (ok (pattern:match-pattern "[[:lower:]]" "z"))
    (ok (not (pattern:match-pattern "[[:lower:]]" "A")))
    (ok (pattern:match-pattern "[[:upper:]]" "A"))
    (ok (pattern:match-pattern "[[:upper:]]" "Z"))
    (ok (not (pattern:match-pattern "[[:upper:]]" "a"))))

  (testing "[:space:] matches whitespace"
    (ok (pattern:match-pattern "[[:space:]]" " "))
    (ok (pattern:match-pattern "[[:space:]]" (string #\Tab)))
    (ok (pattern:match-pattern "[[:space:]]" (string #\Newline)))
    (ok (not (pattern:match-pattern "[[:space:]]" "a"))))

  (testing "[:xdigit:] matches hex digits"
    (ok (pattern:match-pattern "[[:xdigit:]]" "0"))
    (ok (pattern:match-pattern "[[:xdigit:]]" "9"))
    (ok (pattern:match-pattern "[[:xdigit:]]" "a"))
    (ok (pattern:match-pattern "[[:xdigit:]]" "F"))
    (ok (not (pattern:match-pattern "[[:xdigit:]]" "g")))
    (ok (not (pattern:match-pattern "[[:xdigit:]]" "Z"))))

  (testing "Character classes in complex patterns"
    (ok (pattern:match-pattern "test[[:digit:]].txt" "test5.txt"))
    (ok (pattern:match-pattern "[[:alpha:]]*[[:digit:]]" "abc123"))
    (ok (not (pattern:match-pattern "[[:digit:]]*" "abc")))))
