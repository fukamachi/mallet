(defpackage #:mallet/tests/parser/interpolation-extractor
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:parser #:mallet/parser)))
(in-package #:mallet/tests/parser/interpolation-extractor)

(deftest extract-simple-variable
  (testing "Simple ${var} extracts the symbol as a string"
    (let ((forms (parser:extract-interpolation-forms "${name}")))
      (ok (= 1 (length forms)))
      ;; string-parse-result-client returns symbols as strings, unqualified
      ;; symbols are returned as "CURRENT:NAME" (eclector convention)
      (ok (stringp (first forms)))
      (ok (search "NAME" (string-upcase (first forms))))))

  (testing "Symbol from ${x} is a string"
    (let ((forms (parser:extract-interpolation-forms "${x}")))
      (ok (= 1 (length forms)))
      (ok (stringp (first forms)))
      (ok (search "X" (string-upcase (first forms))))))

  (testing "Plain string with no interpolation returns empty list"
    (let ((forms (parser:extract-interpolation-forms "Hello, world!")))
      (ok (null forms))))

  (testing "Empty string returns empty list"
    (let ((forms (parser:extract-interpolation-forms "")))
      (ok (null forms)))))

(deftest extract-escaped-dollar
  (testing "Backslash before dollar does not suppress interpolation (cl:read already consumed one escape level)"
    ;; In source: #?"\\${name}" → cl:read yields string "\${name}"
    ;; The backslash is literal text here; ${name} is still interpolation.
    (let ((forms (parser:extract-interpolation-forms "\\${name}")))
      (ok (= 1 (length forms)))
      (ok (stringp (first forms)))
      (ok (search "NAME" (string-upcase (first forms))))))

  (testing "Backslash-dollar followed by real interpolation extracts both"
    (let ((forms (parser:extract-interpolation-forms "\\${first} ${second}")))
      (ok (= 2 (length forms)))
      (ok (stringp (first forms)))
      (ok (stringp (second forms))))))

(deftest extract-empty-interpolation
  (testing "Empty ${} is skipped"
    (let ((forms (parser:extract-interpolation-forms "${}")))
      (ok (null forms))))

  (testing "Whitespace-only ${   } is skipped"
    (let ((forms (parser:extract-interpolation-forms "${ }")))
      (ok (null forms))))

  (testing "Empty @{} is skipped"
    (let ((forms (parser:extract-interpolation-forms "@{}")))
      (ok (null forms)))))

(deftest extract-complex-expression
  (testing "${(+ x y)} extracts the compound form"
    (let ((forms (parser:extract-interpolation-forms "${(+ x y)}")))
      (ok (= 1 (length forms)))
      (ok (consp (first forms)))))

  (testing "${(format nil \"~a\" name)} extracts the form"
    (let ((forms (parser:extract-interpolation-forms "${(format nil \"~a\" name)}")))
      (ok (= 1 (length forms)))
      (ok (consp (first forms)))))

  (testing "Nested braces in ${(let ((x 1)) x)} are handled"
    (let ((forms (parser:extract-interpolation-forms "${(let ((x 1)) x)}")))
      (ok (= 1 (length forms)))
      (ok (consp (first forms)))))

  (testing "Brace inside string literal in ${(format nil \"a{b}\" x)} is not confused as nesting"
    (let ((forms (parser:extract-interpolation-forms "${(format nil \"a{b}\" x)}")))
      ;; Should extract 1 form — the brace inside the string should not confuse depth
      (ok (= 1 (length forms)))
      (ok (consp (first forms))))))

(deftest extract-at-interpolation
  (testing "@{format nil \"~a\" x} extracts multiple forms"
    (let ((forms (parser:extract-interpolation-forms "@{format nil \"~a\" x}")))
      ;; Should extract: "FORMAT", NIL, "~a", "X"  (or similar)
      (ok (>= (length forms) 2))))

  (testing "@{func arg1 arg2} extracts all forms"
    (let ((forms (parser:extract-interpolation-forms "@{my-func 42 \"hello\"}")))
      (ok (>= (length forms) 1)))))

(deftest extract-multiple-interpolations
  (testing "Multiple ${...} in one string"
    (let ((forms (parser:extract-interpolation-forms "Hello ${first} and ${last}!")))
      (ok (= 2 (length forms)))
      (ok (search "FIRST" (string-upcase (first forms))))
      (ok (search "LAST" (string-upcase (second forms))))))

  (testing "Mix of ${} and @{} in one string"
    (let ((forms (parser:extract-interpolation-forms "Val: ${x} List: @{items a b}")))
      ;; Should have x + items + a + b
      (ok (>= (length forms) 2))))

  (testing "Interpolations separated by text"
    (let ((forms (parser:extract-interpolation-forms "prefix ${a} middle ${b} suffix")))
      (ok (= 2 (length forms)))))

  (testing "Adjacent interpolations"
    (let ((forms (parser:extract-interpolation-forms "${a}${b}")))
      (ok (= 2 (length forms))))))

(deftest extract-malformed-patterns
  (testing "Unclosed brace ${ without } skips"
    (let ((forms (parser:extract-interpolation-forms "${unclosed")))
      ;; No matching brace, so no forms extracted
      (ok (null forms))))

  (testing "Dollar without brace is ignored"
    (let ((forms (parser:extract-interpolation-forms "cost is $10")))
      (ok (null forms))))

  (testing "At sign without brace is ignored"
    (let ((forms (parser:extract-interpolation-forms "email@example.com")))
      (ok (null forms)))))
