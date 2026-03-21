(defpackage #:mallet/tests/engine-coalton-lisp
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:engine #:mallet/engine)))
(in-package #:mallet/tests/engine-coalton-lisp)

;;; Tests for extract-lisp-bodies-from-coalton.
;;;
;;; The Coalton `lisp` escape form has the structure:
;;;   (lisp ReturnType (captured-vars...) cl-body1 cl-body2 ...)
;;;
;;; The function walks a coalton expression tree and collects CL body
;;; forms (positions 3+) from every `lisp` form found at any nesting depth.
;;;
;;; Signature: (extract-lisp-bodies-from-coalton expr position-map
;;;                                               fallback-line fallback-column)
;;; Returns: flat list of (body-expr line column) entries.
;;; Non-cons body expressions (atoms) are skipped.

;;; Helpers

(defun empty-posmap ()
  "Return an empty position-map hash table."
  (make-hash-table :test 'eq))

(defun extract (expr &key (line 1) (column 0) (posmap (empty-posmap)))
  "Convenience wrapper: extract lisp bodies from EXPR with given fallbacks."
  (engine:extract-lisp-bodies-from-coalton expr posmap line column))

;;; Atom and empty-list inputs

(deftest extract-lisp-bodies-atom-inputs
  (testing "nil returns nil"
    (ok (null (extract nil))))

  (testing "a symbol string atom returns nil"
    (ok (null (extract "CURRENT:X"))))

  (testing "an integer atom returns nil"
    (ok (null (extract 42))))

  (testing "empty list returns nil"
    (ok (null (extract '())))))

;;; Non-lisp conses

(deftest extract-lisp-bodies-non-lisp-forms
  (testing "a define form with no nested lisp returns nil"
    (ok (null (extract
               '("CURRENT:DEFINE" ("CURRENT:FOO") "CURRENT:X")))))

  (testing "a match form with no nested lisp returns nil"
    (ok (null (extract
               '("CURRENT:MATCH" "CURRENT:X"
                 ("CURRENT:True" "CURRENT:E1")
                 ("CURRENT:False" "CURRENT:E2"))))))

  (testing "a bare list of atoms returns nil"
    (ok (null (extract '(1 2 3)))))

  (testing "string 'lisp' without colon (not a symbol) is not matched"
    ;; A raw string with no colon is treated as a string literal, not a symbol.
    (ok (null (extract '("lisp" "CURRENT:Integer" () (+ 1 2)))))))

;;; Simple lisp form detection

(deftest extract-lisp-bodies-simple-lisp-form
  (testing "minimal lisp form with one cons body returns one entry"
    (let* ((body-expr '(+ 1 2))
           (result (extract `("CURRENT:LISP" "CURRENT:Integer" () ,body-expr))))
      (ok (= (length result) 1))
      (ok (equal (first (first result)) body-expr))))

  (testing "lisp form with multiple cons bodies returns all entries"
    (let* ((b1 '(let ((x 1)) x))
           (b2 '(+ x 2))
           (result (extract `("CURRENT:LISP" "CURRENT:Integer" () ,b1 ,b2))))
      (ok (= (length result) 2))
      (ok (equal (first (first result)) b1))
      (ok (equal (first (second result)) b2))))

  (testing "lisp form with no body returns nil"
    (ok (null (extract '("CURRENT:LISP" "CURRENT:Integer" ())))))

  (testing "package-qualified coalton:lisp is matched"
    (let* ((body-expr '(+ 1 0))
           (result (extract `("COALTON:LISP" "CURRENT:String" () ,body-expr))))
      (ok (= (length result) 1))
      (ok (equal (first (first result)) body-expr))))

  (testing "case-insensitive: lowercase lisp head is matched"
    (let* ((body-expr '(+ 0 1))
           (result (extract `("CURRENT:lisp" "CURRENT:Integer" () ,body-expr))))
      (ok (= (length result) 1)))))

;;; Atom body expressions are skipped

(deftest extract-lisp-bodies-skips-atoms
  (testing "bare symbol body is skipped"
    ;; (lisp T () some-symbol) — the body is an atom, not a cons
    (ok (null (extract '("CURRENT:LISP" "CURRENT:Unit" () "CURRENT:SOME-SYMBOL")))))

  (testing "bare integer body is skipped"
    (ok (null (extract '("CURRENT:LISP" "CURRENT:Integer" () 42)))))

  (testing "mixed: cons body kept, atom body skipped"
    (let* ((cons-body '(+ 1 2))
           (result (extract `("CURRENT:LISP" "CURRENT:Integer" ()
                              "CURRENT:ATOM" ,cons-body 99))))
      ;; Only the cons body should appear
      (ok (= (length result) 1))
      (ok (equal (first (first result)) cons-body)))))

;;; Positions from fallback and position-map

(deftest extract-lisp-bodies-fallback-positions
  (testing "fallback line and column are used when body-expr not in position-map"
    (let* ((body-expr '(foo bar))
           (result (extract `("CURRENT:LISP" "CURRENT:T" () ,body-expr)
                            :line 10 :column 5)))
      (ok (= (length result) 1))
      (destructuring-bind (expr line col) (first result)
        (declare (ignore expr))
        (ok (= line 10))
        (ok (= col 5)))))

  (testing "position-map entry overrides fallback when body-expr is in the map"
    (let ((body-expr '(foo bar))
          (posmap (make-hash-table :test 'eq)))
      (setf (gethash body-expr posmap) (cons 42 7))
      (let ((result (engine:extract-lisp-bodies-from-coalton
                     `("CURRENT:LISP" "CURRENT:T" () ,body-expr)
                     posmap 1 0)))
        (ok (= (length result) 1))
        (destructuring-bind (expr line col) (first result)
          (declare (ignore expr))
          (ok (= line 42))
          (ok (= col 7)))))))

;;; Nested lisp forms inside Coalton expressions

(deftest extract-lisp-bodies-nested-in-define
  (testing "lisp form nested in a define body is found"
    (let* ((body-expr '(+ x 1))
           (result (extract
                    `("CURRENT:COALTON-TOPLEVEL"
                      ("CURRENT:DEFINE" ("CURRENT:FOO" "CURRENT:X")
                       ("CURRENT:LISP" "CURRENT:Integer" ("CURRENT:X") ,body-expr))))))
      (ok (= (length result) 1))
      (ok (equal (first (first result)) body-expr))))

  (testing "lisp form nested in a match clause is found"
    (let ((body1 '(1+ y))
          (body2 '(progn 0)))
      (let ((result (extract
                     `("CURRENT:MATCH" "CURRENT:OPT"
                       (("CURRENT:Some" "CURRENT:Y")
                        ("CURRENT:LISP" "CURRENT:Integer" ("CURRENT:Y") ,body1))
                       ("CURRENT:None"
                        ("CURRENT:LISP" "CURRENT:Integer" () ,body2))))))
        (ok (= (length result) 2))
        (ok (member body1 (mapcar #'first result) :test #'equal))
        (ok (member body2 (mapcar #'first result) :test #'equal))))))

;;; Multiple lisp forms

(deftest extract-lisp-bodies-multiple-forms
  (testing "two sibling lisp forms both contribute bodies"
    (let* ((b1 '(+ 1 2))
           (b2 '(string-upcase "foo"))
           (result (extract
                    `("CURRENT:COALTON-TOPLEVEL"
                      ("CURRENT:LISP" "CURRENT:Integer" () ,b1)
                      ("CURRENT:LISP" "CURRENT:String" () ,b2)))))
      (ok (= (length result) 2))
      (ok (member b1 (mapcar #'first result) :test #'equal))
      (ok (member b2 (mapcar #'first result) :test #'equal))))

  (testing "multiple lisp forms in different defines are all collected"
    (let ((b1 '(+ 1 1))
          (b2 '(string "bar")))
      (let ((result (extract
                     `("CURRENT:COALTON-TOPLEVEL"
                       ("CURRENT:DEFINE" ("CURRENT:F") ("CURRENT:LISP" "CURRENT:Integer" () ,b1))
                       ("CURRENT:DEFINE" ("CURRENT:G") ("CURRENT:LISP" "CURRENT:String" () ,b2))))))
        (ok (= (length result) 2))
        (ok (member b1 (mapcar #'first result) :test #'equal))
        (ok (member b2 (mapcar #'first result) :test #'equal))))))

;;; Deep nesting

(deftest extract-lisp-bodies-deep-nesting
  (testing "lisp forms found at arbitrary nesting depth"
    (let ((b1 '(when (> y 0) y))
          (b2 '(progn -1)))
      (let ((result (extract
                     `("CURRENT:COALTON-TOPLEVEL"
                       ("CURRENT:DEFINE" ("CURRENT:FOO" "CURRENT:X")
                        ("CURRENT:MATCH" "CURRENT:X"
                         (("CURRENT:Some" "CURRENT:Y")
                          ("CURRENT:IF" "CURRENT:COND"
                           ("CURRENT:LISP" "CURRENT:Integer" ("CURRENT:Y") ,b1)
                           ("CURRENT:LISP" "CURRENT:Integer" () ,b2)))))))))
        (ok (= (length result) 2))
        (ok (member b1 (mapcar #'first result) :test #'equal))
        (ok (member b2 (mapcar #'first result) :test #'equal))))))

;;; Edge cases

(deftest extract-lisp-bodies-coalton-toplevel-no-lisp
  (testing "coalton-toplevel with defines but no lisp forms returns nil"
    (ok (null (extract
               '("CURRENT:COALTON-TOPLEVEL"
                 ("CURRENT:DEFINE" ("CURRENT:FOO" "CURRENT:X")
                  ("CURRENT:+" "CURRENT:X" 1))
                 ("CURRENT:DEFINE" ("CURRENT:BAR")
                  "CURRENT:FOO")))))))

(deftest extract-lisp-bodies-malformed-short-lisp
  (testing "lisp form with only head and type (no vars list) returns nil"
    ;; (lisp Integer) — malformed, should not crash
    (ok (null (extract '("CURRENT:LISP" "CURRENT:Integer")))))

  (testing "lisp form with only head (bare) returns nil"
    (ok (null (extract '("CURRENT:LISP"))))))

(deftest extract-lisp-bodies-nested-in-let
  (testing "lisp form inside Coalton let binding is found"
    (let* ((body-expr '(1+ x))
           (result (extract
                    `("CURRENT:COALTON-TOPLEVEL"
                      ("CURRENT:DEFINE" ("CURRENT:FOO" "CURRENT:X")
                       ("CURRENT:LET" (("CURRENT:Y"
                                        ("CURRENT:LISP" "CURRENT:Integer" ("CURRENT:X")
                                         ,body-expr)))
                        "CURRENT:Y"))))))
      (ok (= (length result) 1))
      (ok (equal (first (first result)) body-expr)))))

(deftest extract-lisp-bodies-deeply-nested-let-match-if
  (testing "lisp forms inside let > match > if at depth 4+ are all found"
    (let ((b1 '(format nil "~A" z))
          (b2 '(concatenate 'string "neg"))
          (b3 '(values)))
      (let ((result (extract
                     `("CURRENT:COALTON-TOPLEVEL"
                       ("CURRENT:DEFINE" ("CURRENT:FOO" "CURRENT:X")
                        ("CURRENT:LET" (("CURRENT:Y" "CURRENT:X"))
                         ("CURRENT:MATCH" "CURRENT:Y"
                          (("CURRENT:Some" "CURRENT:Z")
                           ("CURRENT:IF" ("CURRENT:>" "CURRENT:Z" 0)
                            ("CURRENT:LISP" "CURRENT:String" ("CURRENT:Z") ,b1)
                            ("CURRENT:LISP" "CURRENT:String" () ,b2)))
                          ("CURRENT:None"
                           ("CURRENT:LISP" "CURRENT:Unit" () ,b3)))))))))
        (ok (= (length result) 3))
        (ok (member b1 (mapcar #'first result) :test #'equal))
        (ok (member b2 (mapcar #'first result) :test #'equal))
        (ok (member b3 (mapcar #'first result) :test #'equal))))))

(deftest extract-lisp-bodies-qualified-coalton-lisp-nested
  (testing "COALTON:LISP inside a nested define is found"
    (let* ((body-expr '(1+ x))
           (result (extract
                    `("CURRENT:COALTON-TOPLEVEL"
                      ("CURRENT:DEFINE" ("CURRENT:FOO" "CURRENT:X")
                       ("COALTON:LISP" "CURRENT:Integer" ("CURRENT:X") ,body-expr))))))
      (ok (= (length result) 1))
      (ok (equal (first (first result)) body-expr))))

  (testing "fully-qualified package COALTON-LIBRARY/CLASSES:LISP is recognized"
    (let* ((body-expr '(+ 1 2))
           (result (extract
                    `("COALTON-LIBRARY/CLASSES:LISP" "CURRENT:Integer" () ,body-expr))))
      (ok (= (length result) 1))
      (ok (equal (first (first result)) body-expr)))))

(deftest extract-lisp-bodies-multiple-cons-bodies-single-form
  (testing "lisp form with three cons bodies returns all three"
    (let* ((b1 '(setf x 1))
           (b2 '(setf y 2))
           (b3 '(+ x y))
           (result (extract
                    `("CURRENT:LISP" "CURRENT:Integer" ()
                      ,b1 ,b2 ,b3))))
      (ok (= (length result) 3))
      (ok (equal (first (first result)) b1))
      (ok (equal (first (second result)) b2))
      (ok (equal (first (third result)) b3)))))

;;; Stub guards (prevent trivially-passing implementations)

(deftest extract-lisp-bodies-stub-guard-not-always-nil
  (testing "must return non-nil for a real lisp form (not a stub always returning nil)"
    (let ((result (extract `("CURRENT:LISP" "CURRENT:Integer" () (+ 1 2)))))
      (ok (not (null result))
          "A stub always returning NIL fails this"))))

(deftest extract-lisp-bodies-stub-guard-not-always-list
  (testing "must return nil for an atom (not a stub always returning a list)"
    (ok (null (extract "CURRENT:NOT-LISP"))
        "A stub always returning a list fails this")))

(deftest extract-lisp-bodies-stub-guard-respects-depth
  (testing "must find lisp form at depth > 1 (not only at top level)"
    (let* ((body-expr '(+ 1 1))
           (result (extract
                    `("CURRENT:DEFINE" ("CURRENT:FOO")
                      ("CURRENT:LISP" "CURRENT:Integer" () ,body-expr)))))
      (ok (equal (first (first result)) body-expr)
          "A stub only checking the top-level form fails this"))))

(deftest extract-lisp-bodies-stub-guard-skips-atoms
  (testing "must skip non-cons body (not include every body regardless of type)"
    ;; A stub returning every body would include the atom 99.
    ;; Correct impl only returns conses.
    (let ((result (extract '("CURRENT:LISP" "CURRENT:Integer" () 99))))
      (ok (null result)
          "A stub that includes atom bodies fails this"))))

(deftest extract-lisp-bodies-stub-guard-uses-posmap
  (testing "must use position-map for position lookup (not always use fallback)"
    (let ((body-expr '(foo))
          (posmap (make-hash-table :test 'eq)))
      (setf (gethash body-expr posmap) (cons 99 88))
      (let ((result (engine:extract-lisp-bodies-from-coalton
                     `("CURRENT:LISP" "CURRENT:T" () ,body-expr)
                     posmap 1 0)))
        (destructuring-bind (expr line col) (first result)
          (declare (ignore expr))
          (ok (= line 99) "Line should come from posmap, not fallback")
          (ok (= col 88) "Column should come from posmap, not fallback"))))))
