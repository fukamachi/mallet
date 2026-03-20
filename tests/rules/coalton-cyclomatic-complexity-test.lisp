(defpackage #:mallet/tests/rules/coalton-cyclomatic-complexity
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:base #:mallet/rules/base)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/coalton-cyclomatic-complexity)

;;; Helper

(defun check-code (code &key (max 15) (variant :standard))
  "Run the cyclomatic-complexity rule on CODE and return all violations."
  (let ((forms (parser:parse-forms code #p"test.lisp"))
        (rule (make-instance 'rules:cyclomatic-complexity-rule
                             :max max
                             :variant variant)))
    (mapcan (lambda (form)
              (base:check-form rule form #p"test.lisp"))
            forms)))

;;; coalton-aware-p

(deftest cyclomatic-complexity-coalton-aware-p
  (testing "coalton-aware-p returns T for cyclomatic-complexity-rule"
    (ok (base:coalton-aware-p (make-instance 'rules:cyclomatic-complexity-rule)))))

;;; Backward compatibility — CL forms still work

(deftest cyclomatic-complexity-cl-backward-compat
  (testing "CL defun above threshold still flagged after Coalton extension"
    (let ((violations (check-code "(defun foo (x) (if x 1 2))" :max 1)))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :cyclomatic-complexity))))

  (testing "CL defun within threshold still passes after Coalton extension"
    (ok (null (check-code "(defun foo (x) (if x 1 2))" :max 2)))))

;;; Empty / trivial Coalton forms

(deftest cyclomatic-complexity-coalton-empty
  (testing "Empty coalton-toplevel produces no violations"
    (ok (null (check-code "(coalton-toplevel)" :max 1))))

  (testing "Coalton define with no branches has complexity 1 — no violation at max 1"
    (ok (null (check-code "(coalton-toplevel (define (foo x) x))" :max 1))))

  (testing "Coalton value define (no lambda list) is not flagged"
    (ok (null (check-code "(coalton-toplevel (define x 42))" :max 1)))))

;;; match clause counting

(deftest cyclomatic-complexity-coalton-match-clauses
  (testing "match with 2 non-wildcard clauses: complexity = 1 + 2 = 3"
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (foo x)
                            (match x
                              ((Some y) y)
                              ((None) 0))))"
                       :max 2)))
      (ok (= (length violations) 1))
      (ok (search "complexity of 3"
                  (violation:violation-message (first violations))))))

  (testing "match wildcard _ excluded: (Some y) counts, _ does not"
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (foo x)
                            (match x
                              ((Some y) y)
                              (_ 0))))"
                       :max 1)))
      ;; 1 non-wildcard clause only, complexity = 2
      (ok (= (length violations) 1))
      (ok (search "complexity of 2"
                  (violation:violation-message (first violations))))))

  (testing "match with only wildcard _ has no branches: complexity = 1"
    (ok (null (check-code
               "(coalton-toplevel
                  (define (foo x)
                    (match x
                      (_ 0))))"
               :max 1))))

  (testing "match with 5 non-wildcard clauses: complexity = 1 + 5 = 6"
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (foo x)
                            (match x
                              ((A) 1)
                              ((B) 2)
                              ((C) 3)
                              ((D) 4)
                              ((E) 5))))"
                       :max 5)))
      (ok (= (length violations) 1))
      (ok (search "complexity of 6"
                  (violation:violation-message (first violations))))))

  (testing "match within threshold produces no violation"
    (ok (null (check-code
               "(coalton-toplevel
                  (define (foo x)
                    (match x
                      ((A) 1)
                      ((B) 2))))"
               :max 3)))))

;;; if in Coalton define bodies

(deftest cyclomatic-complexity-coalton-if
  (testing "Coalton define with if: complexity = 1 + 1 = 2"
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (foo x)
                            (if (> x 0) x 0)))"
                       :max 1)))
      (ok (= (length violations) 1))
      (ok (search "complexity of 2"
                  (violation:violation-message (first violations))))))

  (testing "Coalton define with if and match: complexity = 1 + 1 (if) + 2 (match) = 4"
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (foo x y)
                            (if (> x 0)
                                (match y
                                  ((A) 1)
                                  ((B) 2)
                                  (_ 0))
                                0)))"
                       :max 3)))
      (ok (= (length violations) 1))
      (ok (search "complexity of 4"
                  (violation:violation-message (first violations)))))))

;;; Multiple defines

(deftest cyclomatic-complexity-coalton-multiple-defines
  (testing "Simple define + complex define: only complex one flagged"
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (simple x) x)
                          (define (complex x)
                            (match x
                              ((A) 1)
                              ((B) 2)
                              ((C) 3)
                              ((D) 4)
                              ((E) 5))))"
                       :max 5)))
      (ok (= (length violations) 1))
      (ok (search "COMPLEX"
                  (violation:violation-message (first violations))))))

  (testing "Both complex defines flagged when both exceed threshold"
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (foo x)
                            (match x ((A) 1) ((B) 2) ((C) 3) ((D) 4) ((E) 5)))
                          (define (bar x)
                            (match x ((A) 1) ((B) 2) ((C) 3) ((D) 4) ((E) 5))))"
                       :max 5)))
      (ok (= (length violations) 2)))))

;;; Nested defines

(deftest cyclomatic-complexity-coalton-nested-defines
  (testing "Outer simple define is not flagged; inner complex define is flagged independently"
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (outer x)
                            (define (inner y)
                              (match y
                                ((A) 1)
                                ((B) 2)
                                ((C) 3)
                                ((D) 4)
                                ((E) 5)))
                            (inner x)))"
                       :max 5)))
      ;; outer complexity = 1 (no branches; inner define is not counted)
      ;; inner complexity = 6 (5 match clauses + base)
      (ok (= (length violations) 1))
      (ok (search "INNER"
                  (violation:violation-message (first violations)))))))

;;; Nested match forms

(deftest cyclomatic-complexity-coalton-nested-match
  (testing "Nested match in clause body: both matches counted"
    ;; Outer match: 2 clauses. Inner match (in first clause body): 2 clauses.
    ;; complexity = 1 (base) + 2 (outer) + 2 (inner) = 5
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (foo x)
                            (match x
                              ((Some y)
                               (match y
                                 ((Left z) z)
                                 ((Right w) w)))
                              ((None) 0))))"
                       :max 4)))
      (ok (= (length violations) 1))
      (ok (search "complexity of 5"
                  (violation:violation-message (first violations)))))))

;;; Violation metadata

(deftest cyclomatic-complexity-coalton-metadata
  (testing "Violation rule name is :cyclomatic-complexity"
    (let ((violations (check-code
                        "(coalton-toplevel
                           (define (foo x)
                             (match x ((A) 1) ((B) 2) ((C) 3))))"
                        :max 1)))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :cyclomatic-complexity))))

  (testing "Violation message includes function name"
    (let ((violations (check-code
                        "(coalton-toplevel
                           (define (my-func x)
                             (match x ((A) 1) ((B) 2))))"
                        :max 1)))
      (ok (= (length violations) 1))
      (ok (search "MY-FUNC"
                  (violation:violation-message (first violations))))))

  (testing "Violation severity is :info"
    (let ((violations (check-code
                        "(coalton-toplevel
                           (define (foo x)
                             (match x ((A) 1) ((B) 2))))"
                        :max 1)))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-severity (first violations)) :info)))))

;;; Rule class metadata

(deftest cyclomatic-complexity-rule-coalton-class-metadata
  (testing "coalton-aware-p returns T for cyclomatic-complexity-rule"
    (ok (base:coalton-aware-p (make-instance 'rules:cyclomatic-complexity-rule))))

  (testing "Rule is NOT a coalton-rule subclass (it is coalton-aware, not coalton-only)"
    (ok (not (subtypep 'rules:cyclomatic-complexity-rule 'base:coalton-rule))))

  (testing "Rule name is still :cyclomatic-complexity"
    (let ((rule (make-instance 'rules:cyclomatic-complexity-rule)))
      (ok (eq (base:rule-name rule) :cyclomatic-complexity)))))

;;; Coalton when, unless, cond as decision points

(deftest cyclomatic-complexity-coalton-when
  (testing "Coalton define with when: complexity = 1 + 1 = 2"
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (foo x)
                            (when (> x 0) x)))"
                       :max 1)))
      (ok (= (length violations) 1))
      (ok (search "complexity of 2"
                  (violation:violation-message (first violations)))))))

(deftest cyclomatic-complexity-coalton-unless
  (testing "Coalton define with unless: complexity = 1 + 1 = 2"
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (foo x)
                            (unless (> x 0) x)))"
                       :max 1)))
      (ok (= (length violations) 1))
      (ok (search "complexity of 2"
                  (violation:violation-message (first violations)))))))

(deftest cyclomatic-complexity-coalton-cond
  (testing "Coalton define with cond (3 non-default clauses): complexity = 1 + 3 = 4"
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (foo x)
                            (cond
                              ((< x 0) -1)
                              ((== x 0) 0)
                              ((> x 0) 1)
                              (t 42))))"
                       :max 3)))
      (ok (= (length violations) 1))
      (ok (search "complexity of 4"
                  (violation:violation-message (first violations)))))))

;;; match with mixed named and wildcard patterns

(deftest cyclomatic-complexity-coalton-match-mixed-patterns
  (testing "match with 3 named + 1 wildcard: only named count, complexity = 1 + 3 = 4"
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (foo x)
                            (match x
                              ((A) 1)
                              ((B) 2)
                              ((C) 3)
                              (_ 0))))"
                       :max 3)))
      (ok (= (length violations) 1))
      (ok (search "complexity of 4"
                  (violation:violation-message (first violations))))))

  (testing "match with 2 named + 2 wildcard (only last _ is default): counts all non-_ patterns"
    ;; Two non-wildcard patterns: (A) and (B), complexity = 1 + 2 = 3
    ;; Note: only the trailing _ is the default clause
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (foo x)
                            (match x
                              ((A) 1)
                              ((B) 2)
                              (_ 0))))"
                       :max 2)))
      (ok (= (length violations) 1))
      (ok (search "complexity of 3"
                  (violation:violation-message (first violations)))))))

;;; match with multiple _ clauses only

(deftest cyclomatic-complexity-coalton-match-all-wildcards
  (testing "match with only _ clauses adds 0 decision points: complexity = 1"
    (ok (null (check-code
               "(coalton-toplevel
                  (define (foo x)
                    (match x
                      (_ 0)
                      (_ 1)
                      (_ 2))))"
               :max 1)))))

;;; Boundary: complexity exactly at max threshold

(deftest cyclomatic-complexity-coalton-boundary
  (testing "Coalton define at exactly the threshold does NOT trigger (> not >=)"
    ;; match with 2 named clauses: complexity = 1 + 2 = 3, max = 3 => no violation
    (ok (null (check-code
               "(coalton-toplevel
                  (define (foo x)
                    (match x
                      ((A) 1)
                      ((B) 2))))"
               :max 3))))

  (testing "Coalton define one above threshold DOES trigger"
    ;; Same code but max = 2 => violation
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (foo x)
                            (match x
                              ((A) 1)
                              ((B) 2))))"
                       :max 2)))
      (ok (= (length violations) 1)))))

;;; define-type and define-instance are NOT analyzed

(deftest cyclomatic-complexity-coalton-non-define-forms
  (testing "define-type is not analyzed for complexity"
    (ok (null (check-code
               "(coalton-toplevel
                  (define-type (Result a b)
                    (Ok a)
                    (Err b)))"
               :max 1))))

  (testing "define-instance is not analyzed for complexity"
    (ok (null (check-code
               "(coalton-toplevel
                  (define-instance (Eq MyType)
                    (define (== a b)
                      (match a
                        ((A) (match b ((A) True) (_ False)))
                        ((B) (match b ((B) True) (_ False)))
                        ((C) (match b ((C) True) (_ False)))))))"
               :max 1)))))

;;; :modified variant with Coalton match

(deftest cyclomatic-complexity-coalton-modified-variant
  (testing ":modified variant counts match as +1 total, not per-clause"
    ;; match with 5 named clauses: :standard => complexity = 1 + 5 = 6
    ;; :modified => complexity = 1 + 1 = 2 (whole match = +1)
    (let ((code "(coalton-toplevel
                   (define (foo x)
                     (match x
                       ((A) 1)
                       ((B) 2)
                       ((C) 3)
                       ((D) 4)
                       ((E) 5))))"))
      ;; :standard triggers at max 5
      (ok (= (length (check-code code :max 5 :variant :standard)) 1))
      ;; :modified does NOT trigger at max 5 (complexity is only 2)
      (ok (null (check-code code :max 5 :variant :modified)))
      ;; :modified triggers at max 1
      (ok (= (length (check-code code :max 1 :variant :modified)) 1))
      (ok (search "complexity of 2"
                  (violation:violation-message
                   (first (check-code code :max 1 :variant :modified)))))))

  (testing ":modified variant counts match as +1 even with if inside"
    ;; if (+1) + match (+1 modified) = complexity 3
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (foo x y)
                            (if (> x 0)
                                (match y
                                  ((A) 1)
                                  ((B) 2)
                                  ((C) 3))
                                0)))"
                       :max 2
                       :variant :modified)))
      (ok (= (length violations) 1))
      (ok (search "complexity of 3"
                  (violation:violation-message (first violations)))))))

;;; Violation metadata: line and column

(deftest cyclomatic-complexity-coalton-line-column
  (testing "Violation reports the line of the define form, not coalton-toplevel"
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (foo x)
                            (match x ((A) 1) ((B) 2))))"
                       :max 1)))
      (ok (= (length violations) 1))
      ;; The define is on line 2 (coalton-toplevel is line 1)
      (ok (< 1 (violation:violation-line (first violations)))
          "Violation line should be after the coalton-toplevel line")))

  (testing "Violation column is non-negative"
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (foo x)
                            (match x ((A) 1) ((B) 2))))"
                       :max 1)))
      (ok (<= 0 (violation:violation-column (first violations)))))))

;;; Stub guard — ensures a no-op implementation would NOT pass

(deftest cyclomatic-complexity-coalton-stub-guard
  (testing "Rule detects high-complexity Coalton define — stub returning nil would fail this"
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (foo x)
                            (match x
                              ((A) 1)
                              ((B) 2)
                              ((C) 3))))"
                       :max 1)))
      (ok (not (null violations))
          "Rule must detect match-driven complexity in Coalton defines")))

  (testing "Stub returning always-0 complexity would fail: complexity must be > 1 for branching code"
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (foo x)
                            (if (> x 0) x 0)))"
                       :max 1)))
      (ok (not (null violations))
          "if inside Coalton define must produce complexity > 1")
      (ok (search "complexity of 2"
                  (violation:violation-message (first violations)))))))
