(defpackage #:mallet/tests/rules/coalton-rule-base
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:base #:mallet/rules/base)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/coalton-rule-base)

;;; Minimal concrete coalton-rule subclass used only in these tests.
;;; It flags every Coalton toplevel form it receives with a fixed violation.

(defclass test-coalton-rule (base:coalton-rule)
  ()
  (:default-initargs :name :test-coalton-rule
                     :description "Test rule — only runs inside coalton-toplevel"
                     :severity :warning
                     :category :correctness))

;;; Minimal concrete rule subclass (plain, non-Coalton) for contrast.

(defclass test-plain-rule (base:rule)
  ()
  (:default-initargs :name :test-plain-rule
                     :description "Plain rule — skips coalton-toplevel"
                     :severity :warning
                     :category :correctness))

;;; Shared helper — both test rules flag every form they receive.

(defun make-triggered-violation (rule file)
  (make-instance 'violation:violation
                 :rule (base:rule-name rule)
                 :file file
                 :line 1
                 :column 0
                 :severity :warning
                 :message "triggered"))

(defmethod base:check-form ((rule test-coalton-rule) form file)
  (when (handler-case (parser:form-expr form) (error () nil))
    (list (make-triggered-violation rule file))))

(defmethod base:check-form ((rule test-plain-rule) form file)
  (when (handler-case (parser:form-expr form) (error () nil))
    (list (make-triggered-violation rule file))))

;;; Helpers

(defun run-rule (rule-class code)
  (let ((forms (parser:parse-forms code #p"test.lisp"))
        (rule (make-instance rule-class)))
    (mapcan (lambda (form)
              (base:check-form rule form #p"test.lisp"))
            forms)))

(defun run-coalton-rule (code) (run-rule 'test-coalton-rule code))
(defun run-plain-rule (code) (run-rule 'test-plain-rule code))

;;; Tests

(deftest coalton-rule-class-structure
  (testing "coalton-rule class exists in mallet/rules/base"
    (ok (find-class 'base:coalton-rule nil)))

  (testing "coalton-rule is a subclass of rule"
    (ok (subtypep 'base:coalton-rule 'base:rule)))

  (testing "coalton-rule is exported from mallet/rules/base"
    (ok (find-symbol "COALTON-RULE" "MALLET/RULES/BASE"))))

(deftest coalton-rule-runs-on-coalton-toplevel
  (testing "coalton-rule fires on (coalton-toplevel ...) forms"
    (let ((violations (run-coalton-rule "(coalton-toplevel (define x 1))")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :test-coalton-rule))))

  (testing "coalton-rule fires on (coalton:coalton-toplevel ...) qualified form"
    (let ((violations (run-coalton-rule "(coalton:coalton-toplevel (define x 1))")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :test-coalton-rule)))))

(deftest coalton-rule-skips-non-coalton-forms
  (testing "coalton-rule returns no violations for a plain defun"
    (ok (null (run-coalton-rule "(defun foo (x) (+ x 1))"))))

  (testing "coalton-rule returns no violations for a let form"
    (ok (null (run-coalton-rule "(let ((x 1)) x)"))))

  (testing "coalton-rule returns no violations for defpackage"
    (ok (null (run-coalton-rule "(defpackage #:my-pkg (:use #:cl))"))))

  (testing "coalton-rule returns no violations for top-level progn"
    (ok (null (run-coalton-rule "(progn (defun foo () nil))")))))

(deftest plain-rule-still-skips-coalton-toplevel
  (testing "plain rule returns no violations for (coalton-toplevel ...) forms"
    (ok (null (run-plain-rule "(coalton-toplevel (define x 1))"))))

  (testing "plain rule still fires on regular CL forms"
    (let ((violations (run-plain-rule "(defun foo (x) (+ x 1))")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :test-plain-rule)))))

(deftest coalton-rule-multiple-toplevel-forms
  (testing "coalton-rule fires once per coalton-toplevel form among mixed forms"
    (let ((violations (run-coalton-rule
                       "(defun foo () nil)
                        (coalton-toplevel (define x 1))
                        (defun bar () nil)
                        (coalton-toplevel (define y 2))")))
      (ok (= (length violations) 2))
      (ok (every (lambda (v) (eq (violation:violation-rule v) :test-coalton-rule))
                 violations)))))

;;; Edge cases and boundary conditions

(deftest coalton-rule-empty-input
  (testing "coalton-rule produces no violations for empty string"
    (ok (null (run-coalton-rule ""))))

  (testing "plain rule produces no violations for empty string"
    (ok (null (run-plain-rule ""))))

  (testing "coalton-rule produces no violations for whitespace-only input"
    (ok (null (run-coalton-rule "
   "))))

  (testing "coalton-rule produces no violations for comment-only input"
    (ok (null (run-coalton-rule ";; just a comment")))))

(deftest coalton-rule-empty-coalton-toplevel
  (testing "coalton-rule fires on (coalton-toplevel) with empty body"
    (let ((violations (run-coalton-rule "(coalton-toplevel)")))
      (ok (= (length violations) 1)))))

(deftest coalton-rule-class-is-distinct-from-rule
  (testing "coalton-rule is not the same class as rule"
    (ok (not (eq (find-class 'base:coalton-rule)
                 (find-class 'base:rule)))))

  (testing "an instance of coalton-rule is typep coalton-rule"
    (let ((inst (make-instance 'test-coalton-rule)))
      (ok (typep inst 'base:coalton-rule))))

  (testing "an instance of coalton-rule is also typep rule (inheritance)"
    (let ((inst (make-instance 'test-coalton-rule)))
      (ok (typep inst 'base:rule))))

  (testing "an instance of plain rule is NOT typep coalton-rule"
    (let ((inst (make-instance 'test-plain-rule)))
      (ok (not (typep inst 'base:coalton-rule))))))

(deftest coalton-rule-inherits-default-slots
  (testing "coalton-rule instance has enabled-p defaulting to T"
    (let ((rule (make-instance 'test-coalton-rule)))
      (ok (base:rule-enabled-p rule))))

  (testing "coalton-rule instance has type defaulting to :form"
    (let ((rule (make-instance 'test-coalton-rule)))
      (ok (eq (base:rule-type rule) :form))))

  (testing "coalton-rule instance has file-types defaulting to (:lisp)"
    (let ((rule (make-instance 'test-coalton-rule)))
      (ok (equal (base:rule-file-types rule) '(:lisp)))))

  (testing "coalton-rule instance retains custom initargs"
    (let ((rule (make-instance 'test-coalton-rule)))
      (ok (eq (base:rule-name rule) :test-coalton-rule))
      (ok (eq (base:rule-severity rule) :warning))
      (ok (eq (base:rule-category rule) :correctness))
      (ok (stringp (base:rule-description rule))))))

(deftest coalton-rule-violation-metadata
  (testing "violations from coalton-rule have correct metadata"
    (let* ((violations (run-coalton-rule "(coalton-toplevel (define x 1))"))
           (v (first violations)))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule v) :test-coalton-rule))
      (ok (eql (violation:violation-line v) 1))
      (ok (eql (violation:violation-column v) 0))
      (ok (eq (violation:violation-severity v) :warning))
      (ok (string= (violation:violation-message v) "triggered"))
      (ok (pathnamep (violation:violation-file v))))))

(deftest coalton-rule-case-insensitive-head
  (testing "coalton-rule fires on uppercase (COALTON-TOPLEVEL ...)"
    (let ((violations (run-coalton-rule "(COALTON-TOPLEVEL (define x 1))")))
      (ok (= (length violations) 1)))))

(deftest coalton-rule-symmetric-exclusivity
  ;; Both rule types on the same mixed input should partition forms cleanly.
  (testing "coalton-rule and plain-rule partition forms with no overlap"
    (let* ((code "(defun foo () nil) (coalton-toplevel (define x 1))")
           (coalton-violations (run-coalton-rule code))
           (plain-violations   (run-plain-rule code)))
      ;; coalton-rule only fires on coalton-toplevel
      (ok (= (length coalton-violations) 1))
      ;; plain rule only fires on defun
      (ok (= (length plain-violations) 1))
      ;; They fire on different forms
      (ok (eq (violation:violation-rule (first coalton-violations)) :test-coalton-rule))
      (ok (eq (violation:violation-rule (first plain-violations)) :test-plain-rule))))

  (testing "with only coalton-toplevel forms, plain rule gets nothing"
    (let ((plain-violations (run-plain-rule "(coalton-toplevel (define x 1)) (coalton:coalton-toplevel (define y 2))")))
      (ok (null plain-violations))))

  (testing "with only CL forms, coalton-rule gets nothing"
    (let ((coalton-violations (run-coalton-rule "(defun foo () nil) (defvar *x* 1)")))
      (ok (null coalton-violations)))))
