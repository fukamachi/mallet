(defpackage #:mallet/tests/engine-integration
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:engine #:mallet/engine)
   (#:config #:mallet/config)
   (#:rules #:mallet/rules)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/engine-integration)

(defun fixture-path (filename)
  (merge-pathnames (concatenate 'string "tests/fixtures/violations/" filename)
                   (asdf:system-source-directory :mallet)))

(defun make-if-without-else-config ()
  (config:make-config
   :rules (list (rules:make-rule :missing-else))))

(defun make-if-and-stale-config ()
  (config:make-config
   :rules (list (rules:make-rule :missing-else)
                (rules:make-rule :stale-suppression))))

(defun make-needless-let*-config ()
  (config:make-config
   :rules (list (rules:make-rule :needless-let*))))

(defun make-needless-let*-and-stale-config ()
  (config:make-config
   :rules (list (rules:make-rule :needless-let*)
                (rules:make-rule :stale-suppression))))

;;; Inline :suppress tests

(deftest comment-suppress-if-without-else
  (testing "; mallet:suppress suppresses violation for the annotated form only"
    (let* ((file (fixture-path "comment-suppress.lisp"))
           (config (make-if-without-else-config))
           (violations (engine:lint-file file :config config)))

      ;; suppressed-foo should NOT produce a violation
      (let ((foo-violations (remove-if-not
                              (lambda (v) (eq :missing-else (violation:violation-rule v)))
                              (remove-if-not
                                (lambda (v) (< (violation:violation-line v) 15))
                                violations))))
        (ok (null foo-violations) "suppressed-foo produces no if-without-else violation"))

      ;; Exactly 1 if-without-else violation: the unsuppressed-bar function
      (let ((iwe-violations (remove-if-not
                              (lambda (v) (eq :missing-else (violation:violation-rule v)))
                              violations)))
        (ok (= 1 (length iwe-violations))
            "Exactly 1 if-without-else violation (unsuppressed-bar only)")
        (when (= 1 (length iwe-violations))
          (ok (>= (violation:violation-line (first iwe-violations)) 15)
              "Violation is in unsuppressed-bar (line >= 15)"))))))

(deftest comment-suppress-needless-let*
  (testing "; mallet:suppress :needless-let* suppresses that form only"
    (let* ((file (fixture-path "comment-suppress.lisp"))
           (config (make-needless-let*-config))
           (violations (engine:lint-file file :config config)))

      ;; Exactly 1 needless-let* violation: unsuppressed-let*
      (let ((let*-violations (remove-if-not
                               (lambda (v) (eq :needless-let* (violation:violation-rule v)))
                               violations)))
        (ok (= 1 (length let*-violations))
            "Exactly 1 needless-let* violation (unsuppressed-let* only)")))))

;;; Inline :disable / :enable tests

(deftest comment-disable-enable
  (testing "; mallet:disable suppresses until :enable"
    (let* ((file (fixture-path "comment-disable.lisp"))
           (config (make-if-without-else-config))
           (violations (engine:lint-file file :config config)))

      (let ((iwe-violations (remove-if-not
                              (lambda (v) (eq :missing-else (violation:violation-rule v)))
                              violations)))
        ;; Expect 2 violations: before-disable and after-enable; during-disable is suppressed
        (ok (= 2 (length iwe-violations))
            "Exactly 2 violations: before-disable and after-enable")))))

;;; Stale suppression tests

(deftest comment-suppress-stale-detection
  (testing "Stale :suppress generates a stale-suppression violation"
    (let* ((file (fixture-path "comment-stale.lisp"))
           (config (make-if-and-stale-config))
           (violations (engine:lint-file file :config config)))

      ;; No if-without-else violations (clean-function has else clause)
      (let ((iwe-violations (remove-if-not
                              (lambda (v) (eq :missing-else (violation:violation-rule v)))
                              violations)))
        (ok (null iwe-violations) "No if-without-else violations (else present)"))

      ;; One stale-suppression violation
      (let ((stale-violations (remove-if-not
                                (lambda (v) (eq :stale-suppression (violation:violation-rule v)))
                                violations)))
        (ok (= 1 (length stale-violations))
            "Exactly 1 stale-suppression violation")
        (when (= 1 (length stale-violations))
          (ok (eq :warning (violation:violation-severity (first stale-violations)))
              "Stale suppression is a warning"))))))

(deftest comment-suppress-no-stale-when-rule-disabled
  (testing "No stale-suppression when stale-suppression rule not in config"
    (let* ((file (fixture-path "comment-stale.lisp"))
           (config (make-if-without-else-config))  ; no :stale-suppression rule
           (violations (engine:lint-file file :config config)))

      ;; No stale violations when rule is disabled
      (let ((stale-violations (remove-if-not
                                (lambda (v) (eq :stale-suppression (violation:violation-rule v)))
                                violations)))
        (ok (null stale-violations)
            "No stale-suppression violations when rule not in config")))))

(deftest comment-suppress-used-is-not-stale
  (testing "Used :suppress does not generate stale-suppression for that rule"
    (let* ((file (fixture-path "comment-stale.lisp"))
           ;; Use config WITHOUT stale-suppression to check that a plain suppress works
           (config (make-if-without-else-config))
           (violations (engine:lint-file file :config config)))

      ;; clean-function has no if-without-else violation (has else clause)
      ;; The suppress is above it, making it stale - but stale rule is not enabled
      (ok (null violations)
          "No violations (clean file with no matching violations, stale rule not enabled")))

  (testing "Used :suppress on real violation does not generate stale-suppression"
    (let* ((file (fixture-path "comment-suppress.lisp"))
           ;; Config: only if-without-else + stale-suppression
           ;; suppressed-foo has violation that IS suppressed → suppress is used → not stale
           (config (make-if-and-stale-config))
           (violations (engine:lint-file file :config config))
           (stale-violations (remove-if-not
                               (lambda (v) (eq :stale-suppression (violation:violation-rule v)))
                               violations))
           (iwe-stale (remove-if-not
                        (lambda (v)
                          (and (eq :stale-suppression (violation:violation-rule v))
                               ;; On the line of the suppress comment for suppressed-foo (line 9)
                               (= 9 (violation:violation-line v))))
                        stale-violations)))
      ;; The suppress for suppressed-foo (line 9) should NOT be stale
      (ok (null iwe-stale)
          "No stale violation for the suppress that actually suppressed a violation"))))

;;; Declaim suppress-next stale detection tests

(deftest declaim-suppress-next-stale-when-no-violation
  (testing "Stale #+mallet suppress-next generates a stale-suppression violation"
    (let* ((file (fixture-path "declaim-stale.lisp"))
           (config (make-needless-let*-and-stale-config))
           (violations (engine:lint-file file :config config))
           (stale-violations (remove-if-not
                               (lambda (v) (eq :stale-suppression (violation:violation-rule v)))
                               violations))
           (needless-violations (remove-if-not
                                  (lambda (v) (eq :needless-let* (violation:violation-rule v)))
                                  violations)))

      ;; clean-function has a suppress-next but no needless-let* → stale
      (ok (= 1 (length stale-violations))
          "Exactly 1 stale-suppression violation for the unused suppress-next")

      ;; suppressed-function has a suppress-next that matches a real violation → not stale
      ;; no needless-let* violations should appear (they are suppressed)
      (ok (null needless-violations)
          "No needless-let* violations (suppressed-function's violation is suppressed)"))))

(deftest declaim-suppress-next-not-stale-when-used
  (testing "Used #+mallet suppress-next does not generate stale-suppression"
    (let* ((file (fixture-path "declaim-stale.lisp"))
           (config (make-needless-let*-and-stale-config))
           (violations (engine:lint-file file :config config))
           (stale-violations (remove-if-not
                               (lambda (v) (eq :stale-suppression (violation:violation-rule v)))
                               violations)))

      ;; Only 1 stale violation (for clean-function), not 2
      ;; The suppress-next for suppressed-function was used → not stale
      (ok (= 1 (length stale-violations))
          "Only 1 stale-suppression violation (suppressed-function's suppress-next is used)"))))
