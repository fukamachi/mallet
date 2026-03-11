(defpackage #:mallet/tests/engine-comment-suppression
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:engine #:mallet/engine)
   (#:config #:mallet/config)
   (#:rules #:mallet/rules)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/engine-comment-suppression)

;;; Helper functions

(defun fixture-path (subdir filename)
  (merge-pathnames (concatenate 'string "tests/fixtures/" subdir "/" filename)
                   (asdf:system-source-directory :mallet)))

(defun violations-fixture (filename)
  (fixture-path "violations" filename))

(defun no-violations-fixture (filename)
  (fixture-path "no-violations" filename))

(defun make-needless-let*-config ()
  (config:make-config
   :rules (list (rules:make-rule :needless-let*))))

(defun make-needless-let*-and-stale-config ()
  (config:make-config
   :rules (list (rules:make-rule :needless-let*)
                (rules:make-rule :stale-suppression))))

(defun make-if-without-else-config ()
  (config:make-config
   :rules (list (rules:make-rule :if-without-else))))

(defun make-if-without-else-and-stale-config ()
  (config:make-config
   :rules (list (rules:make-rule :if-without-else)
                (rules:make-rule :stale-suppression))))

;;; Test 1: Stale suppression — comment with no matching violation

(deftest comment-suppress-stale-no-violation
  (testing "Stale :suppress generates stale-suppression violation"
    (let* ((file (violations-fixture "comment-suppress-stale.lisp"))
           (config (make-needless-let*-and-stale-config))
           (violations (engine:lint-file file :config config)))

      ;; No needless-let* violations (the form uses let, not let*)
      (let ((let*-violations (remove-if-not
                               (lambda (v) (eq :needless-let* (violation:violation-rule v)))
                               violations)))
        (ok (null let*-violations)
            "No needless-let* violations (form uses plain let)"))

      ;; Exactly one stale-suppression violation
      (let ((stale-violations (remove-if-not
                                (lambda (v) (eq :stale-suppression (violation:violation-rule v)))
                                violations)))
        (ok (= 1 (length stale-violations))
            "Exactly 1 stale-suppression violation for unused :needless-let* suppress")
        (when (= 1 (length stale-violations))
          (ok (eq :warning (violation:violation-severity (first stale-violations)))
              "Stale suppression severity is :warning"))))))

(deftest comment-suppress-stale-no-rule-no-violation
  (testing "No stale-suppression violation when rule not in config"
    (let* ((file (violations-fixture "comment-suppress-stale.lisp"))
           (config (make-needless-let*-config)) ; no :stale-suppression rule
           (violations (engine:lint-file file :config config)))

      (let ((stale-violations (remove-if-not
                                (lambda (v) (eq :stale-suppression (violation:violation-rule v)))
                                violations)))
        (ok (null stale-violations)
            "No stale-suppression violations when rule not in config")))))

;;; Test 2: Active suppression — comment suppresses real violation, no output

(deftest comment-suppress-active-no-output
  (testing "; mallet:suppress :needless-let* suppresses the annotated form"
    (let* ((file (no-violations-fixture "comment-suppress-active.lisp"))
           (config (make-needless-let*-config))
           (violations (engine:lint-file file :config config)))

      (ok (null violations)
          "No violations when needless-let* is suppressed by inline comment")))

  (testing "Suppressed violation is not reported as stale"
    (let* ((file (no-violations-fixture "comment-suppress-active.lisp"))
           (config (make-needless-let*-and-stale-config))
           (violations (engine:lint-file file :config config)))

      ;; The suppress was actually used — it should not be stale
      (let ((stale-violations (remove-if-not
                                (lambda (v) (eq :stale-suppression (violation:violation-rule v)))
                                violations)))
        (ok (null stale-violations)
            "No stale-suppression violation when suppress was used")))))

;;; Test 3: disable/enable region for form-level rule

(deftest comment-disable-enable-region
  (testing "; mallet:disable suppresses until :enable, leaving others intact"
    (let* ((file (violations-fixture "comment-disable-enable.lisp"))
           (config (make-if-without-else-config))
           (violations (engine:lint-file file :config config)))

      (let ((iwe-violations (remove-if-not
                              (lambda (v) (eq :if-without-else (violation:violation-rule v)))
                              violations)))
        ;; before-disable and after-enable should both be flagged
        ;; during-disable should be suppressed
        (ok (= 2 (length iwe-violations))
            "Exactly 2 if-without-else violations: before-disable and after-enable"))))

  (testing "Violations before disable region are reported normally"
    (let* ((file (violations-fixture "comment-disable-enable.lisp"))
           (config (make-if-without-else-config))
           (violations (engine:lint-file file :config config)))

      (let ((iwe-violations (sort
                              (remove-if-not
                               (lambda (v) (eq :if-without-else (violation:violation-rule v)))
                               violations)
                              #'< :key #'violation:violation-line)))
        (when (= 2 (length iwe-violations))
          ;; before-disable is on line ~11, after-enable is on line ~22
          (ok (< (violation:violation-line (first iwe-violations))
                 (violation:violation-line (second iwe-violations)))
              "First violation precedes second violation in source order"))))))
