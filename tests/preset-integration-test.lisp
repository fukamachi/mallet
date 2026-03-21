(defpackage #:mallet/tests/preset-integration
  (:use #:cl #:rove)
  (:local-nicknames
   (#:config #:mallet/config)
   (#:rules #:mallet/rules)
   (#:errors #:mallet/errors)
   (#:engine #:mallet/engine)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/preset-integration)

(defun fixture-config-path (filename)
  (merge-pathnames (concatenate 'string "tests/fixtures/configs/" filename)
                   (asdf:system-source-directory :mallet)))

(defun rule-names (cfg)
  (mapcar #'rules:rule-name (config:config-rules cfg)))

(defmacro with-temp-code-file ((content pathname) &body body)
  `(uiop:with-temporary-file (:stream out :pathname ,pathname
                              :direction :output :type "lisp")
     (write-string ,content out)
     (terpri out)
     (force-output out)
     ,@body))

;;; Tests using fixture config files from tests/fixtures/configs/

(deftest load-config-from-fixture-file
  (testing "user-preset.mallet.lisp: config extends :strict which extends built-in :default"
    (let* ((path (fixture-config-path "user-preset.mallet.lisp"))
           (cfg (config:load-config path))
           (names (rule-names cfg)))
      (ok (member :trailing-whitespace names)
          "Should inherit :trailing-whitespace from :default via :strict")
      (ok (member :unused-variables names)
          "Should inherit :unused-variables from :default via :strict")
      (ok (member :line-length names)
          "Should have :line-length from :strict")
      (let ((ll-rule (find :line-length (config:config-rules cfg)
                           :key #'rules:rule-name)))
        (ok (= 80 (rules:line-length-rule-max-length ll-rule))
            ":line-length max should be 80 as defined in :strict"))))

  (testing "user-preset.mallet.lisp: root-dir is set to the fixture directory"
    (let* ((path (fixture-config-path "user-preset.mallet.lisp"))
           (cfg (config:load-config path)))
      (ok (not (null (config:config-root-dir cfg)))
          "root-dir should be set")))

  (testing "user-preset.mallet.lisp: preset-override :relaxed applies relaxed rules only"
    (let* ((path (fixture-config-path "user-preset.mallet.lisp"))
           (cfg (config:load-config path :preset-override :relaxed))
           (names (rule-names cfg)))
      (ok (member :trailing-whitespace names)
          ":trailing-whitespace should be enabled by :relaxed")
      (ok (not (member :line-length names))
          ":line-length should NOT be present — :relaxed doesn't include it")
      (ok (not (member :unused-variables names))
          ":unused-variables should NOT be present — :relaxed extends :none")))

  (testing "user-preset.mallet.lisp: preset-override :strict applies strict rules"
    (let* ((path (fixture-config-path "user-preset.mallet.lisp"))
           (cfg (config:load-config path :preset-override :strict))
           (names (rule-names cfg)))
      (ok (member :trailing-whitespace names)
          ":trailing-whitespace should be present from :default via :strict")
      (ok (member :line-length names)
          ":line-length should be present from :strict")))

  (testing "user-preset.mallet.lisp: preset-override with unknown preset signals unknown-preset"
    (let ((path (fixture-config-path "user-preset.mallet.lisp")))
      (ok (signals (config:load-config path :preset-override :nonexistent-preset)
                   'errors:unknown-preset)
          "Unknown preset should signal unknown-preset error")))

  (testing "user-preset.mallet.lisp: unknown-preset error names the missing preset"
    (let ((path (fixture-config-path "user-preset.mallet.lisp")))
      (handler-case
          (config:load-config path :preset-override :my-missing-preset)
        (errors:unknown-preset (c)
          (ok (eq :my-missing-preset (errors:unknown-preset-name c))
              "Error should record the missing preset name"))))))

;;; Shadowed built-in preset tests

(deftest load-config-shadow-default
  (testing "shadow-default.mallet.lisp: user-defined :default is used (not built-in)"
    (let* ((path (fixture-config-path "shadow-default.mallet.lisp"))
           (cfg (config:load-config path))
           (names (rule-names cfg)))
      (ok (member :trailing-whitespace names)
          ":trailing-whitespace should be enabled by user-defined :default")
      (ok (member :line-length names)
          ":line-length should be enabled by user-defined :default")
      ;; User-defined :default extends :none, so built-in :default rules are excluded
      (ok (not (member :unused-variables names))
          ":unused-variables should NOT be present — user-defined :default extends :none")))

  (testing "shadow-default.mallet.lisp: shadowing emits note to *error-output*"
    (let* ((path (fixture-config-path "shadow-default.mallet.lisp"))
           (note-output (make-string-output-stream)))
      (let ((*error-output* note-output))
        (config:load-config path))
      (let ((note (get-output-stream-string note-output)))
        (ok (search "default" note)
            "Shadowing note should mention default")
        (ok (search "shadowing" note)
            "Shadowing note should mention 'shadowing'")))))

;;; Unknown-preset error detail tests

(deftest unknown-preset-error-details
  (testing "unknown-preset error includes available preset names"
    (let* ((defns (list
                   (config:parse-preset-definition
                    '(:mallet-preset :strict (:extends :default)))
                   (config:parse-preset-definition
                    '(:mallet-preset :relaxed (:extends :none)))))
           (registry (config:build-preset-registry defns)))
      (handler-case
          (config:resolve-preset :unknown registry)
        (errors:unknown-preset (c)
          (ok (eq :unknown (errors:unknown-preset-name c))
              "Error should name the missing preset")
          (let ((available (errors:unknown-preset-available-names c)))
            (ok (member :strict available)
                "Available names should include :strict")
            (ok (member :relaxed available)
                "Available names should include :relaxed"))))))

  (testing "unknown-preset error message is human-readable"
    (let* ((defns (list (config:parse-preset-definition
                         '(:mallet-preset :my-preset (:extends :default)))))
           (registry (config:build-preset-registry defns)))
      (handler-case
          (config:resolve-preset :no-such-preset registry)
        (errors:unknown-preset (c)
          (let ((msg (format nil "~A" c)))
            (ok (search "no-such-preset" (string-downcase msg))
                "Error message should contain the missing preset name")))))))

;;; Full pipeline: config load → preset resolution → engine linting

(deftest preset-engine-integration
  (flet ((has-violation-p (rule violations)
           (some (lambda (v) (eq rule (violation:violation-rule v))) violations)))

    (testing ":strict preset (via user-preset.mallet.lisp) detects long lines"
      (with-temp-code-file ((make-string 85 :initial-element #\;) code-file)
        (let* ((cfg (config:load-config (fixture-config-path "user-preset.mallet.lisp")))
               (violations (engine:lint-file code-file :config cfg)))
          (ok (has-violation-p :line-length violations)
              "85-char line should trigger :line-length under :strict preset"))))

    (testing ":relaxed preset-override (via user-preset.mallet.lisp) ignores long lines"
      (with-temp-code-file ((make-string 85 :initial-element #\;) code-file)
        (let* ((cfg (config:load-config (fixture-config-path "user-preset.mallet.lisp")
                                        :preset-override :relaxed))
               (violations (engine:lint-file code-file :config cfg)))
          (ok (not (has-violation-p :line-length violations))
              "Long line should NOT be flagged — :relaxed preset has no :line-length rule"))))

    (testing "user-defined :default (shadow-default.mallet.lisp) detects trailing whitespace"
      (with-temp-code-file ("(defun foo () nil)   " code-file)
        (let* ((cfg (config:load-config (fixture-config-path "shadow-default.mallet.lisp")))
               (violations (engine:lint-file code-file :config cfg)))
          (ok (has-violation-p :trailing-whitespace violations)
              "Trailing whitespace should be detected under user-defined :default"))))

    (testing "user-defined :default (shadow-default.mallet.lisp) detects long lines"
      (with-temp-code-file ((make-string 85 :initial-element #\;) code-file)
        (let* ((cfg (config:load-config (fixture-config-path "shadow-default.mallet.lisp")))
               (violations (engine:lint-file code-file :config cfg)))
          (ok (has-violation-p :line-length violations)
              "Long line should be detected — user-defined :default enables :line-length :max 80"))))))
