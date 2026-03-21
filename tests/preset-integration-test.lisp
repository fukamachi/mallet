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

;;; Tests using fixture config files from tests/fixtures/configs/

(deftest load-config-from-fixture-file
  (testing "user-preset.mallet.lisp: config extends :strict which extends built-in :default"
    (let* ((path (fixture-config-path "user-preset.mallet.lisp"))
           (cfg (config:load-config path))
           (rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
      ;; :strict extends :default, so all :default rules should be present
      (ok (member :trailing-whitespace rule-names)
          "Should inherit :trailing-whitespace from :default via :strict")
      (ok (member :unused-variables rule-names)
          "Should inherit :unused-variables from :default via :strict")
      ;; :strict also adds :line-length
      (ok (member :line-length rule-names)
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
           (rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
      ;; :relaxed extends :none and only enables :trailing-whitespace
      (ok (member :trailing-whitespace rule-names)
          ":trailing-whitespace should be enabled by :relaxed")
      (ok (not (member :line-length rule-names))
          ":line-length should NOT be present — :relaxed doesn't include it")
      (ok (not (member :unused-variables rule-names))
          ":unused-variables should NOT be present — :relaxed extends :none")))

  (testing "user-preset.mallet.lisp: preset-override :strict applies strict rules"
    (let* ((path (fixture-config-path "user-preset.mallet.lisp"))
           (cfg (config:load-config path :preset-override :strict))
           (rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
      ;; :strict extends :default, so should have all default rules plus :line-length
      (ok (member :trailing-whitespace rule-names)
          ":trailing-whitespace should be present from :default via :strict")
      (ok (member :line-length rule-names)
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
           (rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
      ;; User-defined :default only enables :trailing-whitespace and :line-length
      (ok (member :trailing-whitespace rule-names)
          ":trailing-whitespace should be enabled by user-defined :default")
      (ok (member :line-length rule-names)
          ":line-length should be enabled by user-defined :default")
      ;; Built-in :default has :unused-variables, but user-defined :default
      ;; extends :none so it should NOT be present
      (ok (not (member :unused-variables rule-names))
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
    ;; Build a small registry and trigger the error via resolve-preset
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
  (testing ":strict preset (via user-preset.mallet.lisp) detects long lines"
    (uiop:with-temporary-file (:stream out :pathname code-file
                               :direction :output :type "lisp")
      ;; Write a line longer than 80 chars (the :strict preset sets :line-length :max 80)
      (write-string (make-string 85 :initial-element #\;) out)
      (terpri out)
      (force-output out)
      (let* ((cfg (config:load-config (fixture-config-path "user-preset.mallet.lisp")))
             (violations (engine:lint-file code-file :config cfg)))
        (ok (some (lambda (v) (eq :line-length (violation:violation-rule v)))
                  violations)
            "85-char line should trigger :line-length under :strict preset"))))

  (testing ":relaxed preset-override (via user-preset.mallet.lisp) ignores long lines"
    (uiop:with-temporary-file (:stream out :pathname code-file
                               :direction :output :type "lisp")
      (write-string (make-string 85 :initial-element #\;) out)
      (terpri out)
      (force-output out)
      (let* ((cfg (config:load-config (fixture-config-path "user-preset.mallet.lisp")
                                      :preset-override :relaxed))
             (violations (engine:lint-file code-file :config cfg)))
        (ok (not (some (lambda (v) (eq :line-length (violation:violation-rule v)))
                       violations))
            "Long line should NOT be flagged — :relaxed preset has no :line-length rule"))))

  (testing "user-defined :default (shadow-default.mallet.lisp) detects trailing whitespace"
    (uiop:with-temporary-file (:stream out :pathname code-file
                               :direction :output :type "lisp")
      (write-string "(defun foo () nil)   " out) ; trailing spaces
      (terpri out)
      (force-output out)
      (let* ((cfg (config:load-config (fixture-config-path "shadow-default.mallet.lisp")))
             (violations (engine:lint-file code-file :config cfg)))
        (ok (some (lambda (v) (eq :trailing-whitespace (violation:violation-rule v)))
                  violations)
            "Trailing whitespace should be detected under user-defined :default"))))

  (testing "user-defined :default (shadow-default.mallet.lisp) detects long lines"
    (uiop:with-temporary-file (:stream out :pathname code-file
                               :direction :output :type "lisp")
      (write-string (make-string 85 :initial-element #\;) out)
      (terpri out)
      (force-output out)
      (let* ((cfg (config:load-config (fixture-config-path "shadow-default.mallet.lisp")))
             (violations (engine:lint-file code-file :config cfg)))
        (ok (some (lambda (v) (eq :line-length (violation:violation-rule v)))
                  violations)
            "Long line should be detected — user-defined :default enables :line-length :max 80")))))
