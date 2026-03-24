(defpackage #:mallet/tests/config
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:config #:mallet/config)
   (#:rules #:mallet/rules)
   (#:errors #:mallet/errors)))
(in-package #:mallet/tests/config)

;;; Config data structure tests

(deftest config-creation
  (testing "Create empty config"
    (let ((cfg (config:make-config)))
      (ok (not (null cfg)))
      (ok (typep cfg 'config:config))))

  (testing "Create config with rules"
    (let ((cfg (config:make-config
                :rules (list (rules:make-rule :line-length :max 100)
                             (rules:make-rule :missing-else))
                :disabled-rules '(:missing-else))))
      (ok (not (null cfg)))
      (ok (= 2 (length (config:config-rules cfg))))
      (ok (member :missing-else (config:config-disabled-rules cfg))))))

;;; Config parsing tests

(deftest parse-config
  (testing "Parse simple config with :enable"
    (let* ((sexp '(:mallet-config
                   (:enable :line-length :max 120)))
           (cfg (config:parse-config sexp)))
      (ok (= 1 (length (config:config-rules cfg))))
      (let ((rule (first (config:config-rules cfg))))
        (ok (eq :line-length (rules:rule-name rule)))
        (ok (= 120 (rules:line-length-rule-max-length rule))))))

  (testing "Parse config with :disable"
    (let* ((sexp '(:mallet-config
                   (:disable :missing-else)))
           (cfg (config:parse-config sexp)))
      (ok (member :missing-else (config:config-disabled-rules cfg)))))

  (testing "Parse config with severity override"
    (let* ((sexp '(:mallet-config
                   (:enable :unused-variables :severity :error)))
           (cfg (config:parse-config sexp)))
      (let ((rule (first (config:config-rules cfg))))
        (ok (eq :error (rules:rule-severity rule)))))))

;;; Config file loading tests

(defmacro with-temporary-config ((content pathname) &body body)
  `(uiop:with-temporary-file (:stream out
                              :pathname ,pathname
                              :direction :output
                              :prefix "mallet-config"
                              :type "lisp")
     (write-string ,content out)
     (force-output out)
     ,@body))

(deftest load-config-file
  (testing "Load config from file"
    (let ((config-content "(:mallet-config
                              (:enable :line-length :max 100))"))
      (with-temporary-config (config-content tmpfile)
        (let ((cfg (config:load-config tmpfile)))
          (ok (= 1 (length (config:config-rules cfg))))
          (let ((rule (first (config:config-rules cfg))))
            (ok (eq :line-length (rules:rule-name rule)))
            (ok (= 100 (rules:line-length-rule-max-length rule)))))))))

;;; Built-in config tests

(deftest built-in-configs
  (testing "Load default config"
    (let ((cfg (config:get-built-in-config :default)))
      (ok (not (null cfg)))
      ;; Check that some rules are in the enabled list
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :trailing-whitespace rule-names))
        (ok (member :no-tabs rule-names))
        (ok (member :unused-variables rule-names))
        (ok (member :missing-else rule-names))
        ;; missing-docstring is opt-in only; must not be in default
        (ok (not (member :missing-docstring rule-names)))
        ;; ASDF best-practices rules in the default preset
        (ok (not (member :asdf-redundant-package-prefix rule-names)))
        (ok (member :asdf-operate-in-perform rule-names))
        (ok (member :asdf-secondary-system-name rule-names))
        (ok (member :asdf-if-feature-keyword rule-names))
        ;; asdf-reader-conditional is disabled in default
        (ok (not (member :asdf-reader-conditional rule-names)))
        ;; one-package-per-file is opt-in only; must not be in default
        (ok (not (member :one-package-per-file rule-names)))
        ;; These opinionated rules were removed from :default (too noisy for legacy codebases)
        (ok (not (member :no-package-use rule-names)))
        (ok (not (member :double-colon-access rule-names)))
        (ok (not (member :closing-paren-on-own-line rule-names)))
        (ok (not (member :redundant-progn rule-names))))
      ;; Check that some rules are disabled
      (let ((disabled (config:config-disabled-rules cfg)))
        (ok (member :line-length disabled))
        (ok (member :constant-naming disabled))
        (ok (member :special-variable-naming disabled))
        ;; Opinionated rules removed from :default should be in disabled list
        (ok (member :no-package-use disabled))
        (ok (member :double-colon-access disabled))
        (ok (member :closing-paren-on-own-line disabled))
        (ok (member :redundant-progn disabled)))))

  (testing "Load all config"
    (let ((cfg (config:get-built-in-config :all)))
      (ok (not (null cfg)))
      ;; All rules should be enabled (none in disabled list)
      (ok (null (config:config-disabled-rules cfg)))
      ;; Check that various rules are present
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :line-length rule-names))
        (ok (member :trailing-whitespace rule-names))
        (ok (member :missing-else rule-names))
        ;; Docstring rules are in the all preset
        (ok (member :missing-docstring rule-names))
        (ok (member :missing-package-docstring rule-names))
        (ok (member :missing-variable-docstring rule-names))
        (ok (member :missing-struct-docstring rule-names))
        ;; one-package-per-file is in the all preset
        (ok (member :one-package-per-file rule-names))
        ;; All 5 ASDF best-practices rules are in the all preset
        (ok (member :asdf-redundant-package-prefix rule-names))
        (ok (member :asdf-operate-in-perform rule-names))
        (ok (member :asdf-secondary-system-name rule-names))
        (ok (member :asdf-if-feature-keyword rule-names))
        (ok (member :asdf-reader-conditional rule-names)))))

  (testing "Load strict config"
    (let ((cfg (config:get-built-in-config :strict)))
      (ok (not (null cfg)))
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        ;; All default rules are present
        (ok (member :trailing-whitespace rule-names))
        (ok (member :no-tabs rule-names))
        (ok (member :unused-variables rule-names))
        (ok (member :no-ignore-errors rule-names))
        ;; Opinionated rules added beyond :default
        (ok (member :no-package-use rule-names))
        (ok (member :double-colon-access rule-names))
        (ok (member :closing-paren-on-own-line rule-names))
        (ok (member :redundant-progn rule-names))
        (ok (member :no-allow-other-keys rule-names))
        (ok (member :error-with-string-only rule-names))
        (ok (member :bare-float-literal rule-names))
        (ok (member :asdf-redundant-package-prefix rule-names))
        (ok (member :asdf-reader-conditional rule-names))
        (ok (member :coalton-missing-declare rule-names))
        (ok (member :runtime-intern rule-names))
        (ok (member :runtime-unintern rule-names))
        (ok (member :coalton-missing-to-boolean rule-names))
        (ok (member :unused-loop-variables rule-names))
        (ok (member :progn-in-conditional rule-names))
        (ok (member :defpackage-interned-symbol rule-names))
        (ok (member :missing-otherwise rule-names))
        ;; These opt-in rules must NOT be in :strict
        (ok (not (member :missing-docstring rule-names)))
        (ok (not (member :one-package-per-file rule-names)))
        (ok (not (member :line-length rule-names))))))

  (testing "Load none config"
    (let ((cfg (config:get-built-in-config :none)))
      (ok (not (null cfg)))
      ;; No rules should be enabled
      (ok (null (config:config-rules cfg)))
      ;; No rules should be disabled
      (ok (null (config:config-disabled-rules cfg))))))

;;; Config extends tests

(deftest config-extends
  (testing "Parse config with extends"
    (let* ((sexp '(:mallet-config
                   (:extends :all)
                   (:enable :line-length :max 100)))
           (cfg (config:parse-config sexp)))
      ;; Should inherit all rules from :all
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :line-length rule-names))
        (ok (member :trailing-whitespace rule-names)))
      ;; Check that line-length has the overridden max-length
      (let ((line-length-rule (find :line-length (config:config-rules cfg)
                                    :key #'rules:rule-name)))
        (ok (= 100 (rules:line-length-rule-max-length line-length-rule))))))

  (testing "Parse config with extends :none"
    (let* ((sexp '(:mallet-config
                   (:extends :none)
                   (:enable :unused-variables)
                   (:enable :trailing-whitespace)))
           (cfg (config:parse-config sexp)))
      ;; Should only have the explicitly enabled rules
      (ok (= 2 (length (config:config-rules cfg))))
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :unused-variables rule-names))
        (ok (member :trailing-whitespace rule-names))
        (ok (not (member :line-length rule-names)))))))

(deftest parse-new-syntax-enable
  (testing "Parse config with :enable syntax"
    (let* ((sexp '(:mallet-config
                   (:enable :line-length :max 120)))
           (cfg (config:parse-config sexp)))
      (ok (= 1 (length (config:config-rules cfg))))
      (let ((rule (first (config:config-rules cfg))))
        (ok (eq :line-length (rules:rule-name rule)))
        (ok (= 120 (rules:line-length-rule-max-length rule))))))

  (testing "Parse config with :enable without options"
    (let* ((sexp '(:mallet-config
                   (:enable :unused-variables)))
           (cfg (config:parse-config sexp)))
      (ok (= 1 (length (config:config-rules cfg))))
      (let ((rule (first (config:config-rules cfg))))
        (ok (eq :unused-variables (rules:rule-name rule))))))

  (testing "Parse config with multiple :enable forms"
    (let* ((sexp '(:mallet-config
                   (:enable :line-length :max 100)
                   (:enable :unused-variables :severity :error)
                   (:enable :special-variable-naming)))
           (cfg (config:parse-config sexp)))
      (ok (= 3 (length (config:config-rules cfg))))
      (let ((rule-map (make-hash-table)))
        (dolist (rule (config:config-rules cfg))
          (setf (gethash (rules:rule-name rule) rule-map) rule))
        ;; Check line-length
        (let ((rule (gethash :line-length rule-map)))
          (ok (not (null rule)))
          (ok (= 100 (rules:line-length-rule-max-length rule))))
        ;; Check unused-variables
        (let ((rule (gethash :unused-variables rule-map)))
          (ok (not (null rule)))
          (ok (eq :error (rules:rule-severity rule))))
        ;; Check special-variable-naming
        (ok (not (null (gethash :special-variable-naming rule-map))))))))

(deftest parse-new-syntax-disable
  (testing "Parse config with :disable syntax"
    (let* ((sexp '(:mallet-config
                   (:extends :default)
                   (:disable :constant-naming)))
           (cfg (config:parse-config sexp)))
      (ok (member :constant-naming (config:config-disabled-rules cfg)))))

  (testing "Parse config with multiple :disable forms"
    (let* ((sexp '(:mallet-config
                   (:extends :default)
                   (:disable :line-length)
                   (:disable :consecutive-blank-lines)))
           (cfg (config:parse-config sexp)))
      (ok (member :line-length (config:config-disabled-rules cfg)))
      (ok (member :consecutive-blank-lines (config:config-disabled-rules cfg))))))

;;; Path override tests

(deftest parse-for-paths
  (testing "Parse config with :for-paths"
    (let* ((sexp '(:mallet-config
                   (:extends :default)
                   (:for-paths ("tests/**/*.lisp")
                    (:enable :line-length :max 120)
                    (:disable :unused-variables))))
           (cfg (config:parse-config sexp)))
      ;; Base config should have default rules
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :unused-variables rule-names)))
      ;; Check that path-rules are stored
      (ok (= 1 (length (config:config-path-rules cfg))))))

  (testing "Parse config with directory name patterns"
    (let* ((sexp '(:mallet-config
                   (:for-paths ("tests/**/*.lisp" "scripts/**/*.lisp")
                    (:disable :line-length))))
           (cfg (config:parse-config sexp)))
      ;; Check that path-rules are stored
      (ok (= 1 (length (config:config-path-rules cfg))))
      ;; Patterns should be stored as-is
      (let* ((path-rule (first (config:config-path-rules cfg)))
             (patterns (config:path-override-patterns path-rule)))
        (ok (= 2 (length patterns)))))))

;;; get-rules-for-file tests

(deftest get-rules-for-file
  (testing "Get rules for file without path-specific overrides"
    (let ((cfg (config:get-built-in-config :default)))
      (let* ((rules (config:get-rules-for-file cfg #P"/src/main.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        ;; Should get base rules, minus disabled ones
        (ok (member :trailing-whitespace rule-names))
        (ok (not (member :line-length rule-names))))))  ; line-length is disabled

  (testing "Get rules for file with path-specific overrides"
    (let* ((sexp '(:mallet-config
                   (:enable :line-length :max 80)
                   (:enable :unused-variables)
                   (:for-paths ("tests/**/*.lisp")
                    (:enable :line-length :max 120)
                    (:disable :unused-variables))))
           (cfg (config:parse-config sexp)))
      ;; For a normal file, should get base rules
      (let* ((rules (config:get-rules-for-file cfg #P"/src/main.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        (ok (member :line-length rule-names))
        (ok (member :unused-variables rule-names)))
      ;; For a test file, should get overridden rules
      (let* ((rules (config:get-rules-for-file cfg #P"/tests/foo-test.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        (ok (member :line-length rule-names))
        (ok (not (member :unused-variables rule-names)))  ; disabled for tests
        ;; Check that line-length has the overridden max-length
        (let ((line-length-rule (find :line-length rules :key #'rules:rule-name)))
          (ok (= 120 (rules:line-length-rule-max-length line-length-rule)))))))

  (testing "Get rules for specific file path"
    (let* ((sexp '(:mallet-config
                   (:enable :unused-variables)
                   (:enable :unused-local-nicknames)
                   (:for-paths ("/src/parser.lisp")
                    (:disable :unused-local-nicknames))))
           (cfg (config:parse-config sexp)))
      ;; For src/parser.lisp, unused-local-nicknames should be disabled
      (let* ((rules (config:get-rules-for-file cfg #P"/src/parser.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        (ok (member :unused-variables rule-names))
        (ok (not (member :unused-local-nicknames rule-names))))  ; disabled for this file
      ;; For other files, both rules should be enabled
      (let* ((rules (config:get-rules-for-file cfg #P"/src/main.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        (ok (member :unused-variables rule-names))
        (ok (member :unused-local-nicknames rule-names))))))

;;; Comprehensive :ignore pattern tests

(deftest ignore-patterns
  (testing "Ignore exact file path with leading /"
    (let* ((sexp '(:mallet-config
                   (:ignore "/tests/fixtures/example.lisp")))
           (cfg (config:parse-config sexp)))
      (ok (config:file-ignored-p cfg #P"/tests/fixtures/example.lisp"))
      (ok (not (config:file-ignored-p cfg #P"/tests/other.lisp")))))

  (testing "Ignore directory pattern with trailing /"
    (let* ((sexp '(:mallet-config
                   (:ignore "/tests/fixtures/")))
           (cfg (config:parse-config sexp)))
      (ok (config:file-ignored-p cfg #P"/tests/fixtures/example.lisp"))
      (ok (config:file-ignored-p cfg #P"/tests/fixtures/nested/deep.lisp"))
      (ok (not (config:file-ignored-p cfg #P"/tests/main-test.lisp")))))

  (testing "Ignore wildcard patterns"
    (let* ((sexp '(:mallet-config
                   (:ignore "**/*-generated.lisp")))
           (cfg (config:parse-config sexp)))
      (ok (config:file-ignored-p cfg #P"/src/auto-generated.lisp"))
      (ok (config:file-ignored-p cfg #P"/tests/fixtures/foo-generated.lisp"))
      (ok (not (config:file-ignored-p cfg #P"/src/main.lisp")))))

  (testing "Multiple ignore patterns"
    (let* ((sexp '(:mallet-config
                   (:ignore "/tests/fixtures/"
                            "**/*-generated.lisp"
                            "/build/")))
           (cfg (config:parse-config sexp)))
      (ok (config:file-ignored-p cfg #P"/tests/fixtures/example.lisp"))
      (ok (config:file-ignored-p cfg #P"/src/auto-generated.lisp"))
      (ok (config:file-ignored-p cfg #P"/build/output.lisp"))
      (ok (not (config:file-ignored-p cfg #P"/src/main.lisp")))))

  (testing "Ignore patterns with specific extensions"
    (let* ((sexp '(:mallet-config
                   (:ignore "**/*.asd")))
           (cfg (config:parse-config sexp)))
      (ok (config:file-ignored-p cfg #P"/mallet.asd"))
      (ok (config:file-ignored-p cfg #P"/tests/test-system.asd"))
      (ok (not (config:file-ignored-p cfg #P"/src/main.lisp")))))

  (testing "Ignore nested directories"
    (let* ((sexp '(:mallet-config
                   (:ignore "/.qlot/")))
           (cfg (config:parse-config sexp)))
      (ok (config:file-ignored-p cfg #P"/.qlot/local-projects/foo/src/main.lisp"))
      (ok (config:file-ignored-p cfg #P"/.qlot/dists/quicklisp/software/alexandria.lisp"))
      (ok (not (config:file-ignored-p cfg #P"/src/main.lisp"))))))

;;; Comprehensive :for-paths pattern tests

(deftest for-paths-patterns
  (testing "For-paths with exact file path"
    (let* ((sexp '(:mallet-config
                   (:enable :unused-variables)
                   (:enable :line-length :max 80)
                   (:for-paths ("/src/parser.lisp")
                    (:disable :unused-variables))))
           (cfg (config:parse-config sexp)))
      ;; For src/parser.lisp, unused-variables should be disabled
      (let* ((rules (config:get-rules-for-file cfg #P"/src/parser.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        (ok (not (member :unused-variables rule-names)))
        (ok (member :line-length rule-names)))
      ;; For other files, both rules should be enabled
      (let* ((rules (config:get-rules-for-file cfg #P"/src/main.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        (ok (member :unused-variables rule-names))
        (ok (member :line-length rule-names)))))

  (testing "For-paths with directory pattern"
    (let* ((sexp '(:mallet-config
                   (:enable :unused-variables)
                   (:enable :line-length :max 80)
                   (:for-paths ("/tests/")
                    (:enable :line-length :max 120)
                    (:disable :unused-variables))))
           (cfg (config:parse-config sexp)))
      ;; For test files, line-length should be 120 and unused-variables disabled
      (let* ((rules (config:get-rules-for-file cfg #P"/tests/main-test.lisp"))
             (rule-names (mapcar #'rules:rule-name rules))
             (line-length-rule (find :line-length rules :key #'rules:rule-name)))
        (ok (not (member :unused-variables rule-names)))
        (ok (= 120 (rules:line-length-rule-max-length line-length-rule))))
      ;; For src files, should use base config
      (let* ((rules (config:get-rules-for-file cfg #P"/src/main.lisp"))
             (rule-names (mapcar #'rules:rule-name rules))
             (line-length-rule (find :line-length rules :key #'rules:rule-name)))
        (ok (member :unused-variables rule-names))
        (ok (= 80 (rules:line-length-rule-max-length line-length-rule))))))

  (testing "For-paths with multiple file paths"
    (let* ((sexp '(:mallet-config
                   (:enable :unused-variables)
                   (:for-paths ("/src/parser.lisp" "/src/tokenizer.lisp")
                    (:disable :unused-variables))))
           (cfg (config:parse-config sexp)))
      ;; Both specified files should have unused-variables disabled
      (ok (not (member :unused-variables
                       (mapcar #'rules:rule-name
                               (config:get-rules-for-file cfg #P"/src/parser.lisp")))))
      (ok (not (member :unused-variables
                       (mapcar #'rules:rule-name
                               (config:get-rules-for-file cfg #P"/src/tokenizer.lisp")))))
      ;; Other files should have it enabled
      (ok (member :unused-variables
                  (mapcar #'rules:rule-name
                          (config:get-rules-for-file cfg #P"/src/main.lisp"))))))

  (testing "For-paths with wildcard patterns"
    (let* ((sexp '(:mallet-config
                   (:enable :unused-variables)
                   (:for-paths ("**/*-test.lisp")
                    (:disable :unused-variables))))
           (cfg (config:parse-config sexp)))
      ;; Test files should have unused-variables disabled
      (ok (not (member :unused-variables
                       (mapcar #'rules:rule-name
                               (config:get-rules-for-file cfg #P"/tests/main-test.lisp")))))
      (ok (not (member :unused-variables
                       (mapcar #'rules:rule-name
                               (config:get-rules-for-file cfg #P"/src/parser-test.lisp")))))
      ;; Non-test files should have it enabled
      (ok (member :unused-variables
                  (mapcar #'rules:rule-name
                          (config:get-rules-for-file cfg #P"/src/main.lisp"))))))

  (testing "For-paths with multiple rule overrides"
    (let* ((sexp '(:mallet-config
                   (:enable :unused-variables)
                   (:enable :line-length :max 80)
                   (:enable :trailing-whitespace)
                   (:for-paths ("/scripts/")
                    (:enable :line-length :max 100)
                    (:disable :unused-variables)
                    (:disable :trailing-whitespace))))
           (cfg (config:parse-config sexp)))
      ;; For scripts, multiple rules should be overridden
      (let* ((rules (config:get-rules-for-file cfg #P"/scripts/build.lisp"))
             (rule-names (mapcar #'rules:rule-name rules))
             (line-length-rule (find :line-length rules :key #'rules:rule-name)))
        (ok (not (member :unused-variables rule-names)))
        (ok (not (member :trailing-whitespace rule-names)))
        (ok (= 100 (rules:line-length-rule-max-length line-length-rule))))
      ;; For other files, base config applies
      (let* ((rules (config:get-rules-for-file cfg #P"/src/main.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        (ok (member :unused-variables rule-names))
        (ok (member :trailing-whitespace rule-names)))))

  (testing "For-paths with nested directory patterns"
    (let* ((sexp '(:mallet-config
                   (:enable :line-length :max 80)
                   (:for-paths ("/tests/fixtures/")
                    (:enable :line-length :max 200))))
           (cfg (config:parse-config sexp)))
      ;; Files in nested fixtures should use overridden config
      (let* ((rules (config:get-rules-for-file cfg #P"/tests/fixtures/nested/deep/example.lisp"))
             (line-length-rule (find :line-length rules :key #'rules:rule-name)))
        (ok (= 200 (rules:line-length-rule-max-length line-length-rule))))
      ;; Files outside should use base config
      (let* ((rules (config:get-rules-for-file cfg #P"/tests/main-test.lisp"))
             (line-length-rule (find :line-length rules :key #'rules:rule-name)))
        (ok (= 80 (rules:line-length-rule-max-length line-length-rule))))))

  (testing "For-paths with .asd files"
    (let* ((sexp '(:mallet-config
                   (:enable :line-length :max 80)
                   (:for-paths ("*.asd")
                    (:enable :line-length :max 100))))
           (cfg (config:parse-config sexp)))
      ;; .asd files should use overridden config
      (let* ((rules (config:get-rules-for-file cfg #P"/mallet.asd"))
             (line-length-rule (find :line-length rules :key #'rules:rule-name)))
        (ok (= 100 (rules:line-length-rule-max-length line-length-rule))))
      ;; .lisp files should use base config
      (let* ((rules (config:get-rules-for-file cfg #P"/src/main.lisp"))
             (line-length-rule (find :line-length rules :key #'rules:rule-name)))
        (ok (= 80 (rules:line-length-rule-max-length line-length-rule)))))))

;;; Preset override tests

(deftest preset-override
  (testing "Override :extends with preset-override parameter"
    (let* ((sexp '(:mallet-config
                   (:extends :default)
                   (:enable :line-length :max 100)
                   (:disable :trailing-whitespace)))
           ;; Override with :all preset
           (cfg (config:parse-config sexp :preset-override :all)))
      ;; Should inherit rules from :all (not :default)
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        ;; :all includes cyclomatic-complexity, :default doesn't
        (ok (member :cyclomatic-complexity rule-names))
        ;; Should have line-length with overridden max-length from config
        (let ((line-length-rule (find :line-length (config:config-rules cfg)
                                      :key #'rules:rule-name)))
          (ok (= 100 (rules:line-length-rule-max-length line-length-rule)))))
      ;; Should respect :disable from config file
      (ok (member :trailing-whitespace (config:config-disabled-rules cfg)))))

  (testing "Preset override when no :extends in config"
    (let* ((sexp '(:mallet-config
                   (:enable :line-length :max 120)
                   (:disable :no-tabs)))
           ;; Provide preset-override even though there's no :extends
           (cfg (config:parse-config sexp :preset-override :default)))
      ;; Should use :default as base
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        ;; :default includes trailing-whitespace
        (ok (member :trailing-whitespace rule-names))
        ;; Should have line-length with custom max-length
        (let ((line-length-rule (find :line-length (config:config-rules cfg)
                                      :key #'rules:rule-name)))
          (ok (= 120 (rules:line-length-rule-max-length line-length-rule)))))
      ;; Should respect :disable from config file
      (ok (member :no-tabs (config:config-disabled-rules cfg)))))

  (testing "Load config file with preset override"
    (let ((config-content "(:mallet-config
                              (:extends :default)
                              (:enable :line-length :max 100)
                              (:disable :unused-variables))"))
      (with-temporary-config (config-content tmpfile)
        (let ((cfg (config:load-config tmpfile :preset-override :all)))
          ;; Should use :all as base (not :default from file)
          (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
            ;; :all includes cyclomatic-complexity
            (ok (member :cyclomatic-complexity rule-names))
            ;; Should have custom line-length
            (let ((line-length-rule (find :line-length (config:config-rules cfg)
                                          :key #'rules:rule-name)))
              (ok (= 100 (rules:line-length-rule-max-length line-length-rule)))))
          ;; Should respect :disable from config file
          (ok (member :unused-variables (config:config-disabled-rules cfg)))))))

  (testing "Preset override respects :for-paths from config"
    (let* ((sexp '(:mallet-config
                   (:extends :default)
                   (:for-paths ("tests/**/*.lisp")
                    (:enable :line-length :max 120)
                    (:disable :unused-variables))))
           (cfg (config:parse-config sexp :preset-override :all)))
      ;; Should have path overrides
      (ok (= 1 (length (config:config-path-rules cfg))))
      ;; For test files, should get path-specific rules
      (let* ((rules (config:get-rules-for-file cfg #P"/tests/foo-test.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        (ok (member :line-length rule-names))
        (ok (not (member :unused-variables rule-names))))
      ;; For non-test files, should get base rules from :all
      (let* ((rules (config:get-rules-for-file cfg #P"/src/main.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        ;; :all includes cyclomatic-complexity
        (ok (member :cyclomatic-complexity rule-names))))))

;;; :for-paths inheritance tests

(deftest for-paths-inherits-project-wide-config
  (testing ":for-paths inherits project-wide :disable settings"
    (let* ((sexp '(:mallet-config
                   (:extends :default)
                   (:disable :missing-else)  ; Project-wide disable
                   (:for-paths ("src/main.lisp")
                    (:enable :line-length :max 100))))
           (cfg (config:parse-config sexp)))
      ;; For src/main.lisp, :missing-else should be disabled (inherited from project-wide)
      (let* ((rules (config:get-rules-for-file cfg #P"/src/main.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        (ok (not (member :missing-else rule-names)) "Project-wide :disable should apply to :for-paths")
        (ok (member :line-length rule-names) ":for-paths :enable should work"))
      ;; For other files, :missing-else should also be disabled
      (let* ((rules (config:get-rules-for-file cfg #P"/src/other.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        (ok (not (member :missing-else rule-names)) "Project-wide :disable should apply to all files"))))

  (testing ":for-paths inherits project-wide :enable settings"
    (let* ((sexp '(:mallet-config
                   (:extends :default)
                   (:enable :line-length :max 80)  ; Project-wide enable
                   (:for-paths ("tests/**/*.lisp")
                    (:enable :line-length :max 120))))  ; Override max-length for tests
           (cfg (config:parse-config sexp)))
      ;; For test files, should have line-length with test-specific max
      (let* ((rules (config:get-rules-for-file cfg #P"/tests/foo-test.lisp"))
             (line-length-rule (find :line-length rules :key #'rules:rule-name)))
        (ok (not (null line-length-rule)) "Should inherit :line-length from project-wide")
        (ok (= 120 (rules:line-length-rule-max-length line-length-rule))
            ":for-paths override should apply"))
      ;; For other files, should have line-length with project-wide max
      (let* ((rules (config:get-rules-for-file cfg #P"/src/main.lisp"))
             (line-length-rule (find :line-length rules :key #'rules:rule-name)))
        (ok (not (null line-length-rule)))
        (ok (= 80 (rules:line-length-rule-max-length line-length-rule))
            "Project-wide settings should apply"))))

  (testing ":for-paths can override project-wide :disable"
    (let* ((sexp '(:mallet-config
                   (:extends :default)
                   (:disable :unused-variables)  ; Project-wide disable
                   (:for-paths ("src/critical.lisp")
                    (:enable :unused-variables))))  ; Re-enable for critical file
           (cfg (config:parse-config sexp)))
      ;; For src/critical.lisp, :unused-variables should be enabled
      (let* ((rules (config:get-rules-for-file cfg #P"/src/critical.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        (ok (member :unused-variables rule-names)
            ":for-paths :enable should override project-wide :disable"))
      ;; For other files, :unused-variables should be disabled
      (let* ((rules (config:get-rules-for-file cfg #P"/src/main.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        (ok (not (member :unused-variables rule-names))
            "Project-wide :disable should apply"))))

  (testing "Priority: extends < project-wide < :for-paths < CLI"
    (let* ((sexp '(:mallet-config
                   (:extends :default)              ; :default has :unused-variables enabled
                   (:disable :unused-variables)     ; Project-wide disables it
                   (:enable :line-length :max 80)   ; Project-wide enables line-length
                   (:for-paths ("tests/**/*.lisp")
                    (:enable :unused-variables)     ; Re-enable for tests
                    (:disable :line-length))))      ; Disable line-length for tests
           (cfg (config:parse-config sexp)))
      ;; For test files
      (let* ((rules (config:get-rules-for-file cfg #P"/tests/foo-test.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        (ok (member :unused-variables rule-names)
            ":for-paths should override project-wide :disable")
        (ok (not (member :line-length rule-names))
            ":for-paths should override project-wide :enable"))
      ;; For other files
      (let* ((rules (config:get-rules-for-file cfg #P"/src/main.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        (ok (not (member :unused-variables rule-names))
            "Project-wide :disable should override :extends")
        (ok (member :line-length rule-names)
            "Project-wide :enable should work")))))

;;; Config discovery tests

(deftest find-config-file
  (testing "Find config in current directory"
    (let* ((tmpdir-str (format nil "/tmp/mallet-test-dir-~A" (get-universal-time)))
           (tmpdir (uiop:ensure-directory-pathname tmpdir-str))
           (config-file (merge-pathnames ".mallet.lisp" tmpdir)))
      (unwind-protect
           (progn
             (ensure-directories-exist tmpdir)
             (with-open-file (out config-file :direction :output :if-exists :supersede)
               (write-string "(:mallet-config)" out))
             (let ((found (config:find-config-file tmpdir)))
               (ok (not (null found)))
               ;; Compare truenames to handle pathname variations
               (ok (equal (truename found) (truename config-file)))))
        ;; Cleanup
        (when (probe-file config-file)
          (delete-file config-file))
        (when (probe-file tmpdir)
          (uiop:delete-directory-tree tmpdir :validate t)))))

  (testing "Find config in parent directory"
    (let* ((tmpdir-str (format nil "/tmp/mallet-test-parent-~A" (get-universal-time)))
           (tmpdir (uiop:ensure-directory-pathname tmpdir-str))
           (subdir (merge-pathnames "sub/" tmpdir))
           (config-file (merge-pathnames ".mallet.lisp" tmpdir)))
      (unwind-protect
           (progn
             (ensure-directories-exist subdir)
             (with-open-file (out config-file :direction :output :if-exists :supersede)
               (write-string "(:mallet-config)" out))
             ;; Search from subdir should find config in parent
             (let ((found (config:find-config-file subdir)))
               (ok (not (null found)))
               ;; Compare truenames to handle pathname variations
               (ok (equal (truename found) (truename config-file)))))
        ;; Cleanup
        (when (probe-file config-file)
          (delete-file config-file))
        (when (probe-file tmpdir)
          (uiop:delete-directory-tree tmpdir :validate t :if-does-not-exist :ignore))))))

;;; CLI override tests

;;; :set-severity directive tests

(deftest set-severity-directive
  (testing "Set severity for all rules in a category"
    (let* ((sexp '(:mallet-config
                   (:enable :unused-variables)  ; :cleanliness category, default :warning
                   (:set-severity :cleanliness :error)))
           (cfg (config:parse-config sexp)))
      (let ((rule (find :unused-variables (config:config-rules cfg)
                        :key #'rules:rule-name)))
        (ok (not (null rule)))
        (ok (eq :error (rules:rule-severity rule))))))

  (testing "Set severity does not affect rules in other categories"
    (let* ((sexp '(:mallet-config
                   (:enable :unused-variables)    ; :cleanliness
                   (:enable :wrong-otherwise)     ; :correctness, default :error
                   (:set-severity :cleanliness :info)))
           (cfg (config:parse-config sexp)))
      (let ((unused (find :unused-variables (config:config-rules cfg) :key #'rules:rule-name))
            (wrong (find :wrong-otherwise (config:config-rules cfg) :key #'rules:rule-name)))
        (ok (eq :info (rules:rule-severity unused)))
        (ok (eq :error (rules:rule-severity wrong))))))

  (testing "Duplicate :set-severity for same category — last one wins"
    (let* ((sexp '(:mallet-config
                   (:enable :unused-variables)     ; :cleanliness
                   (:set-severity :cleanliness :error)
                   (:set-severity :cleanliness :info)))  ; last wins
           (cfg (config:parse-config sexp)))
      (let ((unused (find :unused-variables (config:config-rules cfg) :key #'rules:rule-name)))
        (ok (eq :info (rules:rule-severity unused))
            "last :set-severity for a category should win"))))

  (testing "Multiple :set-severity directives for different categories"
    (let* ((sexp '(:mallet-config
                   (:enable :unused-variables)     ; :cleanliness
                   (:enable :trailing-whitespace)  ; :format
                   (:set-severity :cleanliness :error)
                   (:set-severity :format :info)))
           (cfg (config:parse-config sexp)))
      (let ((unused (find :unused-variables (config:config-rules cfg) :key #'rules:rule-name))
            (whitespace (find :trailing-whitespace (config:config-rules cfg) :key #'rules:rule-name)))
        (ok (eq :error (rules:rule-severity unused)))
        (ok (eq :info (rules:rule-severity whitespace))))))

  (testing ":set-severity with :extends applies to inherited rules"
    (let* ((sexp '(:mallet-config
                   (:extends :default)
                   (:set-severity :format :error)))
           (cfg (config:parse-config sexp)))
      ;; Trailing whitespace is :format category and should be :error now
      (let ((rule (find :trailing-whitespace (config:config-rules cfg)
                        :key #'rules:rule-name)))
        (ok (not (null rule)))
        (ok (eq :error (rules:rule-severity rule)))))))

(deftest set-severity-validates-severity
  (testing "Invalid severity signals a descriptive error"
    (let ((sexp '(:mallet-config
                  (:enable :unused-variables)
                  (:set-severity :cleanliness :foo))))
      (ok (signals (config:parse-config sexp) 'simple-error)
          ":set-severity with invalid severity should signal an error")))

  (testing "Valid severities are accepted without error"
    (dolist (sev '(:error :warning :info))
      (let* ((sexp `(:mallet-config
                     (:enable :unused-variables)
                     (:set-severity :cleanliness ,sev)))
             (cfg (config:parse-config sexp)))
        (ok (not (null cfg))
            (format nil ":set-severity ~S should be accepted" sev))))))

(deftest set-severity-applies-to-for-paths
  (testing ":set-severity propagates into :for-paths rules"
    (let* ((sexp '(:mallet-config
                   (:enable :trailing-whitespace)    ; :format, default :info
                   (:set-severity :format :error)
                   (:for-paths ("tests/**/*.lisp")
                    (:enable :unused-variables))))   ; :cleanliness, unaffected category
           (cfg (config:parse-config sexp)))
      ;; For test files: trailing-whitespace should have :error severity from :set-severity
      (let* ((path-rules (config:get-rules-for-file cfg #P"/tests/foo-test.lisp"))
             (whitespace-rule (find :trailing-whitespace path-rules :key #'rules:rule-name)))
        (ok (not (null whitespace-rule))
            ":trailing-whitespace should be present in :for-paths rules")
        (ok (eq :error (rules:rule-severity whitespace-rule))
            ":set-severity :format :error should apply to :for-paths rules"))))

  (testing ":set-severity applies to rules enabled inside :for-paths"
    (let* ((sexp '(:mallet-config
                   (:set-severity :format :error)
                   (:for-paths ("tests/**/*.lisp")
                    (:enable :trailing-whitespace))))  ; :format category
           (cfg (config:parse-config sexp)))
      (let* ((path-rules (config:get-rules-for-file cfg #P"/tests/foo-test.lisp"))
             (whitespace-rule (find :trailing-whitespace path-rules :key #'rules:rule-name)))
        (ok (not (null whitespace-rule))
            ":trailing-whitespace should be in :for-paths rules")
        (ok (eq :error (rules:rule-severity whitespace-rule))
            ":set-severity should apply to rules enabled inside :for-paths"))))

  (testing ":set-severity and :for-paths: base rules also get correct severity"
    (let* ((sexp '(:mallet-config
                   (:enable :trailing-whitespace)     ; :format
                   (:set-severity :format :error)
                   (:for-paths ("tests/**/*.lisp")
                    (:enable :unused-variables))))
           (cfg (config:parse-config sexp)))
      ;; For non-test files: trailing-whitespace severity should be :error
      (let* ((base-rules (config:get-rules-for-file cfg #P"/src/main.lisp"))
             (whitespace-rule (find :trailing-whitespace base-rules :key #'rules:rule-name)))
        (ok (not (null whitespace-rule)))
        (ok (eq :error (rules:rule-severity whitespace-rule))
            ":set-severity should apply to base rules too")))))

(deftest apply-cli-overrides-enable-rule
  (testing "Enable a rule that's not in base config"
    (let* ((base-config (config:get-built-in-config :default))
           (cli-rules '(:enable-rules ((:line-length :max 120))
                        :disable-rules ()))
           (merged (config:apply-cli-overrides base-config cli-rules)))
      (ok (find :line-length (config:config-rules merged)
                :key #'rules:rule-name))
      ;; Check that the option was applied
      (let ((rule (find :line-length (config:config-rules merged)
                        :key #'rules:rule-name)))
        (ok (not (null rule)))))))

(deftest apply-cli-overrides-disable-rule
  (testing "Disable a rule from base config"
    (let* ((base-config (config:get-built-in-config :default))
           (cli-rules '(:enable-rules ()
                        :disable-rules (:trailing-whitespace)))
           (merged (config:apply-cli-overrides base-config cli-rules)))
      (ok (not (find :trailing-whitespace (config:config-rules merged)
                     :key #'rules:rule-name)))
      (ok (member :trailing-whitespace (config:config-disabled-rules merged))))))

(deftest apply-cli-overrides-empty
  (testing "Empty CLI rules doesn't change config"
    (let* ((base-config (config:get-built-in-config :default))
           (cli-rules '(:enable-rules ()
                        :disable-rules ()))
           (merged (config:apply-cli-overrides base-config cli-rules)))
      ;; Should have same number of rules
      (ok (= (length (config:config-rules base-config))
             (length (config:config-rules merged)))))))

(deftest apply-cli-overrides-combined
  (testing "Complex combination of CLI overrides"
    (let* ((base-config (config:get-built-in-config :default))
           (cli-rules '(:enable-rules ((:line-length :max 120))
                        :disable-rules (:missing-else)))
           (merged (config:apply-cli-overrides base-config cli-rules)))
      ;; line-length enabled with custom option
      (ok (find :line-length (config:config-rules merged)
                :key #'rules:rule-name))
      ;; if-without-else disabled
      (ok (member :missing-else (config:config-disabled-rules merged))))))

;;; Severity precedence contract tests:
;;; per-rule :severity > :set-severity > rule default

(deftest per-rule-severity-wins-over-set-severity
  (testing "explicit per-rule :severity wins over :set-severity for the same category"
    ;; (:enable :unused-variables :severity :warning) wins over
    ;; (:set-severity :cleanliness :info) — per-rule is more specific.
    (let* ((sexp '(:mallet-config
                   (:enable :unused-variables :severity :warning)
                   (:set-severity :cleanliness :info)))
           (cfg (config:parse-config sexp)))
      (let ((rule (find :unused-variables (config:config-rules cfg)
                        :key #'rules:rule-name)))
        (ok (not (null rule)))
        (ok (eq :warning (rules:rule-severity rule))
            "per-rule :severity :warning wins over :set-severity :info"))))

  (testing ":set-severity applies to rules without explicit per-rule :severity"
    ;; A rule enabled without :severity gets the :set-severity override.
    (let* ((sexp '(:mallet-config
                   (:enable :unused-variables)
                   (:set-severity :cleanliness :error)))
           (cfg (config:parse-config sexp)))
      (let ((rule (find :unused-variables (config:config-rules cfg)
                        :key #'rules:rule-name)))
        (ok (not (null rule)))
        (ok (eq :error (rules:rule-severity rule))
            ":set-severity applies when no per-rule :severity is given"))))

  (testing "per-rule :severity wins over :set-severity inside :for-paths"
    (let* ((sexp '(:mallet-config
                   (:set-severity :cleanliness :error)
                   (:for-paths ("tests/**/*.lisp")
                    (:enable :unused-variables :severity :warning))))
           (cfg (config:parse-config sexp)))
      (let* ((path-rules (config:get-rules-for-file cfg #P"/tests/foo-test.lisp"))
             (rule (find :unused-variables path-rules :key #'rules:rule-name)))
        (ok (not (null rule)))
        (ok (eq :warning (rules:rule-severity rule))
            "per-rule :severity :warning wins over :set-severity :error inside :for-paths")))))

;;; CLI --enable and :set-severity interaction tests

(deftest apply-cli-overrides-set-severity-interaction
  (testing "CLI-enabled rule not in base config gets :set-severity override applied"
    ;; When a rule is absent from the base config and enabled on the CLI without
    ;; an explicit :severity, the config's :set-severity override is applied.
    (let* ((sexp '(:mallet-config
                   (:extends :none)
                   (:set-severity :format :error)))
           (base-config (config:parse-config sexp))
           (cli-rules '(:enable-rules ((:trailing-whitespace))
                        :disable-rules ()))
           (merged (config:apply-cli-overrides base-config cli-rules)))
      (let ((rule (find :trailing-whitespace (config:config-rules merged)
                        :key #'rules:rule-name)))
        (ok (not (null rule)) ":trailing-whitespace should be in merged rules")
        (ok (eq :error (rules:rule-severity rule))
            "CLI-enabled rule gets :set-severity from config when no explicit :severity given"))))

  (testing "CLI-enabled rule that IS in base config preserves :set-severity from config"
    ;; When the rule already exists in the base config, Step 1 of
    ;; apply-cli-overrides copies the config-applied severity unless the
    ;; CLI provides an explicit :severity override.
    (let* ((sexp '(:mallet-config
                   (:enable :trailing-whitespace)   ; :format category
                   (:set-severity :format :error)))
           (base-config (config:parse-config sexp))
           (cli-rules '(:enable-rules ((:trailing-whitespace))
                        :disable-rules ()))
           (merged (config:apply-cli-overrides base-config cli-rules)))
      (let ((rule (find :trailing-whitespace (config:config-rules merged)
                        :key #'rules:rule-name)))
        (ok (not (null rule)))
        (ok (eq :error (rules:rule-severity rule))
            "Existing rule re-enabled by CLI preserves config-applied severity"))))

  (testing "Explicit CLI :severity wins over :set-severity (per-rule > set-severity)"
    (let* ((sexp '(:mallet-config
                   (:extends :none)
                   (:set-severity :format :error)))
           (base-config (config:parse-config sexp))
           (cli-rules '(:enable-rules ((:trailing-whitespace :severity :info))
                        :disable-rules ()))
           (merged (config:apply-cli-overrides base-config cli-rules)))
      (let ((rule (find :trailing-whitespace (config:config-rules merged)
                        :key #'rules:rule-name)))
        (ok (not (null rule)))
        (ok (eq :info (rules:rule-severity rule))
            "Explicit CLI :severity :info wins over :set-severity :error")))))

;;; read-mallet-forms tests

(deftest read-mallet-forms
  (testing "single :mallet-config form — backward compatible"
    (with-temporary-config ("(:mallet-config (:enable :line-length))" path)
      (multiple-value-bind (presets config)
          (config:read-mallet-forms path)
        (ok (null presets))
        (ok (not (null config)))
        (ok (eq :mallet-config (first config))))))

  (testing "empty file returns nil config and empty preset list"
    (with-temporary-config ("" path)
      (multiple-value-bind (presets config)
          (config:read-mallet-forms path)
        (ok (null presets))
        (ok (null config)))))

  (testing "file with preset and config returns both"
    (with-temporary-config ("(:mallet-preset :strict (:enable :line-length))
                             (:mallet-config (:extends :strict))" path)
      (multiple-value-bind (presets config)
          (config:read-mallet-forms path)
        (ok (= 1 (length presets)))
        (ok (eq :mallet-preset (first (first presets))))
        (ok (eq :strict (second (first presets))))
        (ok (not (null config)))
        (ok (eq :mallet-config (first config))))))

  (testing "multiple presets and config"
    (with-temporary-config ("(:mallet-preset :strict (:enable :line-length))
                             (:mallet-preset :ci (:extends :strict))
                             (:mallet-config (:extends :ci))" path)
      (multiple-value-bind (presets config)
          (config:read-mallet-forms path)
        (ok (= 2 (length presets)))
        (ok (eq :mallet-config (first config))))))

  (testing "presets only, no config"
    (with-temporary-config ("(:mallet-preset :strict (:enable :line-length))
                             (:mallet-preset :ci (:extends :strict))" path)
      (multiple-value-bind (presets config)
          (config:read-mallet-forms path)
        (ok (= 2 (length presets)))
        (ok (null config)))))

  (testing "unknown top-level form signals unknown-config-form"
    (with-temporary-config ("(:bad-keyword :foo)" path)
      (ok (signals (config:read-mallet-forms path)
                   'errors:unknown-config-form))))

  (testing "multiple :mallet-config forms signals multiple-config-forms"
    (with-temporary-config ("(:mallet-config (:enable :line-length))
                             (:mallet-config (:disable :no-tabs))" path)
      (ok (signals (config:read-mallet-forms path)
                   'errors:multiple-config-forms))))

  (testing "duplicate preset name signals duplicate-preset-name"
    (with-temporary-config ("(:mallet-preset :strict (:enable :line-length))
                             (:mallet-preset :strict (:disable :no-tabs))" path)
      (ok (signals (config:read-mallet-forms path)
                   'errors:duplicate-preset-name))))

  (testing "*read-eval* nil prevents code execution"
    (with-temporary-config ("(:mallet-config #.(error \"read-eval executed\"))" path)
      (ok (signals (config:read-mallet-forms path) 'reader-error)
          "reader-error from #. with *read-eval* nil — no code executed"))))

;;; Preset definition parsing tests

(deftest parse-preset-definition
  (testing "Parse minimal preset (name only, no sub-forms)"
    (let* ((sexp '(:mallet-preset :minimal))
           (defn (config:parse-preset-definition sexp)))
      (ok (eq :minimal (config:preset-definition-name defn)))
      (ok (null (config:preset-definition-extends defn)))
      (ok (null (config:preset-definition-enable-specs defn)))
      (ok (null (config:preset-definition-disable-specs defn)))))

  (testing "Parse preset with :extends"
    (let* ((sexp '(:mallet-preset :strict
                   (:extends :default)))
           (defn (config:parse-preset-definition sexp)))
      (ok (eq :strict (config:preset-definition-name defn)))
      (ok (eq :default (config:preset-definition-extends defn)))))

  (testing "Parse preset with :enable specs"
    (let* ((sexp '(:mallet-preset :custom
                   (:enable :line-length :max 100)
                   (:enable :unused-variables)))
           (defn (config:parse-preset-definition sexp)))
      (ok (= 2 (length (config:preset-definition-enable-specs defn))))
      (let* ((specs (config:preset-definition-enable-specs defn))
             (first-spec (first specs))
             (second-spec (second specs)))
        (ok (eq :line-length (car first-spec)))
        (ok (= 100 (getf (cdr first-spec) :max)))
        (ok (eq :unused-variables (car second-spec))))))

  (testing "Parse preset with :disable specs"
    (let* ((sexp '(:mallet-preset :relaxed
                   (:extends :default)
                   (:disable :missing-else)
                   (:disable :unused-variables)))
           (defn (config:parse-preset-definition sexp)))
      (ok (= 2 (length (config:preset-definition-disable-specs defn))))
      (ok (member :missing-else (config:preset-definition-disable-specs defn)))
      (ok (member :unused-variables (config:preset-definition-disable-specs defn)))))

  (testing "Parse preset with all options"
    (let* ((sexp '(:mallet-preset :ci
                   (:extends :strict)
                   (:enable :missing-docstring)
                   (:enable :line-length :max 120)
                   (:disable :progn-in-conditional)))
           (defn (config:parse-preset-definition sexp)))
      (ok (eq :ci (config:preset-definition-name defn)))
      (ok (eq :strict (config:preset-definition-extends defn)))
      (ok (= 2 (length (config:preset-definition-enable-specs defn))))
      (ok (= 1 (length (config:preset-definition-disable-specs defn))))
      (ok (member :progn-in-conditional (config:preset-definition-disable-specs defn)))))

  (testing "parse-preset-definition resolves rule aliases"
    ;; Ensure that rule aliases (if any) are resolved during parsing
    ;; Use :no-tabs as a baseline (it has no alias, but tests the code path)
    (let* ((sexp '(:mallet-preset :test
                   (:enable :no-tabs)))
           (defn (config:parse-preset-definition sexp)))
      (ok (= 1 (length (config:preset-definition-enable-specs defn))))
      (ok (eq :no-tabs (caar (config:preset-definition-enable-specs defn)))))))

;;; Preset registry tests

(deftest build-preset-registry
  (testing "Build empty registry"
    (let ((registry (config:build-preset-registry '())))
      (ok (hash-table-p registry))
      (ok (= 0 (hash-table-count registry)))))

  (testing "Build registry from single preset"
    (let* ((defn (config:parse-preset-definition '(:mallet-preset :strict
                                                   (:extends :default))))
           (registry (config:build-preset-registry (list defn))))
      (ok (= 1 (hash-table-count registry)))
      (ok (not (null (gethash :strict registry))))))

  (testing "Build registry from multiple presets"
    (let* ((defns (list
                   (config:parse-preset-definition '(:mallet-preset :strict
                                                     (:extends :default)))
                   (config:parse-preset-definition '(:mallet-preset :ci
                                                     (:extends :strict)))))
           (registry (config:build-preset-registry defns)))
      (ok (= 2 (hash-table-count registry)))
      (ok (not (null (gethash :strict registry))))
      (ok (not (null (gethash :ci registry))))))

  (testing "Registry keyed by preset name keyword"
    (let* ((defn (config:parse-preset-definition '(:mallet-preset :my-preset)))
           (registry (config:build-preset-registry (list defn)))
           (found (gethash :my-preset registry)))
      (ok (not (null found)))
      (ok (eq :my-preset (config:preset-definition-name found))))))

;;; Preset resolution tests

(deftest resolve-preset
  (testing "Resolve built-in preset :default from empty registry"
    (let* ((registry (config:build-preset-registry '()))
           (cfg (config:resolve-preset :default registry)))
      (ok (typep cfg 'config:config))
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :trailing-whitespace rule-names))
        (ok (member :unused-variables rule-names)))))

  (testing "Resolve built-in preset :none from empty registry"
    (let* ((registry (config:build-preset-registry '()))
           (cfg (config:resolve-preset :none registry)))
      (ok (typep cfg 'config:config))
      (ok (null (config:config-rules cfg)))))

  (testing "Resolve built-in preset :all from empty registry"
    (let* ((registry (config:build-preset-registry '()))
           (cfg (config:resolve-preset :all registry)))
      (ok (typep cfg 'config:config))
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :line-length rule-names))
        (ok (member :cyclomatic-complexity rule-names)))))

  (testing "Resolve user-defined preset with enable specs"
    (let* ((defns (list
                   (config:parse-preset-definition
                    '(:mallet-preset :custom
                      (:enable :line-length :max 100)
                      (:enable :unused-variables)))))
           (registry (config:build-preset-registry defns))
           (cfg (config:resolve-preset :custom registry)))
      (ok (typep cfg 'config:config))
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :line-length rule-names))
        (ok (member :unused-variables rule-names)))
      (let ((rule (find :line-length (config:config-rules cfg) :key #'rules:rule-name)))
        (ok (= 100 (rules:line-length-rule-max-length rule))))))

  (testing "Resolve user-defined preset extending built-in"
    (let* ((defns (list
                   (config:parse-preset-definition
                    '(:mallet-preset :strict
                      (:extends :default)
                      (:enable :line-length :max 80)
                      (:disable :progn-in-conditional)))))
           (registry (config:build-preset-registry defns))
           (cfg (config:resolve-preset :strict registry)))
      (ok (typep cfg 'config:config))
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        ;; Should inherit from :default
        (ok (member :trailing-whitespace rule-names))
        (ok (member :unused-variables rule-names))
        ;; Should have line-length from the preset
        (ok (member :line-length rule-names)))
      ;; Verify disable-specs applied
      (ok (member :progn-in-conditional (config:config-disabled-rules cfg)))
      ;; Verify line-length max
      (let ((rule (find :line-length (config:config-rules cfg) :key #'rules:rule-name)))
        (ok (= 80 (rules:line-length-rule-max-length rule))))))

  (testing "Resolve preset chain (:ci -> :strict -> :default)"
    (let* ((defns (list
                   (config:parse-preset-definition
                    '(:mallet-preset :strict
                      (:extends :default)
                      (:enable :line-length :max 80)))
                   (config:parse-preset-definition
                    '(:mallet-preset :ci
                      (:extends :strict)
                      (:enable :missing-docstring)))))
           (registry (config:build-preset-registry defns))
           (cfg (config:resolve-preset :ci registry)))
      (ok (typep cfg 'config:config))
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        ;; Should inherit from :default (via :strict)
        (ok (member :trailing-whitespace rule-names))
        (ok (member :unused-variables rule-names))
        ;; Should have :line-length from :strict
        (ok (member :line-length rule-names))
        ;; Should have :missing-docstring from :ci
        (ok (member :missing-docstring rule-names)))
      ;; line-length max should be 80 (from :strict)
      (let ((rule (find :line-length (config:config-rules cfg) :key #'rules:rule-name)))
        (ok (= 80 (rules:line-length-rule-max-length rule))))))

  (testing "Circular preset reference signals circular-preset-reference error"
    (let* ((defns (list
                   (config:parse-preset-definition '(:mallet-preset :a (:extends :b)))
                   (config:parse-preset-definition '(:mallet-preset :b (:extends :a)))))
           (registry (config:build-preset-registry defns)))
      (ok (handler-case
              (progn (config:resolve-preset :a registry) nil)
            (errors:circular-preset-reference () t)))))

  (testing "Circular reference error includes the cycle chain"
    (let* ((defns (list
                   (config:parse-preset-definition '(:mallet-preset :a (:extends :b)))
                   (config:parse-preset-definition '(:mallet-preset :b (:extends :a)))))
           (registry (config:build-preset-registry defns)))
      (handler-case
          (config:resolve-preset :a registry)
        (errors:circular-preset-reference (c)
          (let ((chain (errors:circular-preset-reference-chain c)))
            (ok (member :a chain))
            (ok (member :b chain)))))))

  (testing "Unknown preset signals unknown-preset error"
    (let ((registry (config:build-preset-registry '())))
      (ok (handler-case
              (progn (config:resolve-preset :nonexistent registry) nil)
            (errors:unknown-preset () t)))))

  (testing "Unknown preset error includes the requested name"
    (let ((registry (config:build-preset-registry '())))
      (handler-case
          (config:resolve-preset :ghost registry)
        (errors:unknown-preset (c)
          (ok (eq :ghost (errors:unknown-preset-name c)))))))

  (testing "Unknown preset error includes available names"
    (let* ((defns (list
                   (config:parse-preset-definition '(:mallet-preset :strict (:extends :default)))))
           (registry (config:build-preset-registry defns)))
      (handler-case
          (config:resolve-preset :nonexistent registry)
        (errors:unknown-preset (c)
          (let ((available (errors:unknown-preset-available-names c)))
            (ok (member :strict available))
            ;; Built-in names should also be included
            (ok (member :default available)))))))

  (testing "Shadowing built-in :default emits note to *error-output*"
    (let* ((defns (list
                   (config:parse-preset-definition
                    '(:mallet-preset :default
                      (:enable :line-length :max 100)))))
           (registry (config:build-preset-registry defns))
           (output (with-output-to-string (s)
                     (let ((*error-output* s))
                       (config:resolve-preset :default registry)))))
      (ok (search "shadowing built-in" (string-downcase output)))))

  (testing "Shadowing note mentions the preset name"
    (let* ((defns (list
                   (config:parse-preset-definition
                    '(:mallet-preset :all
                      (:enable :line-length :max 100)))))
           (registry (config:build-preset-registry defns))
           (output (with-output-to-string (s)
                     (let ((*error-output* s))
                       (config:resolve-preset :all registry)))))
      (ok (search "all" (string-downcase output)))))

  (testing "Resolving non-built-in user preset does NOT emit shadowing note"
    (let* ((defns (list
                   (config:parse-preset-definition
                    '(:mallet-preset :my-preset
                      (:enable :line-length :max 100)))))
           (registry (config:build-preset-registry defns))
           (output (with-output-to-string (s)
                     (let ((*error-output* s))
                       (config:resolve-preset :my-preset registry)))))
      (ok (zerop (length output)))))

  (testing "Resolve user-defined preset with disable-specs"
    (let* ((defns (list
                   (config:parse-preset-definition
                    '(:mallet-preset :relaxed
                      (:extends :default)
                      (:disable :missing-else)
                      (:disable :unused-variables)))))
           (registry (config:build-preset-registry defns))
           (cfg (config:resolve-preset :relaxed registry)))
      (ok (member :missing-else (config:config-disabled-rules cfg)))
      (ok (member :unused-variables (config:config-disabled-rules cfg)))))

  (testing "User-defined :default extending built-in :default resolves without circular error"
    (let* ((defns (list
                   (config:parse-preset-definition
                    '(:mallet-preset :default
                      (:extends :default)
                      (:enable :line-length :max 120)))))
           (registry (config:build-preset-registry defns))
           (cfg (let ((*error-output* (make-broadcast-stream)))
                  (config:resolve-preset :default registry)))
           (rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
      ;; Should have built-in :default rules PLUS :line-length
      (ok (member :line-length rule-names)
          ":line-length from user-defined :default")
      (ok (member :trailing-whitespace rule-names)
          ":trailing-whitespace inherited from built-in :default")
      (ok (member :unused-variables rule-names)
          ":unused-variables inherited from built-in :default")))

  (testing "resolve-preset and build-preset-registry are exported from mallet/config"
    (ok (find-symbol "RESOLVE-PRESET" :mallet/config))
    (ok (find-symbol "BUILD-PRESET-REGISTRY" :mallet/config))
    (ok (eq :external (nth-value 1 (find-symbol "RESOLVE-PRESET" :mallet/config))))
    (ok (eq :external (nth-value 1 (find-symbol "BUILD-PRESET-REGISTRY" :mallet/config))))))

;;; Edge case tests for preset registry and resolution

(deftest preset-only-disable-specs
  (testing "Preset with only :disable specs and no :extends defaults to :none base"
    (let* ((defns (list
                   (config:parse-preset-definition
                    '(:mallet-preset :minimal
                      (:disable :line-length)
                      (:disable :missing-else)))))
           (registry (config:build-preset-registry defns))
           (cfg (config:resolve-preset :minimal registry)))
      ;; With :none base and no :enable, rules list should be empty
      (ok (null (config:config-rules cfg))
          "No rules should be enabled when no :extends and no :enable")
      ;; But disable-specs should still be recorded
      (ok (member :line-length (config:config-disabled-rules cfg))
          ":line-length should be in disabled list")
      (ok (member :missing-else (config:config-disabled-rules cfg))
          ":missing-else should be in disabled list")))

  (testing "Preset with only :disable specs extending :default removes rules"
    (let* ((defns (list
                   (config:parse-preset-definition
                    '(:mallet-preset :relaxed
                      (:extends :default)
                      (:disable :missing-else)
                      (:disable :unused-variables)))))
           (registry (config:build-preset-registry defns))
           (cfg (config:resolve-preset :relaxed registry)))
      ;; Should still have inherited rules from :default
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :trailing-whitespace rule-names)
            "Should inherit :trailing-whitespace from :default"))
      ;; But disabled rules should be present
      (ok (member :missing-else (config:config-disabled-rules cfg)))
      (ok (member :unused-variables (config:config-disabled-rules cfg))))))

(deftest preset-extending-all-or-none
  (testing "Preset extending :all inherits all rules"
    (let* ((defns (list
                   (config:parse-preset-definition
                    '(:mallet-preset :everything-strict
                      (:extends :all)
                      (:enable :line-length :max 80)))))
           (registry (config:build-preset-registry defns))
           (cfg (config:resolve-preset :everything-strict registry)))
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        ;; Should have rules from :all
        (ok (member :cyclomatic-complexity rule-names)
            "Should inherit :cyclomatic-complexity from :all")
        (ok (member :function-length rule-names)
            "Should inherit :function-length from :all")
        (ok (member :line-length rule-names)
            "Should have :line-length from preset enable"))
      ;; :all has no disabled rules, so disabled list should be empty
      (ok (null (config:config-disabled-rules cfg))
          "No rules should be disabled when extending :all")
      ;; line-length should have overridden max
      (let ((rule (find :line-length (config:config-rules cfg) :key #'rules:rule-name)))
        (ok (= 80 (rules:line-length-rule-max-length rule))
            "line-length :max should be 80 from preset override"))))

  (testing "Preset extending :none starts clean"
    (let* ((defns (list
                   (config:parse-preset-definition
                    '(:mallet-preset :barebones
                      (:extends :none)
                      (:enable :trailing-whitespace)
                      (:enable :no-tabs)))))
           (registry (config:build-preset-registry defns))
           (cfg (config:resolve-preset :barebones registry)))
      ;; Should only have the two explicitly enabled rules
      (ok (= 2 (length (config:config-rules cfg)))
          "Should have exactly 2 rules")
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :trailing-whitespace rule-names))
        (ok (member :no-tabs rule-names))
        (ok (not (member :unused-variables rule-names))
            "Should NOT have rules from :default")))))

(deftest preset-with-rule-options
  (testing "Preset preserves rule options through resolution"
    (let* ((defns (list
                   (config:parse-preset-definition
                    '(:mallet-preset :custom-metrics
                      (:extends :none)
                      (:enable :line-length :max 120)
                      (:enable :unused-variables :severity :error)))))
           (registry (config:build-preset-registry defns))
           (cfg (config:resolve-preset :custom-metrics registry)))
      ;; Verify exact rule count (no extras from :none base)
      (ok (= 2 (length (config:config-rules cfg)))
          "Should have exactly 2 rules")
      ;; Verify line-length :max is preserved
      (let ((ll-rule (find :line-length (config:config-rules cfg)
                           :key #'rules:rule-name)))
        (ok (not (null ll-rule)) "line-length rule should exist")
        (ok (= 120 (rules:line-length-rule-max-length ll-rule))
            "line-length :max should be 120"))
      ;; Verify :severity option is preserved
      (let ((uv-rule (find :unused-variables (config:config-rules cfg)
                           :key #'rules:rule-name)))
        (ok (not (null uv-rule)) "unused-variables rule should exist")
        (ok (eq :error (rules:rule-severity uv-rule))
            "unused-variables :severity should be :error"))))

  (testing "Child preset overrides parent rule options"
    (let* ((defns (list
                   (config:parse-preset-definition
                    '(:mallet-preset :base
                      (:extends :none)
                      (:enable :line-length :max 80)))
                   (config:parse-preset-definition
                    '(:mallet-preset :wide
                      (:extends :base)
                      (:enable :line-length :max 120)))))
           (registry (config:build-preset-registry defns))
           (cfg (config:resolve-preset :wide registry)))
      (let ((rule (find :line-length (config:config-rules cfg) :key #'rules:rule-name)))
        (ok (not (null rule)))
        (ok (= 120 (rules:line-length-rule-max-length rule))
            "Child preset :max 120 should override parent :max 80")))))

(deftest disable-specs-merge-through-chain
  (testing "Disable-specs accumulate through preset chain"
    (let* ((defns (list
                   (config:parse-preset-definition
                    '(:mallet-preset :base
                      (:extends :default)
                      (:disable :missing-else)))
                   (config:parse-preset-definition
                    '(:mallet-preset :stricter
                      (:extends :base)
                      (:disable :unused-variables)))))
           (registry (config:build-preset-registry defns))
           (cfg (config:resolve-preset :stricter registry)))
      ;; Both disables should be present
      (ok (member :missing-else (config:config-disabled-rules cfg))
          ":missing-else disabled by :base should propagate to :stricter")
      (ok (member :unused-variables (config:config-disabled-rules cfg))
          ":unused-variables disabled by :stricter should be present")
      ;; Rules that are NOT disabled should still be present
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :trailing-whitespace rule-names)
            ":trailing-whitespace should survive through the chain"))))

  (testing "Child preset can re-enable a rule disabled by parent"
    (let* ((defns (list
                   (config:parse-preset-definition
                    '(:mallet-preset :parent
                      (:extends :default)
                      (:disable :unused-variables)))
                   (config:parse-preset-definition
                    '(:mallet-preset :child
                      (:extends :parent)
                      (:enable :unused-variables)))))
           (registry (config:build-preset-registry defns))
           (cfg (config:resolve-preset :child registry)))
      ;; :unused-variables should be re-enabled by child
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :unused-variables rule-names)
            ":unused-variables should be re-enabled by child preset"))
      (ok (not (member :unused-variables (config:config-disabled-rules cfg)))
          ":unused-variables should NOT be in disabled list"))))

(deftest shadowing-note-for-none-preset
  (testing "Shadowing built-in :none emits note to *error-output*"
    (let* ((defns (list
                   (config:parse-preset-definition
                    '(:mallet-preset :none
                      (:enable :trailing-whitespace)))))
           (registry (config:build-preset-registry defns))
           (output (with-output-to-string (s)
                     (let ((*error-output* s))
                       (config:resolve-preset :none registry)))))
      (ok (search "shadowing built-in" (string-downcase output))
          "Shadowing :none should emit a note")
      (ok (search "none" (string-downcase output))
          "Shadowing note should mention 'none'"))))

(deftest circular-reference-self-and-three-node
  (testing "Self-referencing preset signals circular-preset-reference"
    (let* ((defns (list
                   (config:parse-preset-definition
                    '(:mallet-preset :self-ref (:extends :self-ref)))))
           (registry (config:build-preset-registry defns)))
      (ok (handler-case
              (progn (config:resolve-preset :self-ref registry) nil)
            (errors:circular-preset-reference () t))
          "Self-referencing preset should signal circular-preset-reference")))

  (testing "Three-node circular chain: a -> b -> c -> a"
    (let* ((defns (list
                   (config:parse-preset-definition '(:mallet-preset :a (:extends :b)))
                   (config:parse-preset-definition '(:mallet-preset :b (:extends :c)))
                   (config:parse-preset-definition '(:mallet-preset :c (:extends :a)))))
           (registry (config:build-preset-registry defns)))
      (ok (handler-case
              (progn (config:resolve-preset :a registry) nil)
            (errors:circular-preset-reference (c)
              (let ((chain (errors:circular-preset-reference-chain c)))
                ;; Chain should include all three nodes
                (and (member :a chain)
                     (member :b chain)
                     (member :c chain)))))
          "Three-node cycle should include all three names in chain")))

  (testing "Non-circular chain resolves correctly"
    (let* ((defns (list
                   (config:parse-preset-definition
                    '(:mallet-preset :a (:extends :b)))
                   (config:parse-preset-definition
                    '(:mallet-preset :b (:extends :default)))))
           (registry (config:build-preset-registry defns))
           (cfg (config:resolve-preset :a registry)))
      (ok (typep cfg 'config:config)
          "Non-circular chain should resolve successfully")
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :trailing-whitespace rule-names)
            "Should inherit from :default through chain")))))

;;; Preset registry wiring tests (Task 4)

(deftest parse-config-with-registry
  (testing "parse-config with registry resolves user-defined preset via :extends"
    (let* ((preset-sexp '(:mallet-preset :strict (:enable :line-length :max 80)))
           (registry (config:build-preset-registry
                      (list (config:parse-preset-definition preset-sexp))))
           (sexp '(:mallet-config (:extends :strict)))
           (cfg (config:parse-config sexp :preset-registry registry)))
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :line-length rule-names)
            "Should inherit :line-length from user-defined :strict preset"))))

  (testing "parse-config without registry falls back to built-in for :extends"
    (let* ((sexp '(:mallet-config (:extends :none) (:enable :trailing-whitespace)))
           (cfg (config:parse-config sexp)))
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :trailing-whitespace rule-names))
        (ok (not (member :no-tabs rule-names))
            "Only explicitly enabled rule with :none base"))))

  (testing "parse-config with registry: preset-override takes precedence over :extends"
    (let* ((preset-sexp '(:mallet-preset :strict (:enable :line-length :max 80)))
           (registry (config:build-preset-registry
                      (list (config:parse-preset-definition preset-sexp))))
           (sexp '(:mallet-config (:extends :strict)))
           (cfg (config:parse-config sexp
                                     :preset-registry registry
                                     :preset-override :none)))
      ;; :none has no rules, so line-length should NOT be inherited from :strict
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (not (member :line-length rule-names))
            "preset-override :none beats :extends :strict"))))

  (testing "parse-config with registry: user-defined preset-override is resolved via registry"
    (let* ((preset-sexp '(:mallet-preset :ci (:enable :trailing-whitespace)))
           (registry (config:build-preset-registry
                      (list (config:parse-preset-definition preset-sexp))))
           (sexp '(:mallet-config))
           (cfg (config:parse-config sexp
                                     :preset-registry registry
                                     :preset-override :ci)))
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :trailing-whitespace rule-names)
            "User-defined :ci resolved through registry as preset-override")))))

(deftest load-config-with-presets
  (testing "load-config with multi-sexp file: preset + config"
    (with-temporary-config
        ("(:mallet-preset :strict (:enable :line-length :max 80))
          (:mallet-config (:extends :strict))" path)
      (let* ((cfg (config:load-config path))
             (rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :line-length rule-names)
            "Config inherits :line-length from user-defined :strict preset"))))

  (testing "load-config with preset-only file and :default preset is auto-resolved"
    (with-temporary-config
        ("(:mallet-preset :default (:enable :trailing-whitespace))" path)
      (let* ((cfg (config:load-config path))
             (rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :trailing-whitespace rule-names)
            "User-defined :default preset used when no :mallet-config present"))))

  (testing "load-config: preset-override overrides :extends in config"
    (with-temporary-config
        ("(:mallet-preset :strict (:enable :line-length :max 80))
          (:mallet-config (:extends :strict))" path)
      (let* ((cfg (config:load-config path :preset-override :none))
             (rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (not (member :line-length rule-names))
            "preset-override :none beats :extends :strict in config"))))

  (testing "load-config: user-defined preset-override resolved from registry"
    (with-temporary-config
        ("(:mallet-preset :ci (:enable :trailing-whitespace))
          (:mallet-config)" path)
      (let* ((cfg (config:load-config path :preset-override :ci))
             (rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :trailing-whitespace rule-names)
            "User-defined :ci used as preset-override"))))

  (testing "load-config: root-dir is set to config file directory"
    (with-temporary-config
        ("(:mallet-config (:enable :trailing-whitespace))" path)
      (let ((cfg (config:load-config path)))
        (ok (not (null (config:config-root-dir cfg)))
            "root-dir should be set after loading from file"))))

  (testing "load-config: backward compat — single-sexp .mallet.lisp still works"
    (with-temporary-config
        ("(:mallet-config (:extends :default) (:enable :line-length :max 100))" path)
      (let* ((cfg (config:load-config path))
             (rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :trailing-whitespace rule-names)
            "Should inherit :trailing-whitespace from :default")
        (ok (member :line-length rule-names)
            "Should have explicitly enabled :line-length")
        (let ((ll-rule (find :line-length (config:config-rules cfg) :key #'rules:rule-name)))
          (ok (= 100 (rules:line-length-rule-max-length ll-rule))
              ":line-length :max should be 100")))))

  (testing "load-config: preset-only file without :default uses built-in default"
    (with-temporary-config
        ("(:mallet-preset :strict (:extends :default) (:enable :line-length :max 80))" path)
      (let* ((cfg (config:load-config path))
             (rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        ;; No :mallet-config and no user-defined :default => built-in :default
        (ok (member :trailing-whitespace rule-names)
            "Should use built-in :default when no user-defined :default")
        (ok (not (member :line-length rule-names))
            ":line-length should NOT be enabled (not in built-in :default)"))))

  (testing "load-config: chained user-defined presets in file"
    (with-temporary-config
        ("(:mallet-preset :base (:extends :default) (:enable :line-length :max 80))
          (:mallet-preset :ci (:extends :base) (:enable :missing-docstring))
          (:mallet-config (:extends :ci))" path)
      (let* ((cfg (config:load-config path))
             (rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :trailing-whitespace rule-names)
            "Should inherit :trailing-whitespace from :default via chain")
        (ok (member :line-length rule-names)
            "Should inherit :line-length from :base")
        (ok (member :missing-docstring rule-names)
            "Should have :missing-docstring from :ci")
        (let ((ll-rule (find :line-length (config:config-rules cfg) :key #'rules:rule-name)))
          (ok (= 80 (rules:line-length-rule-max-length ll-rule))
              ":line-length :max should be 80 from :base")))))

  (testing "load-config: config with enables/disables on top of user-defined preset"
    (with-temporary-config
        ("(:mallet-preset :strict (:extends :default) (:enable :line-length :max 80))
          (:mallet-config (:extends :strict)
            (:disable :unused-variables)
            (:enable :line-length :max 120))" path)
      (let* ((cfg (config:load-config path))
             (rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :trailing-whitespace rule-names)
            "Should inherit :trailing-whitespace from :default via :strict")
        (ok (member :line-length rule-names)
            "Should have :line-length")
        (ok (member :unused-variables (config:config-disabled-rules cfg))
            ":unused-variables should be disabled by config")
        (let ((ll-rule (find :line-length (config:config-rules cfg) :key #'rules:rule-name)))
          (ok (= 120 (rules:line-length-rule-max-length ll-rule))
              "Config :line-length :max 120 should override preset :max 80"))))))

;;; parse-config with registry: :extends chain through user-defined -> built-in

(deftest parse-config-registry-chain-to-builtin
  (testing ":extends chain: user-defined -> user-defined -> built-in via parse-config"
    (let* ((defns (list
                   (config:parse-preset-definition
                    '(:mallet-preset :base
                      (:extends :default)
                      (:enable :line-length :max 80)))
                   (config:parse-preset-definition
                    '(:mallet-preset :ci
                      (:extends :base)
                      (:enable :missing-docstring)))))
           (registry (config:build-preset-registry defns))
           (sexp '(:mallet-config (:extends :ci) (:enable :trailing-whitespace :severity :error)))
           (cfg (config:parse-config sexp :preset-registry registry)))
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        ;; Inherited from :default (via :base -> :ci)
        (ok (member :unused-variables rule-names)
            "Should inherit :unused-variables from :default")
        ;; From :base
        (ok (member :line-length rule-names)
            "Should inherit :line-length from :base")
        ;; From :ci
        (ok (member :missing-docstring rule-names)
            "Should inherit :missing-docstring from :ci")
        ;; Explicit in config
        (ok (member :trailing-whitespace rule-names)
            "Should have :trailing-whitespace from config"))
      ;; Verify config-level severity override applied
      (let ((tw-rule (find :trailing-whitespace (config:config-rules cfg) :key #'rules:rule-name)))
        (ok (eq :error (rules:rule-severity tw-rule))
            "Config-level severity override should apply"))))

  (testing ":extends user-defined preset with preset-override overriding from CLI"
    (let* ((defns (list
                   (config:parse-preset-definition
                    '(:mallet-preset :strict
                      (:extends :default)
                      (:enable :line-length :max 80)))))
           (registry (config:build-preset-registry defns))
           ;; Config says :extends :strict, but CLI overrides to :none
           (sexp '(:mallet-config (:extends :strict) (:enable :trailing-whitespace)))
           (cfg (config:parse-config sexp
                                     :preset-registry registry
                                     :preset-override :none)))
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        ;; :none has no rules, so only explicitly enabled rules should be present
        (ok (member :trailing-whitespace rule-names)
            "Explicitly enabled rule should be present")
        (ok (not (member :line-length rule-names))
            ":line-length from :strict should NOT be present — overridden by :none")
        (ok (not (member :unused-variables rule-names))
            ":unused-variables from :default should NOT be present — overridden by :none")))))

;;; :for-paths with preset registry

(deftest for-paths-with-preset-registry
  (testing ":for-paths inherits rules from user-defined preset via :extends"
    (let* ((defns (list
                   (config:parse-preset-definition
                    '(:mallet-preset :strict
                      (:extends :default)
                      (:enable :line-length :max 80)))))
           (registry (config:build-preset-registry defns))
           (sexp '(:mallet-config
                   (:extends :strict)
                   (:for-paths ("tests/**/*.lisp")
                    (:enable :line-length :max 120)
                    (:disable :unused-variables))))
           (cfg (config:parse-config sexp :preset-registry registry)))
      ;; Base rules should include preset's rules
      (let* ((base-rules (config:get-rules-for-file cfg #P"/src/main.lisp"))
             (base-names (mapcar #'rules:rule-name base-rules)))
        (ok (member :line-length base-names)
            "Base should have :line-length from :strict")
        (ok (member :unused-variables base-names)
            "Base should have :unused-variables from :default via :strict")
        (let ((ll-rule (find :line-length base-rules :key #'rules:rule-name)))
          (ok (= 80 (rules:line-length-rule-max-length ll-rule))
              "Base :line-length should be 80 from :strict")))
      ;; Test path rules
      (let* ((test-rules (config:get-rules-for-file cfg #P"/tests/foo-test.lisp"))
             (test-names (mapcar #'rules:rule-name test-rules)))
        (ok (member :line-length test-names)
            "Tests should have :line-length")
        (ok (not (member :unused-variables test-names))
            ":unused-variables should be disabled for tests")
        (let ((ll-rule (find :line-length test-rules :key #'rules:rule-name)))
          (ok (= 120 (rules:line-length-rule-max-length ll-rule))
              "Test :line-length should be 120 from :for-paths override")))))

  (testing ":for-paths with preset registry: inherits project-wide overrides on top of preset"
    (let* ((defns (list
                   (config:parse-preset-definition
                    '(:mallet-preset :strict
                      (:extends :default)
                      (:enable :line-length :max 80)))))
           (registry (config:build-preset-registry defns))
           (sexp '(:mallet-config
                   (:extends :strict)
                   (:disable :missing-else)
                   (:for-paths ("tests/**/*.lisp")
                    (:enable :line-length :max 120))))
           (cfg (config:parse-config sexp :preset-registry registry)))
      ;; :for-paths should inherit project-wide :disable :missing-else
      (let* ((test-rules (config:get-rules-for-file cfg #P"/tests/foo-test.lisp"))
             (test-names (mapcar #'rules:rule-name test-rules)))
        (ok (not (member :missing-else test-names))
            ":missing-else should be disabled in :for-paths via project-wide :disable")
        (ok (member :line-length test-names)
            ":line-length should be present from :for-paths :enable")))))

;;; String-path :extends rejection

(deftest string-extends-rejected
  (testing ":extends with string value signals an error"
    ;; String paths for :extends are removed — only keywords (preset names) are valid
    (ok (signals (config:parse-config '(:mallet-config (:extends "./other-config.lisp")))
                 'error)
        "String :extends should signal an error (no longer supported)"))

  (testing ":extends with string value fails even with preset-registry"
    (let ((registry (config:build-preset-registry '())))
      (ok (signals (config:parse-config '(:mallet-config (:extends "strict"))
                                         :preset-registry registry)
                   'error)
          "String :extends should fail even when preset-registry is provided"))))
