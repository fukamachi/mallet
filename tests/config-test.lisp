(defpackage #:mallet/tests/config
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:config #:mallet/config)
   (#:rules #:mallet/rules)))
(in-package #:mallet/tests/config)

;;; Config data structure tests

(deftest config-creation
  (testing "Create empty config"
    (let ((cfg (config:make-config)))
      (ok (not (null cfg)))
      (ok (typep cfg 'config:config))))

  (testing "Create config with rules"
    (let ((cfg (config:make-config
                :rules (list (rules:make-rule :line-length :max-length 100)
                             (rules:make-rule :if-without-else))
                :disabled-rules '(:if-without-else))))
      (ok (not (null cfg)))
      (ok (= 2 (length (config:config-rules cfg))))
      (ok (member :if-without-else (config:config-disabled-rules cfg))))))

;;; Config parsing tests

(deftest parse-config
  (testing "Parse simple config with :enable"
    (let* ((sexp '(:mallet-config
                   (:enable :line-length :max-length 120)))
           (cfg (config:parse-config sexp)))
      (ok (= 1 (length (config:config-rules cfg))))
      (let ((rule (first (config:config-rules cfg))))
        (ok (eq :line-length (rules:rule-name rule)))
        (ok (= 120 (rules:line-length-rule-max-length rule))))))

  (testing "Parse config with :disable"
    (let* ((sexp '(:mallet-config
                   (:disable :if-without-else)))
           (cfg (config:parse-config sexp)))
      (ok (member :if-without-else (config:config-disabled-rules cfg)))))

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
                              (:enable :line-length :max-length 100))"))
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
        (ok (member :if-without-else rule-names)))
      ;; Check that some rules are disabled
      (let ((disabled (config:config-disabled-rules cfg)))
        (ok (member :line-length disabled))
        (ok (member :constant-naming disabled))
        (ok (member :special-variable-naming disabled)))))

  (testing "Load all config"
    (let ((cfg (config:get-built-in-config :all)))
      (ok (not (null cfg)))
      ;; All rules should be enabled (none in disabled list)
      (ok (null (config:config-disabled-rules cfg)))
      ;; Check that various rules are present
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :line-length rule-names))
        (ok (member :trailing-whitespace rule-names))
        (ok (member :if-without-else rule-names))))))

;;; Config extends tests

(deftest config-extends
  (testing "Parse config with extends"
    (let* ((sexp '(:mallet-config
                   (:extends :all)
                   (:enable :line-length :max-length 100)))
           (cfg (config:parse-config sexp)))
      ;; Should inherit all rules from :all
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :line-length rule-names))
        (ok (member :trailing-whitespace rule-names)))
      ;; Check that line-length has the overridden max-length
      (let ((line-length-rule (find :line-length (config:config-rules cfg)
                                    :key #'rules:rule-name)))
        (ok (= 100 (rules:line-length-rule-max-length line-length-rule)))))))

(deftest parse-new-syntax-enable
  (testing "Parse config with :enable syntax"
    (let* ((sexp '(:mallet-config
                   (:enable :line-length :max-length 120)))
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
                   (:enable :line-length :max-length 100)
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
                    (:enable :line-length :max-length 120)
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
                   (:enable :line-length :max-length 80)
                   (:enable :unused-variables)
                   (:for-paths ("tests/**/*.lisp")
                    (:enable :line-length :max-length 120)
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
                   (:enable :line-length :max-length 80)
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
                   (:enable :line-length :max-length 80)
                   (:for-paths ("/tests/")
                    (:enable :line-length :max-length 120)
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
                   (:enable :line-length :max-length 80)
                   (:enable :trailing-whitespace)
                   (:for-paths ("/scripts/")
                    (:enable :line-length :max-length 100)
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
                   (:enable :line-length :max-length 80)
                   (:for-paths ("/tests/fixtures/")
                    (:enable :line-length :max-length 200))))
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
                   (:enable :line-length :max-length 80)
                   (:for-paths ("*.asd")
                    (:enable :line-length :max-length 100))))
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
                   (:enable :line-length :max-length 100)
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
                   (:enable :line-length :max-length 120)
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
                              (:enable :line-length :max-length 100)
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
                    (:enable :line-length :max-length 120)
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
