(defpackage #:malvolio/tests/config
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:config #:malvolio/config)))
(in-package #:malvolio/tests/config)

;;; Config data structure tests

(deftest config-creation
  (testing "Create empty config"
    (let ((cfg (config:make-config)))
      (ok (not (null cfg)))
      (ok (typep cfg 'config:config))))

  (testing "Create config with options"
    (let ((cfg (config:make-config
                :rules '((:line-length :enabled t :max-length 100)
                         (:if-without-else :enabled nil)))))
      (ok (not (null cfg)))
      (ok (config:rule-enabled-p cfg :line-length))
      (ok (not (config:rule-enabled-p cfg :if-without-else)))
      (ok (= 100 (config:get-rule-option cfg :line-length :max-length))))))

;;; Config parsing tests

(deftest parse-config
  (testing "Parse simple config"
    (let* ((sexp '(:malvolio-config
                   (:rules
                    (:line-length :enabled t :max-length 120))))
           (cfg (config:parse-config sexp)))
      (ok (config:rule-enabled-p cfg :line-length))
      (ok (= 120 (config:get-rule-option cfg :line-length :max-length)))))

  (testing "Parse config with disabled rule"
    (let* ((sexp '(:malvolio-config
                   (:rules
                    (:if-without-else :enabled nil))))
           (cfg (config:parse-config sexp)))
      (ok (not (config:rule-enabled-p cfg :if-without-else)))))

  (testing "Parse config with severity override"
    (let* ((sexp '(:malvolio-config
                   (:rules
                    (:unused-variables :severity :error))))
           (cfg (config:parse-config sexp)))
      (ok (eq :error (config:get-rule-option cfg :unused-variables :severity))))))

;;; Config file loading tests

(deftest load-config-file
  (testing "Load config from file"
    (let* ((tmpfile (format nil "/tmp/malvolio-test-~A.lisp" (get-universal-time)))
           (config-content "(:malvolio-config
                             (:rules
                              (:line-length :enabled t :max-length 100)))"))
      (unwind-protect
          (progn
            (with-open-file (out tmpfile :direction :output :if-exists :supersede)
              (write-string config-content out))
            (let ((cfg (config:load-config tmpfile)))
              (ok (config:rule-enabled-p cfg :line-length))
              (ok (= 100 (config:get-rule-option cfg :line-length :max-length)))))
        ;; Cleanup
        (when (probe-file tmpfile)
          (delete-file tmpfile))))))

;;; Config merging tests

(deftest merge-configs
  (testing "Merge two configs"
    (let ((base (config:make-config
                 :rules '((:line-length :enabled t :max-length 80)
                          (:if-without-else :enabled t))))
          (override (config:make-config
                    :rules '((:line-length :max-length 120)
                             (:unused-variables :enabled t)))))
      (let ((merged (config:merge-configs base override)))
        ;; line-length should have overridden max-length
        (ok (= 120 (config:get-rule-option merged :line-length :max-length)))
        ;; if-without-else should still be enabled from base
        (ok (config:rule-enabled-p merged :if-without-else))
        ;; unused-variables should be added from override
        (ok (config:rule-enabled-p merged :unused-variables))))))

;;; Built-in config tests

(deftest built-in-configs
  (testing "Load recommended config"
    (let ((cfg (config:get-built-in-config :recommended)))
      (ok (not (null cfg)))
      ;; All rules should be enabled by default
      (ok (config:rule-enabled-p cfg :line-length))
      (ok (config:rule-enabled-p cfg :comment-level))
      (ok (config:rule-enabled-p cfg :if-without-else))))

  (testing "Load minimal config"
    (let ((cfg (config:get-built-in-config :minimal)))
      (ok (not (null cfg)))
      ;; Only error-severity rules should be enabled
      (ok (config:rule-enabled-p cfg :wrong-otherwise))
      ;; Warning rules should be disabled
      (ok (not (config:rule-enabled-p cfg :line-length)))))

  (testing "Load strict config"
    (let ((cfg (config:get-built-in-config :strict)))
      (ok (not (null cfg)))
      ;; All rules enabled
      (ok (config:rule-enabled-p cfg :line-length))
      ;; All severities should be :error
      (ok (eq :error (config:get-rule-option cfg :line-length :severity))))))

;;; Config extends tests

(deftest config-extends
  (testing "Parse config with extends"
    (let* ((sexp '(:malvolio-config
                   (:extends :recommended)
                   (:rules
                    (:line-length :max-length 100))))
           (cfg (config:parse-config sexp)))
      ;; Should inherit all rules from recommended
      (ok (config:rule-enabled-p cfg :line-length))
      (ok (config:rule-enabled-p cfg :comment-level))
      ;; But override specific options
      (ok (= 100 (config:get-rule-option cfg :line-length :max-length))))))

;;; Config discovery tests

(deftest find-config-file
  (testing "Find config in current directory"
    (let* ((tmpdir (format nil "/tmp/malvolio-test-dir-~A" (get-universal-time)))
           (config-file (merge-pathnames ".malvolio.lisp" tmpdir)))
      (unwind-protect
          (progn
            (ensure-directories-exist tmpdir)
            (with-open-file (out config-file :direction :output :if-exists :supersede)
              (write-string "(:malvolio-config)" out))
            (let ((found (config:find-config-file tmpdir)))
              (ok (not (null found)))
              (ok (equal (namestring found) (namestring config-file)))))
        ;; Cleanup
        (when (probe-file config-file)
          (delete-file config-file))
        (when (probe-file tmpdir)
          (uiop:delete-directory-tree tmpdir :validate t)))))

  (testing "Find config in parent directory"
    (let* ((tmpdir (format nil "/tmp/malvolio-test-parent-~A" (get-universal-time)))
           (subdir (merge-pathnames "sub/" tmpdir))
           (config-file (merge-pathnames ".malvolio.lisp" tmpdir)))
      (unwind-protect
          (progn
            (ensure-directories-exist subdir)
            (with-open-file (out config-file :direction :output :if-exists :supersede)
              (write-string "(:malvolio-config)" out))
            ;; Search from subdir should find config in parent
            (let ((found (config:find-config-file subdir)))
              (ok (not (null found)))
              (ok (equal (namestring found) (namestring config-file)))))
        ;; Cleanup
        (when (probe-file config-file)
          (delete-file config-file))
        (when (probe-file tmpdir)
          (uiop:delete-directory-tree tmpdir :validate t :if-does-not-exist :ignore))))))
