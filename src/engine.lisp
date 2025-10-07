(defpackage #:malvolio/engine
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:parser #:malvolio/parser)
   (#:rules #:malvolio/rules)
   (#:violation #:malvolio/violation))
  (:export #:lint-file
           #:lint-files
           #:make-default-registry))
(in-package #:malvolio/engine)

(defun make-default-registry ()
  "Create a registry with all default rules enabled."
  (let ((registry (rules:make-registry)))
    ;; Text-level rules
    (rules:register-rule registry :line-length
                        :description "Lines should not exceed maximum length"
                        :severity :warning
                        :type :text
                        :enabled t)

    ;; Token-level rules
    (rules:register-rule registry :comment-level
                        :description "Comments should use appropriate semicolon count"
                        :severity :warning
                        :type :token
                        :enabled t)

    ;; Form-level rules
    (rules:register-rule registry :if-without-else
                        :description "Use 'when' or 'unless' instead of 'if' without else"
                        :severity :warning
                        :type :form
                        :enabled t)

    (rules:register-rule registry :bare-progn-in-if
                        :description "Use 'cond' instead of 'if' with bare 'progn'"
                        :severity :warning
                        :type :form
                        :enabled t)

    (rules:register-rule registry :missing-otherwise
                        :description "case/typecase should have 'otherwise' clause"
                        :severity :warning
                        :type :form
                        :enabled t)

    (rules:register-rule registry :wrong-otherwise
                        :description "ecase/etypecase should not have 'otherwise' or 't'"
                        :severity :error
                        :type :form
                        :enabled t)

    registry))

(defun lint-file (file &key (registry (make-default-registry)))
  "Lint a single FILE using REGISTRY rules.
Returns a list of VIOLATION objects."
  (check-type file pathname)
  (check-type registry rules:registry)

  (unless (probe-file file)
    (error "File not found: ~A" file))

  (let ((text (uiop:read-file-string file))
        (violations '()))

    ;; Run text-level rules
    (dolist (rule (rules:list-rules registry))
      (when (and (rules:rule-enabled-p rule)
                 (eq (rules:rule-type rule) :text))
        (let ((rule-impl (make-instance 'rules:line-length-rule)))
          (setf violations
                (nconc violations (rules:check-text rule-impl text file))))))

    ;; Run token-level rules
    (let ((tokens (parser:tokenize text file)))
      (dolist (rule (rules:list-rules registry))
        (when (and (rules:rule-enabled-p rule)
                   (eq (rules:rule-type rule) :token))
          (let ((rule-impl (make-instance 'rules:comment-level-rule)))
            (setf violations
                  (nconc violations (rules:check-tokens rule-impl tokens file)))))))

    ;; Run form-level rules
    (let ((forms (parser:parse-forms text file)))
      (dolist (form forms)
        (dolist (rule (rules:list-rules registry))
          (when (and (rules:rule-enabled-p rule)
                     (eq (rules:rule-type rule) :form))
            (let ((rule-impl
                   (case (rules:rule-name rule)
                     (:if-without-else (make-instance 'rules:if-without-else-rule))
                     (:bare-progn-in-if (make-instance 'rules:bare-progn-in-if-rule))
                     (:missing-otherwise (make-instance 'rules:missing-otherwise-rule))
                     (:wrong-otherwise (make-instance 'rules:wrong-otherwise-rule))
                     (t nil))))
              (when rule-impl
                (setf violations
                      (nconc violations
                             (rules:check-form rule-impl form file)))))))))

    violations))

(defun lint-files (files &key (registry (make-default-registry)))
  "Lint multiple FILES using REGISTRY rules.
Returns an alist mapping file paths to violation lists."
  (check-type files list)
  (check-type registry rules:registry)

  (loop for file in files
        for violations = (lint-file file :registry registry)
        collect (cons file violations)))
