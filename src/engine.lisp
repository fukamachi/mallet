(defpackage #:mallet/engine
  (:use #:cl)
  (:local-nicknames
   (#:parser #:mallet/parser)
   (#:rules #:mallet/rules)
   (#:config #:mallet/config)
   (#:violation #:mallet/violation)
   (#:suppression #:mallet/suppression))
  (:export #:lint-file
           #:lint-files
           #:*suppression-state*))
(in-package #:mallet/engine)

;;; Special variable for dynamic suppression state

(defvar *suppression-state* nil
  "Dynamic suppression state during linting.
   Bound per-file, available to all rules during recursive checking.
   This allows rules to check if they're suppressed without explicit passing.")

(defun load-or-use-config (config file)
  "Load configuration for FILE or use provided CONFIG.
Returns (values final-config ignored-p)."
  (let ((config-file (when (not config)
                       (config:find-config-file (uiop:pathname-directory-pathname file)))))
    ;; Check if explicitly provided config ignores this file
    (when (and config (config:file-ignored-p config file))
      (return-from load-or-use-config (values nil t)))

    (values
     (or config
         (if config-file
             (let ((loaded-config (config:load-config config-file)))
               (when (config:file-ignored-p loaded-config file)
                 (return-from load-or-use-config (values nil t)))
               loaded-config)
             (config:get-built-in-config)))
     nil)))

(defun should-run-rule-on-file-p (rule file-type)
  "Check if RULE should run on file with FILE-TYPE."
  (and file-type
       (member file-type (rules:rule-file-types rule))))

(defun process-single-form (form rules file file-type violations)
  "Process a single FORM with RULES, accumulating VIOLATIONS.
Returns updated violations list."
  (dolist (rule rules)
    (when (and (eq (rules:rule-type rule) :form)
               (should-run-rule-on-file-p rule file-type))
      (setf violations
            (nconc violations (rules:check-form rule form file)))))
  violations)


(defun lint-file (file &key config)
  "Lint a single FILE using CONFIG.
Returns (values violations ignored-p).
If ignored-p is T, the file was ignored and violations will be NIL."
  (check-type file pathname)

  (unless (probe-file file)
    (error "File not found: ~A" file))

  ;; Load or use provided config
  (multiple-value-bind (final-config ignored-p)
      (load-or-use-config config file)
    (when ignored-p
      (return-from lint-file (values nil t)))
    (setf config final-config)
    (check-type config config:config))

  ;; Get rules for this file
  (let* ((rules (config:get-rules-for-file config file))
         (text (uiop:read-file-string file))
         (violations '())
         (file-type (let ((type-string (pathname-type file)))
                      (when type-string
                        (intern (string-upcase type-string) :keyword)))))

    ;; Run text-level rules
    (dolist (rule rules)
      (when (and (eq (rules:rule-type rule) :text)
                 (should-run-rule-on-file-p rule file-type))
        (setf violations
              (nconc violations (rules:check-text rule text file)))))

    ;; Run token-level rules
    (when (some (lambda (r) (eq (rules:rule-type r) :token)) rules)
      (let ((tokens (parser:tokenize text file)))
        (dolist (rule rules)
          (when (and (eq (rules:rule-type rule) :token)
                     (should-run-rule-on-file-p rule file-type))
            (setf violations
                  (nconc violations (rules:check-tokens rule tokens file)))))))

    ;; Run form-level rules
    (when (some (lambda (r) (eq (rules:rule-type r) :form)) rules)
      (suppression:ensure-mallet-package-exists)

      (multiple-value-bind (forms parse-errors)
          (let ((*features* (cons :mallet *features*)))
            (parser:parse-forms text file))

        ;; Convert parse errors to violations
        (dolist (parse-error parse-errors)
          (push (make-instance 'violation:violation
                               :rule :parse-error
                               :file (parser:parse-error-info-file parse-error)
                               :line (parser:parse-error-info-line parse-error)
                               :column (parser:parse-error-info-column parse-error)
                               :severity :error
                               :message (parser:parse-error-info-message parse-error)
                               :fix nil)
                violations))

        (let ((*suppression-state* (suppression:make-suppression-state))
              (pending-next-form-suppression nil))

          (dolist (form forms)
            (let ((form-expr (parser:form-expr form)))
              (cond
                ((and (suppression:mallet-declaim-p form-expr)
                      (suppression:mallet-suppress-next-p form-expr))
                 (let ((next-rules (suppression:extract-suppress-next-rules form-expr)))
                   (setf pending-next-form-suppression
                         (if pending-next-form-suppression
                             (union pending-next-form-suppression next-rules :test #'eq)
                             next-rules))))

                ((suppression:mallet-declaim-p form-expr)
                 (suppression:update-suppression-for-declaim form-expr *suppression-state*))

                (t
                 (unwind-protect
                      (progn
                        (when pending-next-form-suppression
                          (suppression:push-scope-suppression *suppression-state*
                                                              pending-next-form-suppression))
                        (setf violations
                              (process-single-form form rules file file-type violations)))
                   (when pending-next-form-suppression
                     (suppression:pop-scope-suppression *suppression-state*)
                     (setf pending-next-form-suppression nil))))))))))

    ;; Generate fix metadata
    (dolist (v violations)
      (let ((rule (find (violation:violation-rule v) rules :key #'rules:rule-name)))
        (when rule
          (let ((fix (rules:make-fix rule text file v)))
            (when fix
              (setf (violation:violation-fix v) fix))))))

    violations))


(defun lint-files (files &key config)
  "Lint multiple FILES using CONFIG.
Returns an alist mapping file paths to violation lists.
Ignored files are excluded from results entirely."
  (check-type files list)

  (loop for file in files
        for (violations ignored-p) = (multiple-value-list
                                      (lint-file file :config config))
        unless ignored-p
          collect (cons file violations)))
