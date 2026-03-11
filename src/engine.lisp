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

(defun consume-comment-directives (pending-directives prev-end-line form-start-line state
                                   pending-suppress-records)
  "Consume comment directives that apply to the form starting at FORM-START-LINE.

   Returns (values remaining-directives updated-suppress-records), consuming all directives D where:
   - D.line >= PREV-END-LINE (not inside the previous form)
   - D.line <= FORM-START-LINE (on or before current form starts)

   Directives with D.line < PREV-END-LINE are silently discarded (inside a previous form).

   :suppress directives are tracked in PENDING-SUPPRESS-RECORDS as (id line rules reason).
   :disable directives are registered + applied to region.
   :enable directives are applied immediately."
  ;; Discard directives that fall inside the previous form
  (loop while (and pending-directives
                   (< (first (first pending-directives)) prev-end-line))
        do (pop pending-directives))
  ;; Consume directives that apply to this form
  (loop while (and pending-directives
                   (<= (first (first pending-directives)) form-start-line))
        do (destructuring-bind (d-line d-type d-rules d-reason)
               (pop pending-directives)
             (ecase d-type
               (:suppress
                ;; Register for stale tracking, but don't add to registered-suppressions.
                ;; Violation filtering is done by the engine after the form runs.
                (let ((id (suppression:register-suppression
                            state d-line d-rules d-reason :suppress)))
                  ;; Remove immediately from registered-suppressions — we don't want
                  ;; rule-suppressed-p to pick this up and mark it used prematurely.
                  (setf (suppression:registered-suppressions state)
                        (delete id (suppression:registered-suppressions state) :key #'car))
                  (push (list id d-line d-rules d-reason) pending-suppress-records)))
               (:disable
                (suppression:set-region-disabled state d-rules))
               (:enable
                (suppression:enable-region-rules state d-rules)))))
  (values pending-directives pending-suppress-records))

(defun filter-suppress-violations (form-violations pending-suppress-records state
                                   stale-suppress-entries)
  "Filter FORM-VIOLATIONS against PENDING-SUPPRESS-RECORDS.

   Violations whose rule matches a pending suppress record are suppressed:
   - The matching suppression ID is marked as used in STATE.
   - The violation is not included in the returned list.

   Violations that don't match any pending suppress are returned as-is.

   Suppress records that matched no violation are added to STALE-SUPPRESS-ENTRIES.

   Returns (values kept-violations updated-stale-suppress-entries)."
  (let ((kept-violations nil)
        (used-suppress-ids (make-hash-table :test 'eql)))
    ;; Filter each violation
    (dolist (v form-violations)
      (let* ((rule-name (violation:violation-rule v))
             (matched-record (find-if (lambda (record)
                                        (destructuring-bind (id line rules reason) record
                                          (declare (ignore id line reason))
                                          (or (member rule-name rules :test #'eq)
                                              (member :all rules :test #'eq))))
                                      pending-suppress-records)))
        (if matched-record
            ;; Suppressed — mark used
            (setf (gethash (first matched-record) used-suppress-ids) t)
            ;; Not suppressed — keep
            (push v kept-violations))))

    ;; Mark used in state and collect stale entries
    (dolist (record pending-suppress-records)
      (destructuring-bind (id d-line d-rules d-reason) record
        (if (gethash id used-suppress-ids)
            ;; Suppression was used — mark in state for (currently unused) external tracking
            (suppression:mark-suppression-used state id)
            ;; Suppression was NOT used — it's stale
            (push (cons id (list :line d-line :rules d-rules :reason d-reason))
                  stale-suppress-entries))))

    (values (nreverse kept-violations) stale-suppress-entries)))

(defun generate-stale-suppression-violations (stale-entries rules file violations)
  "Generate :stale-suppression violations for each unused suppression entry.

   Only generates violations if the :stale-suppression rule is present in RULES.
   Generates one violation per suppressed rule in each stale entry."
  (let ((stale-rule (find :stale-suppression rules :key #'rules:rule-name)))
    (when stale-rule
      (dolist (entry stale-entries)
        (let* ((plist (cdr entry))
               (d-line (getf plist :line))
               (d-rules (getf plist :rules))
               (d-reason (getf plist :reason)))
          (dolist (rule-name d-rules)
            (push (make-instance 'violation:violation
                                 :rule :stale-suppression
                                 :file file
                                 :line d-line
                                 :column 0
                                 :severity (rules:rule-severity stale-rule)
                                 :category (rules:rule-category stale-rule)
                                 :message (rules:make-stale-suppression-message rule-name d-reason)
                                 :fix nil)
                  violations))))))
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
              (pending-next-form-suppression nil)
              ;; Comment directive integration
              (pending-directives (suppression:parse-comment-directives text))
              ;; :suppress records: list of (id line rules reason). Not in registered-suppressions.
              (pending-suppress-records nil)
              (stale-suppress-entries nil)
              (prev-end-line 0))

          (dolist (form forms)
            (let* ((form-start-line (parser:form-line form))
                   (form-expr (parser:form-expr form)))

              ;; Consume comment directives that apply to this form
              (multiple-value-setq (pending-directives pending-suppress-records)
                (consume-comment-directives pending-directives prev-end-line form-start-line
                                            *suppression-state* pending-suppress-records))

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
                 ;; Run form, collecting violations
                 (let ((form-violations nil))
                   (unwind-protect
                        (progn
                          (when pending-next-form-suppression
                            (suppression:push-scope-suppression *suppression-state*
                                                                pending-next-form-suppression))
                          (setf form-violations
                                (process-single-form form rules file file-type nil)))
                     (when pending-next-form-suppression
                       (suppression:pop-scope-suppression *suppression-state*)
                       (setf pending-next-form-suppression nil)))

                   ;; Filter violations against pending :suppress records
                   (when pending-suppress-records
                     (multiple-value-setq (form-violations stale-suppress-entries)
                       (filter-suppress-violations form-violations pending-suppress-records
                                                   *suppression-state* stale-suppress-entries))
                     (setf pending-suppress-records nil))

                   (setf violations (nconc violations form-violations)))))

              (setf prev-end-line (parser:form-end-line form))))

          ;; After all forms: generate stale-suppression violations
          (let ((all-stale (append stale-suppress-entries
                                   (suppression:collect-stale-suppressions *suppression-state*))))
            (setf violations
                  (generate-stale-suppression-violations all-stale rules file violations))))))

    ;; Generate fix metadata and populate category from rule
    (dolist (v violations)
      (let ((rule (find (violation:violation-rule v) rules :key #'rules:rule-name)))
        (when rule
          (unless (violation:violation-category v)
            (setf (violation:violation-category v) (rules:rule-category rule)))
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
