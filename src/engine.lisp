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

(defun build-disable-intervals (directives last-line)
  "Build a list of (rule-keyword start-line end-line) intervals from DIRECTIVES.

   DIRECTIVES is the sorted list returned by PARSE-COMMENT-DIRECTIVES.
   LAST-LINE is the last line number in the file (for unclosed :disable regions).

   For each :disable directive at line S with no matching :enable, the interval
   extends from S to LAST-LINE. Nested :disable/:enable for the same rule are
   handled by tracking the nesting depth.

   Returns an alist mapping rule keywords to lists of (start . end) conses."
  (let ((open-starts (make-hash-table :test 'eq))   ; rule -> list of start lines
        (intervals (make-hash-table :test 'eq)))     ; rule -> list of (start . end)
    (dolist (directive directives)
      (destructuring-bind (d-line d-type d-rules d-reason) directive
        (declare (ignore d-reason))
        (when (member d-type '(:disable :enable))
          (dolist (rule d-rules)
            (ecase d-type
              (:disable
               (push d-line (gethash rule open-starts nil)))
              (:enable
               (let ((starts (gethash rule open-starts nil)))
                 (when starts
                   (push (cons (first starts) (1- d-line))
                         (gethash rule intervals nil))
                   (setf (gethash rule open-starts) (rest starts))))))))))
    ;; Close any unclosed :disable regions — they extend to EOF
    (maphash (lambda (rule starts)
               (dolist (start starts)
                 (push (cons start last-line)
                       (gethash rule intervals nil))))
             open-starts)
    intervals))

(defun violation-in-disabled-interval-p (violation intervals)
  "Return T if VIOLATION's line falls within a disabled interval for its rule.

   INTERVALS is the alist built by BUILD-DISABLE-INTERVALS, mapping rule keywords
   to lists of (start . end) conses."
  (let* ((rule (violation:violation-rule violation))
         (line (violation:violation-line violation))
         (rule-intervals (gethash rule intervals nil)))
    (or (some (lambda (interval)
                (and (>= line (car interval))
                     (<= line (cdr interval))))
              rule-intervals)
        ;; Also check :all-rules intervals
        (let ((all-intervals (gethash :all intervals nil)))
          (some (lambda (interval)
                  (and (>= line (car interval))
                       (<= line (cdr interval))))
                all-intervals)))))

(defun filter-text-token-violations (violations directives state last-line)
  "Filter text/token VIOLATIONS using comment :disable/:enable DIRECTIVES.

   Violations whose line falls within a disabled interval for their rule are removed.
   For each suppressed violation, MARK-SUPPRESSION-USED is called on the corresponding
   suppression ID in STATE.

   :suppress directives are NOT applied to text/token violations.

   Returns the filtered violations list."
  (when (null directives)
    (return-from filter-text-token-violations violations))
  (let ((intervals (build-disable-intervals directives last-line))
        ;; Map each :disable directive to a suppression ID for stale tracking
        (directive-ids (make-hash-table :test 'equal)))  ; (rule . line) -> id
    ;; Register each :disable directive in state for stale tracking
    (dolist (directive directives)
      (destructuring-bind (d-line d-type d-rules d-reason) directive
        (when (eq d-type :disable)
          (dolist (rule d-rules)
            (let ((id (suppression:register-suppression
                        state d-line (list rule) d-reason :text-token-disable)))
              (setf (gethash (cons rule d-line) directive-ids) id))))))
    ;; Filter violations
    (let ((kept nil)
          (used-rule-lines (make-hash-table :test 'equal)))  ; (rule . line) -> t
      (dolist (v violations)
        (if (violation-in-disabled-interval-p v intervals)
            ;; Find the :disable directive responsible and mark it used
            (let* ((rule (violation:violation-rule v))
                   (vline (violation:violation-line v))
                   ;; Find the most-recently-opened interval containing vline
                   (rule-intervals (gethash rule intervals nil))
                   (matched-interval (find-if (lambda (interval)
                                                (and (>= vline (car interval))
                                                     (<= vline (cdr interval))))
                                              rule-intervals))
                   ;; Find the :disable directive that opened this interval
                   (disable-line (when matched-interval
                                   (car matched-interval)))
                   (id (when disable-line
                         (gethash (cons rule disable-line) directive-ids))))
              (when (and id (not (gethash (cons rule disable-line) used-rule-lines)))
                (setf (gethash (cons rule disable-line) used-rule-lines) t)
                (suppression:mark-suppression-used state id)))
            ;; Not suppressed — keep
            (push v kept)))
      (nreverse kept))))

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
                        (intern (string-upcase type-string) :keyword))))
         ;; Parse comment directives once — used by both text/token and form processing
         (all-directives (suppression:parse-comment-directives text))
         ;; Count lines for unclosed :disable interval handling
         (last-line (let ((n 0))
                      (with-input-from-string (s text)
                        (loop for line = (read-line s nil nil)
                              while line do (incf n)))
                      (max n 1)))
         ;; Suppression state for text/token violation tracking (stale detection)
         (text-token-suppression-state (suppression:make-suppression-state)))

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

    ;; Filter text/token violations by comment :disable/:enable directives
    (setf violations
          (filter-text-token-violations violations all-directives
                                         text-token-suppression-state last-line))

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
              ;; Comment directive integration — use the already-parsed directives
              (pending-directives all-directives)
              ;; :suppress records: list of (id line rules reason). Not in registered-suppressions.
              ;; Used for both comment :suppress and declaim suppress-next directives.
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
                 (let* ((next-rules (suppression:extract-suppress-next-rules form-expr))
                        (id (suppression:register-suppression
                              *suppression-state* form-start-line next-rules nil :declaim)))
                   ;; Remove from registered-suppressions: stale tracking is done via
                   ;; pending-suppress-records + filter-suppress-violations, not rule-suppressed-p.
                   (setf (suppression:registered-suppressions *suppression-state*)
                         (delete id (suppression:registered-suppressions *suppression-state*)
                                 :key #'car))
                   ;; Add to pending records for post-form violation filtering
                   (push (list id form-start-line next-rules nil) pending-suppress-records)))

                ((suppression:mallet-declaim-p form-expr)
                 (suppression:update-suppression-for-declaim form-expr *suppression-state*))

                (t
                 ;; Run form, collecting violations
                 (let ((form-violations (process-single-form form rules file file-type nil)))

                   ;; Filter violations against pending :suppress records
                   ;; (includes both comment :suppress and declaim suppress-next)
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
