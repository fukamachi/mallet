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
           #:*suppression-state*
           #:extract-lisp-bodies-from-coalton))
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

   Returns a hash table mapping rule keywords to lists of (start . end) conses."
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

   INTERVALS is the hash table built by BUILD-DISABLE-INTERVALS, mapping rule keywords
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
   :disable directives are applied to the region only (NOT registered for stale tracking).
   :enable directives are applied immediately.

   TODO: Stale detection for form-level :disable regions is not yet implemented.
   Unlike :suppress directives, :disable regions bypass register-suppression and are
   never checked for staleness. When stale detection for disable regions is added,
   each :disable directive should call register-suppression and track whether any
   violation was suppressed by the region.

   KNOWN LIMITATION (U-4): A trailing ; mallet:suppress comment on the LAST line of a
   multi-line form is not applied to that form. Because this function only consumes
   directives with D.line <= FORM-START-LINE, a directive on e.g. line 9 of a form
   spanning lines 5-9 is not consumed here (9 > 5). After the form, prev-end-line
   becomes 9, and on the next iteration the directive passes the discard check and is
   consumed for the NEXT form instead. Trailing ; mallet:suppress is therefore reliable
   only for single-line forms."
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


(defun extract-lisp-bodies-from-coalton (expr position-map fallback-line fallback-column)
  "Walk EXPR (a coalton-toplevel form tree) and collect CL body expressions
from all (lisp Type (vars) body...) forms at any nesting depth.

Returns a flat list of (body-expr line column) entries.
Non-cons body expressions (atoms) are skipped — CL form rules only check list
expressions."
  (let ((results '()))
    (labels ((extract-symbol-name (head)
               ;; Require a colon to distinguish symbol strings ("PKG:NAME")
               ;; from string literals ("lisp") — mirrors symbol-matches-p in base.lisp.
               (typecase head
                 (string (when (find #\: head)
                           (rules:symbol-name-from-string head)))
                 (symbol (symbol-name head))
                 (otherwise nil)))
             (walk (e)
               (when (consp e)
                 (let ((head-name (extract-symbol-name (first e))))
                   (when (and head-name (string-equal head-name "LISP"))
                     ;; (lisp Type (vars) body...)
                     ;; body starts after Type and (vars) — i.e., cdddr
                     (dolist (body-expr (cdddr e))
                       (when (consp body-expr)
                         (multiple-value-bind (line col)
                             (parser:find-position body-expr position-map
                                                   fallback-line fallback-column)
                           (push (list body-expr line col) results)))))
                   ;; Walk all elements (not just rest) so that cons heads such
                   ;; as let-binding lists are fully traversed.
                   (dolist (sub e)
                     (when (consp sub)
                       (walk sub)))))))
      (walk expr))
    (nreverse results)))

; mallet:suppress cyclomatic-complexity comment-ratio
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
            (let ((form-start-line (parser:form-line form))
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

                   ;; Dispatch (lisp Type (vars) body...) bodies to CL rules.
                   ;; Coalton rules skip synthetic forms (coalton-form-p returns NIL);
                   ;; CL rules process them normally via the existing :around methods.
                   (when (rules:coalton-form-p form)
                     (let* ((position-map (parser:form-position-map form))
                            (lisp-bodies
                              (extract-lisp-bodies-from-coalton
                                form-expr
                                position-map
                                form-start-line
                                (parser:form-column form))))
                       (dolist (entry lisp-bodies)
                         (destructuring-bind (body-expr line column) entry
                           (let ((synthetic
                                   (make-instance 'parser:form
                                                  :expr body-expr
                                                  :file (parser:form-file form)
                                                  :line line
                                                  :column column
                                                  :end-line line
                                                  :end-column 0
                                                  :source nil
                                                  :position-map position-map)))
                             (setf form-violations
                                   (nconc form-violations
                                          (process-single-form
                                            synthetic rules file file-type nil))))))))

                   ;; Filter violations against pending :suppress records
                   ;; (includes both comment :suppress and declaim suppress-next)
                   (when pending-suppress-records
                     (multiple-value-setq (form-violations stale-suppress-entries)
                       (filter-suppress-violations form-violations pending-suppress-records
                                                   *suppression-state* stale-suppress-entries))
                     (setf pending-suppress-records nil))

                   (setf violations (nconc violations form-violations)))))

              (setf prev-end-line (parser:form-end-line form))))

          ;; U-2: Flush any remaining pending-suppress-records and unconsumed
          ;; pending-directives as stale.  Two cases arise when a ; mallet:suppress
          ;; comment appears after the last top-level form:
          ;;
          ;;  (a) The directive was consumed into pending-suppress-records by an earlier
          ;;      consume-comment-directives call but no subsequent real form ever ran
          ;;      filter-suppress-violations for it.
          ;;
          ;;  (b) The directive was never consumed at all because its line number
          ;;      exceeds the start line of every form (e.g. it follows the last form).
          ;;      These are still sitting in pending-directives.

          ;; Case (a): records already registered but never filtered
          ;; The car is nil (no registered ID) because these entries were never
          ;; matched against a real form.  generate-stale-suppression-violations
          ;; only reads (cdr entry), so nil does not overlap with any live ID.
          (dolist (record pending-suppress-records)
            (destructuring-bind (id d-line d-rules d-reason) record
              (declare (ignore id))
              (push (cons nil (list :line d-line :rules d-rules :reason d-reason))
                    stale-suppress-entries)))

          ;; Case (b): :suppress directives that were never consumed (after last form)
          ;; Same nil-car convention: no ID exists for directives that were never
          ;; consumed; generate-stale-suppression-violations ignores the car.
          (dolist (directive pending-directives)
            (destructuring-bind (d-line d-type d-rules d-reason) directive
              ;; NOTE: Only :suppress is flagged here. :disable directives in
              ;; pending-directives at EOF may have been processed by the text/token
              ;; pipeline (filter-text-token-violations), which registers them in
              ;; text-token-suppression-state. Flagging them here would double-count.
              ;; Text/token-level stale :disable regions are reported via
              ;; collect-stale-suppressions on text-token-suppression-state below.
              ;; Form-level :disable stale detection is a known TODO (see docstring).
              (when (eq d-type :suppress)
                (push (cons nil (list :line d-line :rules d-rules :reason d-reason))
                      stale-suppress-entries))))

          ;; After all forms: generate stale-suppression violations.
          ;; U-1: also collect stale entries from text-token-suppression-state so that
          ;; :disable regions for text/token rules that suppressed zero violations are reported.
          (let ((all-stale (append stale-suppress-entries
                                   (suppression:collect-stale-suppressions *suppression-state*)
                                   (suppression:collect-stale-suppressions
                                     text-token-suppression-state))))
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
