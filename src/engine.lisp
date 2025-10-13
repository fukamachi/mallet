(defpackage #:mallet/engine
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:parser #:mallet/parser)
   (#:rules #:mallet/rules)
   (#:config #:mallet/config)
   (#:violation #:mallet/violation)
   (#:suppression #:mallet/suppression))
  (:export #:lint-file
           #:lint-files))
(in-package #:mallet/engine)

(defun lint-file (file &key config)
  "Lint a single FILE using CONFIG.
Returns (values violations ignored-p).
If ignored-p is T, the file was ignored and violations will be NIL."
  (check-type file pathname)

  (unless (probe-file file)
    (error "File not found: ~A" file))

  ;; Load or use provided config
  (let ((config-file (when (not config)
                       (config:find-config-file (uiop:pathname-directory-pathname file)))))
    ;; Check if explicitly provided config ignores this file
    (when (and config (config:file-ignored-p config file))
      (return-from lint-file (values nil t)))

    (setf config
          (or config
              (if config-file
                  (let ((loaded-config (config:load-config config-file)))
                    ;; Check if file should be ignored
                    (when (config:file-ignored-p loaded-config file)
                      (return-from lint-file (values nil t)))
                    loaded-config)
                  (config:get-built-in-config))))

    (check-type config config:config))

  ;; Get rules for this file
  (let* ((rules (config:get-rules-for-file config file))
         (text (uiop:read-file-string file))
         (violations '())
         ;; Extract file extension and convert to keyword (e.g., "lisp" -> :LISP, "asd" -> :ASD)
         (file-type (let ((type-string (pathname-type file)))
                      (when type-string
                        (intern (string-upcase type-string) :keyword)))))

    ;; Run text-level rules
    (dolist (rule rules)
      (when (and (eq (rules:rule-type rule) :text)
                 ;; Only run rule if file has extension AND it matches rule's file-types
                 ;; Files without extensions (LICENSE, README, etc.) are skipped
                 (and file-type
                      (member file-type (rules:rule-file-types rule))))
        (setf violations
              (nconc violations (rules:check-text rule text file)))))

    ;; Run form-level rules
    (when (some (lambda (r) (eq (rules:rule-type r) :form)) rules)
      ;; Ensure mallet package exists for reading #+mallet declarations
      (suppression:ensure-mallet-package-exists)

      (multiple-value-bind (forms parse-errors)
          ;; Add :mallet to *features* so #+mallet declarations are read
          (let ((*features* (cons :mallet *features*)))
            (parser:parse-forms text file))

        ;; Convert parse errors to violations (always reported - not configurable)
        ;; Parse errors are :error severity since they prevent compilation
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

        ;; Create suppression state for this file
        (let ((suppression-state (suppression:make-suppression-state))
              (pending-next-form-suppression nil))

          ;; Process forms and check rules
          (dolist (form forms)
            (let ((form-expr (parser:form-expr form)))
              (cond
                ;; Handle mallet suppress-next declaration
                ((and (suppression:mallet-declaim-p form-expr)
                      (suppression:mallet-suppress-next-p form-expr))
                 ;; Accumulate rules to suppress for next form
                 (let ((next-rules (suppression:extract-suppress-next-rules form-expr)))
                   (setf pending-next-form-suppression
                         (if pending-next-form-suppression
                             (union pending-next-form-suppression next-rules :test #'eq)
                             next-rules))))

                ;; Handle other mallet declarations (disable/enable/suppress-function)
                ((suppression:mallet-declaim-p form-expr)
                 (suppression:update-suppression-for-declaim form-expr suppression-state))

                ;; Regular form - check with rules
                (t
                 ;; Apply pending suppress-next if we have one
                 (unwind-protect
                     (progn
                       (when pending-next-form-suppression
                         (suppression:push-scope-suppression suppression-state
                                                            pending-next-form-suppression))
                       ;; Check form with all rules
                       (dolist (rule rules)
                         (when (and (eq (rules:rule-type rule) :form)
                                    ;; Only run rule if file has extension AND it matches rule's file-types
                                    (and file-type
                                         (member file-type (rules:rule-file-types rule)))
                                    ;; Check if rule is not suppressed
                                    (not (suppression:rule-suppressed-p
                                          suppression-state
                                          (rules:rule-name rule))))
                           (setf violations
                                 (nconc violations
                                        (rules:check-form rule form file))))))
                   ;; AUTOMATIC CLEANUP: Pop scope and clear pending
                   (when pending-next-form-suppression
                     (suppression:pop-scope-suppression suppression-state)
                     (setf pending-next-form-suppression nil)))))))))) ; close cond, let, dolist, let, multiple-value-bind, when

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