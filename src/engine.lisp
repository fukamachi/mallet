(defpackage #:malo/engine
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:parser #:malo/parser)
   (#:rules #:malo/rules)
   (#:config #:malo/config)
   (#:violation #:malo/violation))
  (:export #:lint-file
           #:lint-files))
(in-package #:malo/engine)

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
      (multiple-value-bind (forms parse-errors)
          (parser:parse-forms text file)

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

        (dolist (form forms)
          (dolist (rule rules)
            (when (and (eq (rules:rule-type rule) :form)
                       ;; Only run rule if file has extension AND it matches rule's file-types
                       ;; Files without extensions (LICENSE, README, etc.) are skipped
                       (and file-type
                            (member file-type (rules:rule-file-types rule))))
              (setf violations
                    (nconc violations
                           (rules:check-form rule form file))))))))

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