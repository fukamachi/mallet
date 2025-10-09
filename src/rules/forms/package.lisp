(defpackage #:malvolio/rules/forms/package
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:base #:malvolio/rules/base)
   (#:violation #:malvolio/violation)
   (#:parser #:malvolio/parser))
  (:export #:unused-local-nicknames-rule
           #:unused-imported-symbols-rule))
(in-package #:malvolio/rules/forms/package)

;;; Unused Local Nicknames Rule

(defclass unused-local-nicknames-rule (base:rule)
  ()
  (:default-initargs
   :name :unused-local-nicknames
   :description "Local nicknames in :local-nicknames should be used"
   :severity :info
   :type :form))

(defmethod base:check-form ((rule unused-local-nicknames-rule) form file)
  "Check for unused local nicknames in defpackage forms."
  (let ((expr (parser:form-expr form)))
    (when (and (consp expr)
               (stringp (first expr))
               (base:symbol-matches-p (first expr) "DEFPACKAGE"))
      ;; This is a defpackage form
      ;; Re-parse file to get all forms (we need forms after this defpackage)
      (let* ((text (uiop:read-file-string file))
             (all-forms (parser:parse-forms text file))
             ;; Find defpackage by line/column since we're re-parsing
             (defpkg-line (parser:form-line form))
             (defpkg-col (parser:form-column form))
             (defpkg-pos (position-if
                          (lambda (f)
                            (and (= (parser:form-line f) defpkg-line)
                                 (= (parser:form-column f) defpkg-col)))
                          all-forms))
             (defpkg-form (when defpkg-pos (nth defpkg-pos all-forms)))
             (forms-after-defpkg (when defpkg-pos (nthcdr (1+ defpkg-pos) all-forms)))
             (in-package-pos (position-if
                              (lambda (f)
                                (let ((e (parser:form-expr f)))
                                  (and (consp e)
                                       (stringp (first e))
                                       (base:symbol-matches-p (first e) "IN-PACKAGE"))))
                              forms-after-defpkg))
             (code-forms (when in-package-pos
                          (nthcdr (1+ in-package-pos) forms-after-defpkg)))
             (position-map (when defpkg-form (parser:form-position-map defpkg-form))))

        ;; Only check if there's actual code after in-package (not a package-only file)
        (when code-forms
          (let ((nicknames (extract-local-nicknames-with-exprs expr)))
            (loop for (nickname package nick-expr) in nicknames
                  for refs = (find-nickname-references code-forms nickname)
                  when (zerop (length refs))
                  collect (multiple-value-bind (line column)
                              (if (and position-map nick-expr)
                                  (parser:find-position nick-expr position-map
                                                       (parser:form-line form)
                                                       (parser:form-column form))
                                  (values (parser:form-line form) (parser:form-column form)))
                            (make-instance 'violation:violation
                                           :rule (base:rule-name rule)
                                           :file file
                                           :line line
                                           :column column
                                           :message (format nil "Local nickname '~A' (for ~A) is unused"
                                                            nickname package)
                                           :severity (base:rule-severity rule))))))))))

;;; Unused Imported Symbols Rule

(defclass unused-imported-symbols-rule (base:rule)
  ()
  (:default-initargs
   :name :unused-imported-symbols
   :description "Imported symbols from :import-from should be used or re-exported"
   :severity :info
   :type :form))

(defmethod base:check-form ((rule unused-imported-symbols-rule) form file)
  "Check for unused imported symbols in defpackage forms."
  (let ((expr (parser:form-expr form)))
    (when (and (consp expr)
               (stringp (first expr))
               (base:symbol-matches-p (first expr) "DEFPACKAGE"))
      ;; This is a defpackage form
      ;; Re-parse file to get all forms (we need forms after this defpackage)
      (let* ((text (uiop:read-file-string file))
             (all-forms (parser:parse-forms text file))
             ;; Find defpackage by line/column since we're re-parsing
             (defpkg-line (parser:form-line form))
             (defpkg-col (parser:form-column form))
             (defpkg-pos (position-if
                          (lambda (f)
                            (and (= (parser:form-line f) defpkg-line)
                                 (= (parser:form-column f) defpkg-col)))
                          all-forms))
             (defpkg-form (when defpkg-pos (nth defpkg-pos all-forms)))
             (forms-after-defpkg (when defpkg-pos (nthcdr (1+ defpkg-pos) all-forms)))
             (in-package-pos (position-if
                              (lambda (f)
                                (let ((e (parser:form-expr f)))
                                  (and (consp e)
                                       (stringp (first e))
                                       (base:symbol-matches-p (first e) "IN-PACKAGE"))))
                              forms-after-defpkg))
             (code-forms (when in-package-pos
                          (nthcdr (1+ in-package-pos) forms-after-defpkg)))
             (position-map (when defpkg-form (parser:form-position-map defpkg-form))))

        ;; Only check if there's actual code after in-package (not a package-only file)
        (when code-forms
          (let ((imports (extract-imported-symbols-with-exprs expr))
                (exports (extract-exports expr))
                (violations '()))
            (dolist (import imports)
              (let ((pkg (first import))
                    (symbols (second import)))
                (dolist (sym-data symbols)
                  (let* ((sym (first sym-data))
                         (sym-expr (second sym-data))
                         (refs (find-symbol-references code-forms sym))
                         (exported-p (member sym exports :test #'string-equal)))
                    ;; Only report if not used AND not re-exported
                    (when (and (zerop (length refs)) (not exported-p))
                      (multiple-value-bind (line column)
                          (if (and position-map sym-expr)
                              (parser:find-position sym-expr position-map
                                                   (parser:form-line form)
                                                   (parser:form-column form))
                              (values (parser:form-line form) (parser:form-column form)))
                        (push (make-instance 'violation:violation
                                             :rule (base:rule-name rule)
                                             :file file
                                             :line line
                                             :column column
                                             :message (format nil "Imported symbol '~A' from ~A is unused"
                                                              sym pkg)
                                             :severity (base:rule-severity rule))
                              violations)))))))
            (nreverse violations)))))))

;;; Helper Functions

(defun extract-local-nicknames-with-exprs (defpackage-form)
  "Extract local nicknames from a defpackage form.
Returns list of (nickname package nick-expr) tuples where nick-expr is the actual sub-expression."
  (let ((nicknames '()))
    (dolist (clause (cddr defpackage-form))
      (when (and (consp clause)
                 (stringp (first clause))
                 (string-equal (first clause) ":local-nicknames"))
        (dolist (pair (rest clause))
          (when (and (consp pair) (= 2 (length pair)))
            ;; pair is the actual sub-expression like (#:a #:alexandria)
            (push (list (first pair) (second pair) pair) nicknames)))))
    (nreverse nicknames)))

(defun extract-imported-symbols-with-exprs (defpackage-form)
  "Extract imported symbols from a defpackage form.
Returns list of (package symbols) where symbols is a list of (symbol expr) tuples."
  (let ((imports '()))
    (dolist (clause (cddr defpackage-form))
      (when (and (consp clause)
                 (stringp (first clause))
                 (string-equal (first clause) ":import-from"))
        (let ((pkg (second clause))
              (syms (loop for sym in (cddr clause)
                          collect (list sym sym))))  ; (symbol expr) - expr is just the symbol itself
          (push (list pkg syms) imports))))
    (nreverse imports)))

(defun extract-exports (defpackage-form)
  "Extract exported symbols from defpackage form.
Returns list of symbol names."
  (let ((exports '()))
    (dolist (clause (cddr defpackage-form))
      (when (and (consp clause)
                 (stringp (first clause))
                 (string-equal (first clause) ":export"))
        (setf exports (append exports (rest clause)))))
    exports))

(defun find-nickname-references (forms nickname)
  "Find references to a nickname in forms.
Looks for 'nickname:*' patterns.
Returns list of strings where nickname was found."
  (let ((references '()))
    (labels ((search-expr (expr)
               (cond
                 ((stringp expr)
                  (when (and (position #\: expr)
                             (string-equal (subseq expr 0 (position #\: expr))
                                           nickname))
                    (push expr references)))
                 ((consp expr)
                  (search-expr (car expr))
                  (search-expr (cdr expr))))))
      (dolist (form forms)
        (search-expr (parser:form-expr form))))
    references))

(defun find-symbol-references (forms symbol)
  "Find references to a symbol in forms.
Looks for 'CURRENT:symbol', 'package:symbol', or unqualified 'symbol'.
Case-insensitive comparison.
Returns list of strings where symbol was found."
  (let ((references '()))
    (labels ((search-expr (expr)
               (cond
                 ((stringp expr)
                  (when (string-equal (base:symbol-name-from-string expr) symbol)
                    (push expr references)))
                 ((consp expr)
                  (search-expr (car expr))
                  (search-expr (cdr expr))))))
      (dolist (form forms)
        (search-expr (parser:form-expr form))))
    references))
