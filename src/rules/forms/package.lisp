(defpackage #:mallet/rules/forms/package
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:base #:mallet/rules/base)
   (#:violation #:mallet/violation)
   (#:parser #:mallet/parser))
  (:export #:unused-local-nicknames-rule
           #:unused-imported-symbols-rule))
(in-package #:mallet/rules/forms/package)

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
             (all-forms (nth-value 0 (parser:parse-forms text file)))
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
          (let ((nicknames (extract-local-nicknames-with-exprs (parser:form-expr defpkg-form))))
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
                                             :end-line (parser:form-end-line form)
                                             :end-column (parser:form-end-column form)
                                             :message (format nil "Local nickname '~A' (for ~A) is unused"
                                                              nickname package)
                                             :severity (base:rule-severity rule))))))))))

(defmethod base:make-fix ((rule unused-local-nicknames-rule) text file violation)
  "Generate fix for unused local nickname - remove it from defpackage."
  (declare (ignore file))
  (check-type text string)

  ;; Extract the unused nickname from the violation message
  (let ((nickname (extract-unused-nickname-from-message (violation:violation-message violation))))
    (unless nickname
      (return-from base:make-fix nil))

    ;; Re-parse the file to get the defpackage form
    (let* ((all-forms (nth-value 0 (parser:parse-forms text file)))
           ;; Find the defpackage form that contains this violation
           ;; The violation has end-line/end-column pointing to the whole defpackage
           (defpkg-form (find-if
                         (lambda (f)
                           (let ((expr (parser:form-expr f)))
                             (and (consp expr)
                                  (stringp (first expr))
                                  (base:symbol-matches-p (first expr) "DEFPACKAGE")
                                  ;; Check if this defpackage contains the violation
                                  (or
                                   ;; Match by end positions (if available)
                                   (and (violation:violation-end-line violation)
                                        (= (parser:form-end-line f) (violation:violation-end-line violation))
                                        (= (parser:form-end-column f) (violation:violation-end-column violation)))
                                   ;; Or check if violation is within the form
                                   (and (<= (parser:form-line f) (violation:violation-line violation))
                                        (>= (parser:form-end-line f) (violation:violation-line violation)))))))
                         all-forms)))
      (unless defpkg-form
        (return-from base:make-fix nil))

      ;; Get the S-expression and remove the unused nickname
      (let* ((defpkg-expr (parser:form-expr defpkg-form))
             (modified-expr (remove-local-nickname defpkg-expr nickname)))
        (unless modified-expr
          (return-from base:make-fix nil))

        ;; Pretty-print the modified form
        (let ((replacement-content (pretty-print-defpackage modified-expr)))
          ;; Create the fix
          ;; Use defpackage form's line/column, not violation's (which points to the nickname/symbol)
          (violation:make-violation-fix
           :type :replace-form
           :start-line (parser:form-line defpkg-form)
           :end-line (parser:form-end-line defpkg-form)
           :replacement-content replacement-content))))))

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
             (all-forms (nth-value 0 (parser:parse-forms text file)))
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
          (let ((imports (extract-imported-symbols-with-exprs (parser:form-expr defpkg-form)))
                (exports (extract-exports (parser:form-expr defpkg-form)))
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
                                             :end-line (parser:form-end-line form)
                                             :end-column (parser:form-end-column form)
                                             :message (format nil "Imported symbol '~A' from ~A is unused"
                                                              (base:symbol-name-from-string sym) pkg)
                                             :severity (base:rule-severity rule))
                              violations)))))))
            (nreverse violations)))))))

(defmethod base:make-fix ((rule unused-imported-symbols-rule) text file violation)
  "Generate fix for unused imported symbol - remove it from defpackage."
  (declare (ignore file))
  (check-type text string)

  ;; Extract the unused symbol and package from the violation message
  ;; Message format: "Imported symbol 'SYM' from PKG is unused"
  (let* ((message (violation:violation-message violation))
         (symbol (extract-unused-symbol-from-message message))
         ;; Extract package name
         (pkg-start (search " from " message))
         (pkg-end (when pkg-start (search " is unused" message)))
         (package (when (and pkg-start pkg-end)
                    (string-upcase (string-trim " " (subseq message (+ pkg-start 6) pkg-end))))))
    (unless (and symbol package)
      (return-from base:make-fix nil))

    ;; Re-parse the file to get the defpackage form
    (let* ((all-forms (nth-value 0 (parser:parse-forms text file)))
           ;; Find the defpackage form that contains this violation
           ;; The violation has end-line/end-column pointing to the whole defpackage
           (defpkg-form (find-if
                         (lambda (f)
                           (let ((expr (parser:form-expr f)))
                             (and (consp expr)
                                  (stringp (first expr))
                                  (base:symbol-matches-p (first expr) "DEFPACKAGE")
                                  ;; Check if this defpackage contains the violation
                                  (or
                                   ;; Match by end positions (if available)
                                   (and (violation:violation-end-line violation)
                                        (= (parser:form-end-line f) (violation:violation-end-line violation))
                                        (= (parser:form-end-column f) (violation:violation-end-column violation)))
                                   ;; Or check if violation is within the form
                                   (and (<= (parser:form-line f) (violation:violation-line violation))
                                        (>= (parser:form-end-line f) (violation:violation-line violation)))))))
                         all-forms)))
      (unless defpkg-form
        (return-from base:make-fix nil))

      ;; Get the S-expression and remove the unused symbol
      (let* ((defpkg-expr (parser:form-expr defpkg-form))
             (modified-expr (remove-imported-symbol defpkg-expr symbol package)))
        (unless modified-expr
          (return-from base:make-fix nil))

        ;; Pretty-print the modified form
        (let ((replacement-content (pretty-print-defpackage modified-expr)))
          ;; Create the fix
          ;; Use defpackage form's line/column, not violation's (which points to the nickname/symbol)
          (violation:make-violation-fix
           :type :replace-form
           :start-line (parser:form-line defpkg-form)
           :end-line (parser:form-end-line defpkg-form)
           :replacement-content replacement-content))))))

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
    (dolist (form forms)
      (base:traverse-expr (parser:form-expr form)
                          (lambda (expr)
                            (when (and (stringp expr)
                                       (position #\: expr :test #'char=))
                              (let ((colon-pos (position #\: expr :test #'char=)))
                                (when (string-equal (subseq expr 0 colon-pos)
                                                    nickname)
                                  (push expr references)))))))
    references))

(defun find-symbol-references (forms symbol)
  "Find references to a symbol in forms.
Looks for 'CURRENT:symbol', 'package:symbol', or unqualified 'symbol'.
Case-insensitive comparison.
Returns list of strings where symbol was found."
  (let ((references '()))
    (dolist (form forms)
      (base:traverse-expr (parser:form-expr form)
                          (lambda (expr)
                            (when (and (stringp expr)
                                       (string-equal (base:symbol-name-from-string expr) symbol))
                              (push expr references)))))
    references))

;;; Defpackage Modification Helpers for Auto-Fix

(defun extract-unused-nickname-from-message (message)
  "Extract the unused nickname from a violation message.
Message format: \"Local nickname 'NICK' (for PACKAGE) is unused\"
Returns the nickname as a string (uppercase), or NIL if not found."
  (let* ((start (search "Local nickname '" message))
         (end (when start (search "'" message :start2 (+ start 16)))))
    (when (and start end)
      (string-upcase (subseq message (+ start 16) end)))))

(defun extract-unused-symbol-from-message (message)
  "Extract the unused symbol from a violation message.
Message format: \"Imported symbol 'SYM' from PKG is unused\"
Returns the symbol as a string (uppercase), or NIL if not found."
  (let* ((start (search "Imported symbol '" message))
         (end (when start (search "'" message :start2 (+ start 17)))))
    (when (and start end)
      (string-upcase (subseq message (+ start 17) end)))))

(defun remove-local-nickname (defpackage-form nickname)
  "Remove a local nickname from a defpackage form.
DEFPACKAGE-FORM - The parsed defpackage S-expression
NICKNAME - The nickname to remove (string, case-insensitive)

Returns the modified defpackage form, or NIL if nickname not found."
  (let ((modified nil)
        (found nil))
    (cons (first defpackage-form)  ; defpackage
          (cons (second defpackage-form)  ; package name
                (loop for clause in (cddr defpackage-form)
                      collect (if (and (consp clause)
                                       (stringp (first clause))
                                       (string-equal (first clause) ":local-nicknames"))
                                  ;; This is the :local-nicknames clause
                                  (let ((filtered-nicknames
                                          (loop for pair in (rest clause)
                                                unless (and (consp pair)
                                                            (stringp (first pair))
                                                            (string-equal
                                                             (string-trim "#:" (first pair))
                                                             (string-trim "#:" nickname)))
                                                  collect pair
                                                else do (setf found t))))
                                    (setf modified t)
                                    ;; If there are remaining nicknames, keep the clause
                                    ;; Otherwise, don't include it (remove empty clause)
                                    (when filtered-nicknames
                                      (cons ":local-nicknames" filtered-nicknames)))
                                  ;; Not the :local-nicknames clause, keep as-is
                                  clause))))))

(defun remove-imported-symbol (defpackage-form symbol package)
  "Remove an imported symbol from a defpackage form.
DEFPACKAGE-FORM - The parsed defpackage S-expression
SYMBOL - The symbol to remove (string, case-insensitive)
PACKAGE - The package it's imported from (string, case-insensitive)

Returns the modified defpackage form, or NIL if symbol not found."
  (let ((modified nil)
        (found nil))
    (cons (first defpackage-form)  ; defpackage
          (cons (second defpackage-form)  ; package name
                (loop for clause in (cddr defpackage-form)
                      collect (if (and (consp clause)
                                       (stringp (first clause))
                                       (string-equal (first clause) ":import-from"))
                                  ;; This is an :import-from clause
                                  (let* ((pkg-name (second clause))
                                         (symbols (cddr clause)))
                                    (if (string-equal (string-trim "#:" pkg-name)
                                                      (string-trim "#:" package))
                                        ;; This is the package we're looking for
                                        (let ((filtered-symbols
                                                (loop for sym in symbols
                                                      unless (and (stringp sym)
                                                                  (string-equal
                                                                   (string-trim "#:" sym)
                                                                   (string-trim "#:" symbol)))
                                                        collect sym
                                                      else do (setf found t))))
                                          (setf modified t)
                                          ;; If there are remaining symbols, keep the clause
                                          ;; Otherwise, don't include it (remove empty clause)
                                          (when filtered-symbols
                                            (list* ":import-from" pkg-name filtered-symbols)))
                                        ;; Different package, keep as-is
                                        clause))
                                  ;; Not an :import-from clause, keep as-is
                                  clause))))))

(defun symbol-name-matches-p (str target)
  "Check if symbol string STR matches TARGET (ignoring package prefix and case)."
  (let* ((colon-pos (position #\: str :from-end t))
         (symbol-part (if colon-pos (subseq str (1+ colon-pos)) str)))
    (string-equal symbol-part target)))

(defun print-symbol-string (str out)
  "Print a symbol string (from parser) as a Lisp symbol.
The parser returns symbols as strings like 'defpackage', ':use', 'CURRENT:foo', etc."
  (cond
    ;; Keyword (starts with :)
    ((and (> (length str) 0) (char= (char str 0) #\:))
     (format out "~(~A~)" str))
    ;; Common Lisp special operators and macros - print without #: prefix
    ((or (symbol-name-matches-p str "defpackage")
         (symbol-name-matches-p str "in-package")
         (symbol-name-matches-p str "cl")
         (symbol-name-matches-p str "common-lisp"))
     ;; Extract just the symbol part (after last :)
     (let ((colon-pos (position #\: str :from-end t)))
       (if colon-pos
           (format out "~(~A~)" (subseq str (1+ colon-pos)))
           (format out "~(~A~)" str))))
    ;; Qualified symbol (contains :) - strip package prefix, print symbol part as uninterned
    ((position #\: str)
     (let ((colon-pos (position #\: str :from-end t)))
       (format out "#:~(~A~)" (subseq str (1+ colon-pos)))))
    ;; Unqualified symbol - print as uninterned
    (t
     (format out "#:~(~A~)" str))))

(defun pretty-print-defpackage (defpackage-form)
  "Pretty-print a defpackage form with proper indentation.
Returns a string with the formatted defpackage form."
  (with-output-to-string (out)
    ;; Print opening: (defpackage #:package-name
    (format out "(")
    (print-symbol-string (first defpackage-form) out)
    (format out " ")
    (print-symbol-string (second defpackage-form) out)

    ;; Print each clause on its own line
    (dolist (clause (cddr defpackage-form))
      (when clause  ; Skip nil clauses (removed clauses)
        (format out "~%  ")
        (if (and (consp clause) (stringp (first clause)))
            ;; Print clause keyword and contents
            (progn
              (format out "(")
              (print-symbol-string (first clause) out)
              ;; Handle different clause types
              (cond
                ;; :local-nicknames - print each pair on its own line
                ((string-equal (first clause) ":local-nicknames")
                 (dolist (pair (rest clause))
                   (format out "~%   (")
                   (print-symbol-string (first pair) out)
                   (format out " ")
                   (print-symbol-string (second pair) out)
                   (format out ")")))
                ;; :import-from - package on same line, symbols on next lines
                ((string-equal (first clause) ":import-from")
                 (format out " ")
                 (print-symbol-string (second clause) out)
                 (dolist (sym (cddr clause))
                   (format out "~%   ")
                   (print-symbol-string sym out)))
                ;; :use, :export - print items on same line or separate lines depending on count
                ((or (string-equal (first clause) ":use")
                     (string-equal (first clause) ":export"))
                 (dolist (item (rest clause))
                   (format out " ")
                   (print-symbol-string item out)))
                ;; Other clauses
                (t
                 (dolist (item (rest clause))
                   (if (consp item)
                       (progn
                         (format out "~%   (")
                         (dolist (sub-item item)
                           (if (stringp sub-item)
                               (print-symbol-string sub-item out)
                               (format out "~S" sub-item))
                           (format out " "))
                         (format out ")"))
                       (progn
                         (format out " ")
                         (if (stringp item)
                             (print-symbol-string item out)
                             (format out "~S" item)))))))
              (format out ")"))
            ;; Non-list clause (shouldn't happen in defpackage)
            (format out "~S" clause))))

    ;; Print closing
    (format out ")")
    ;; Add final newline
    (format out "~%")))
