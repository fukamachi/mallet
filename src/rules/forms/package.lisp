(defpackage #:mallet/rules/forms/package
  (:use #:cl)
  (:local-nicknames
   (#:base #:mallet/rules/base)
   (#:violation #:mallet/violation)
   (#:parser #:mallet/parser))
  (:export #:unused-local-nicknames-rule
           #:unused-imported-symbols-rule))
(in-package #:mallet/rules/forms/package)

;;; Unused Local Nicknames Rule

(defun defpackage-form-p (expr)
  "Check if EXPR is a defpackage or uiop:define-package form."
  (and (consp expr)
       (stringp (first expr))
       (or (base:symbol-matches-p (first expr) "DEFPACKAGE")
           (base:symbol-matches-p (first expr) "DEFINE-PACKAGE"))))

(defclass unused-local-nicknames-rule (base:rule)
  ()
  (:default-initargs
   :name :unused-local-nicknames
   :description "Local nicknames in :local-nicknames should be used"
   :severity :info
   :type :form))

(defmethod base:check-form ((rule unused-local-nicknames-rule) form file)
  "Check for unused local nicknames in defpackage/define-package forms."
  (let ((expr (parser:form-expr form)))
    (when (defpackage-form-p expr)
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
  "Generate minimal fix for unused local nickname - remove just that line.
Preserves comments, formatting, and structural close parens."
  (declare (ignore file))
  (check-type text string)

  ;; Extract the unused nickname from the violation message
  (let ((nickname (extract-unused-nickname-from-message (violation:violation-message violation))))
    (unless nickname
      (return-from base:make-fix nil))

    ;; Re-parse to check if this is the last nickname
    (let* ((all-forms (nth-value 0 (parser:parse-forms text file)))
           (defpkg-form (find-if
                         (lambda (f)
                           (let ((expr (parser:form-expr f)))
                             (and (defpackage-form-p expr)
                                  (or
                                   (and (violation:violation-end-line violation)
                                        (= (parser:form-end-line f) (violation:violation-end-line violation))
                                        (= (parser:form-end-column f) (violation:violation-end-column violation)))
                                   (and (<= (parser:form-line f) (violation:violation-line violation))
                                        (>= (parser:form-end-line f) (violation:violation-line violation)))))))
                         all-forms)))
      (unless defpkg-form
        (return-from base:make-fix nil))

      (let* ((defpkg-expr (parser:form-expr defpkg-form))
             (used-count (count-used-local-nicknames defpkg-expr text file))
             (violation-line (violation:violation-line violation))
             (violation-col (violation:violation-column violation)))

        (cond
          ;; All nicknames unused: delete entire clause
          ;; All violations get the same fix; fixer will deduplicate
          ((zerop used-count)
           (find-local-nicknames-clause-line-range text violation-line))

          ;; Some nicknames still used: delete only this nickname
          (t
           (multiple-value-bind (end-line end-col)
               (base:find-expression-end-position text violation-line violation-col)
             (if (and end-line end-col (= violation-line end-line))
                 ;; Expression is on single line
                 (let* ((lines (uiop:split-string text :separator '(#\Newline)))
                        (line-text (nth (1- violation-line) lines))
                        (trailing-text (subseq line-text end-col)))
                   (if (and (> (length trailing-text) 0)
                            (find #\) trailing-text)
                            ;; Only tidy parens if there's a previous line
                            (> violation-line 1))
                       ;; Has trailing close parens - move them to previous line
                       ;; Delete from end of previous line through the current line,
                       ;; but only preserve close parens (strip comments)
                       (let* ((prev-line (nth (- violation-line 2) lines))
                              ;; Find where comment starts (if any) in trailing-text
                              ;; Use string-aware detection to avoid false positives with semicolons in strings
                              (trailing-comment-pos (base:find-comment-start trailing-text))
                              ;; Extract just the close parens part (before comment/whitespace)
                              (parens-only (string-trim '(#\Space #\Tab)
                                             (if trailing-comment-pos
                                                 (subseq trailing-text 0 trailing-comment-pos)
                                                 trailing-text)))
                              ;; Find comment in previous line (if any)
                              ;; Use string-aware detection
                              (prev-comment-pos (base:find-comment-start prev-line))
                              ;; Split previous line into code and comment parts
                              ;; Trim trailing whitespace from code part
                              (prev-line-code (string-right-trim '(#\Space #\Tab)
                                                (if prev-comment-pos
                                                    (subseq prev-line 0 prev-comment-pos)
                                                    prev-line)))
                              (prev-line-comment (if prev-comment-pos
                                                     (subseq prev-line prev-comment-pos)
                                                     ""))
                              ;; New content: code + parens + comment (comment after parens)
                              ;; (strips current line's comment)
                              (replacement (concatenate 'string
                                             prev-line-code
                                             parens-only
                                             (if (> (length prev-line-comment) 0)
                                                 (concatenate 'string "  " prev-line-comment)
                                                 "")
                                             (string #\Newline))))
                         ;; Replace both lines (prev and current) with new content
                         ;; This moves close parens up, deletes unused nickname, strips comment
                         (violation:make-violation-fix
                          :type :replace-form
                          :start-line (1- violation-line)
                          :end-line violation-line
                          :replacement-content replacement))
                       ;; No trailing close parens - delete entire line including newline
                       (violation:make-violation-fix
                        :type :delete-range
                        :start-line violation-line
                        :start-column 0
                        :end-line (1+ violation-line)
                        :end-column 0)))
                 ;; Multi-line or couldn't find end - fall back to line deletion
                 (violation:make-violation-fix
                  :type :delete-lines
                  :start-line violation-line
                  :end-line violation-line)))))))))

;;; Unused Imported Symbols Rule

(defclass unused-imported-symbols-rule (base:rule)
  ()
  (:default-initargs
   :name :unused-imported-symbols
   :description "Imported symbols from :import-from should be used or re-exported"
   :severity :info
   :type :form))

(defmethod base:check-form ((rule unused-imported-symbols-rule) form file)
  "Check for unused imported symbols in defpackage/define-package forms."
  (let ((expr (parser:form-expr form)))
    (when (defpackage-form-p expr)
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

(defun count-imported-symbols-in-clause (defpackage-expr package-name)
  "Count the number of symbols imported from PACKAGE-NAME in DEFPACKAGE-EXPR."
  (let ((count 0))
    (dolist (clause (cddr defpackage-expr))
      (when (and (consp clause)
                 (stringp (first clause))
                 (string-equal (first clause) ":import-from"))
        (let ((pkg (second clause)))
          (when (string-equal (base:symbol-name-from-string pkg) package-name)
            (setf count (length (cddr clause)))
            (return)))))
    count))

(defun find-import-from-clause-line-range (text violation-line package-name)
  "Find the line range of the :import-from clause for PACKAGE-NAME containing VIOLATION-LINE.
Returns a violation-fix for :delete-lines."
  (let* ((lines (uiop:split-string text :separator '(#\Newline)))
         (start-line nil)
         (end-line nil))

    ;; Search backward to find the :import-from clause start for the correct package
    (loop for line-num from violation-line downto 1
          for line-text = (nth (1- line-num) lines)
          do (when (and (search ":import-from" line-text :test #'char-equal)
                        (search package-name line-text :test #'char-equal))
               (setf start-line line-num)
               (return)))

    (unless start-line
      (return-from find-import-from-clause-line-range nil))

    ;; Find the end by counting parens from start-line
    (let ((paren-depth 0)
          (found-open nil))
      (loop for line-num from start-line to (length lines)
            for line-text = (nth (1- line-num) lines)
            do (loop for ch across line-text
                     do (cond
                          ((char= ch #\()
                           (incf paren-depth)
                           (setf found-open t))
                          ((char= ch #\))
                           (decf paren-depth)
                           (when (and found-open (zerop paren-depth))
                             (setf end-line line-num)
                             (return-from find-import-from-clause-line-range
                               (violation:make-violation-fix
                                :type :delete-lines
                                :start-line start-line
                                :end-line end-line))))))
            when (and found-open (zerop paren-depth))
              do (return)))

    ;; If we get here, couldn't find balanced parens
    nil))

(defmethod base:make-fix ((rule unused-imported-symbols-rule) text file violation)
  "Generate minimal fix for unused imported symbol - remove just that line.
Preserves comments, formatting, and structural close parens."
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

    ;; Re-parse to check if this is the last symbol in the import-from clause
    (let* ((all-forms (nth-value 0 (parser:parse-forms text file)))
           (defpkg-form (find-if
                         (lambda (f)
                           (let ((expr (parser:form-expr f)))
                             (and (defpackage-form-p expr)
                                  (or
                                   (and (violation:violation-end-line violation)
                                        (= (parser:form-end-line f) (violation:violation-end-line violation))
                                        (= (parser:form-end-column f) (violation:violation-end-column violation)))
                                   (and (<= (parser:form-line f) (violation:violation-line violation))
                                        (>= (parser:form-end-line f) (violation:violation-line violation)))))))
                         all-forms)))
      (unless defpkg-form
        (return-from base:make-fix nil))

      (let* ((defpkg-expr (parser:form-expr defpkg-form))
             (symbol-count (count-imported-symbols-in-clause defpkg-expr package))
             (violation-line (violation:violation-line violation))
             (violation-col (violation:violation-column violation)))

        (cond
          ;; Last symbol: delete entire :import-from clause
          ((= symbol-count 1)
           (find-import-from-clause-line-range text violation-line package))

          ;; Multiple symbols: delete just this line, tidy up trailing parens
          (t
           ;; Calculate symbol's end position (symbol is on single line)
           ;; The violation's end-line/end-column are for the whole defpackage form, not the symbol
           (let* ((lines (uiop:split-string text :separator '(#\Newline)))
                  (line-text (nth (1- violation-line) lines))
                  ;; Find end of symbol by scanning for whitespace or paren
                  (end-col (loop for i from violation-col below (length line-text)
                                 for ch = (char line-text i)
                                 when (or (char= ch #\Space)
                                          (char= ch #\Tab)
                                          (char= ch #\))
                                          (char= ch #\Newline))
                                   return i
                                 finally (return (length line-text)))))
             (if end-col
                 ;; Expression is on single line
                 (let ((trailing-text (subseq line-text end-col)))
                   (if (and (> (length trailing-text) 0)
                            (find #\) trailing-text)
                            ;; Only tidy parens if there's a previous line
                            (> violation-line 1))
                       ;; Has trailing close parens - move them to previous line
                       (let* ((prev-line (nth (- violation-line 2) lines))
                              ;; Find where comment starts (if any) in trailing-text
                              (trailing-comment-pos (base:find-comment-start trailing-text))
                              ;; Extract just the close parens part (before comment/whitespace)
                              (parens-only (string-trim '(#\Space #\Tab)
                                             (if trailing-comment-pos
                                                 (subseq trailing-text 0 trailing-comment-pos)
                                                 trailing-text)))
                              ;; Find comment in previous line (if any)
                              (prev-comment-pos (base:find-comment-start prev-line))
                              ;; Split previous line into code and comment parts
                              (prev-line-code (string-right-trim '(#\Space #\Tab)
                                                (if prev-comment-pos
                                                    (subseq prev-line 0 prev-comment-pos)
                                                    prev-line)))
                              (prev-line-comment (if prev-comment-pos
                                                     (subseq prev-line prev-comment-pos)
                                                     ""))
                              ;; New content: code + parens + comment (comment after parens)
                              (replacement (concatenate 'string
                                             prev-line-code
                                             parens-only
                                             (if (> (length prev-line-comment) 0)
                                                 (concatenate 'string "  " prev-line-comment)
                                                 "")
                                             (string #\Newline))))
                         ;; Replace both lines (prev and current) with new content
                         (violation:make-violation-fix
                          :type :replace-form
                          :start-line (1- violation-line)
                          :end-line violation-line
                          :replacement-content replacement))
                       ;; No trailing close parens - delete entire line including newline
                       (violation:make-violation-fix
                        :type :delete-range
                        :start-line violation-line
                        :start-column 0
                        :end-line (1+ violation-line)
                        :end-column 0)))
                 ;; Multi-line or couldn't find end - fall back to line deletion
                 (violation:make-violation-fix
                  :type :delete-lines
                  :start-line violation-line
                  :end-line violation-line)))))))))

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

;;; Defpackage-Specific Helpers

(defun count-local-nicknames (defpackage-expr)
  "Count how many local nicknames are in DEFPACKAGE-EXPR."
  (let ((count 0))
    (dolist (clause (cddr defpackage-expr))
      (when (and (consp clause)
                 (stringp (first clause))
                 (string-equal (first clause) ":local-nicknames"))
        (setf count (length (rest clause)))))
    count))

(defun count-used-local-nicknames (defpackage-expr text file)
  "Count how many local nicknames in DEFPACKAGE-EXPR are actually used.
Returns the count of USED nicknames."
  (let ((nicknames (extract-local-nicknames-with-exprs defpackage-expr)))
    (if (null nicknames)
        0
        ;; Need to find code forms after in-package to check usage
        (let* ((all-forms (nth-value 0 (parser:parse-forms text file)))
               (defpkg-pos (position-if
                            (lambda (f)
                              (defpackage-form-p (parser:form-expr f)))
                            all-forms))
               (forms-after-defpkg (when defpkg-pos (nthcdr (1+ defpkg-pos) all-forms)))
               (in-package-pos (position-if
                                (lambda (f)
                                  (let ((e (parser:form-expr f)))
                                    (and (consp e)
                                         (stringp (first e))
                                         (base:symbol-matches-p (first e) "IN-PACKAGE"))))
                                forms-after-defpkg))
               (code-forms (when in-package-pos
                             (nthcdr (1+ in-package-pos) forms-after-defpkg))))
          (if (null code-forms)
              0  ; No code after in-package, so no nicknames used
              ;; Count how many nicknames are used
              (count-if (lambda (nick-tuple)
                          (let ((nickname (first nick-tuple)))
                            (plusp (length (find-nickname-references code-forms nickname)))))
                        nicknames))))))

(defun is-last-nickname-line-p (text violation-line)
  "Check if VIOLATION-LINE is the last nickname in the :local-nicknames clause.
This is used to ensure only one fix deletes the entire clause when all are unused."
  (multiple-value-bind (clause-start clause-end)
      (base:find-clause-boundaries text violation-line ":local-nicknames")
    (unless (and clause-start clause-end)
      (return-from is-last-nickname-line-p nil))

    ;; Find the last line within the clause that contains a nickname pattern "(#:"
    (let ((lines (uiop:split-string text :separator '(#\Newline)))
          (last-nickname-line nil))
      (loop for line-num from clause-end downto (1+ clause-start)
            for line-text = (nth (1- line-num) lines)
            when (search "(#:" line-text)
              do (setf last-nickname-line line-num)
                 (return))

      ;; Check if violation-line is the last nickname line
      (and last-nickname-line (= violation-line last-nickname-line)))))

(defun find-local-nicknames-clause-line-range (text violation-line)
  "Find the line range of the :local-nicknames clause containing VIOLATION-LINE.
Returns a violation-fix to delete those lines, or NIL if not found.
Uses the generic base:find-clause-line-range helper."
  (base:find-clause-line-range text violation-line ":local-nicknames"))
