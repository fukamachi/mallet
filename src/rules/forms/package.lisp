(defpackage #:mallet/rules/forms/package
  (:use #:cl)
  (:local-nicknames
   (#:base #:mallet/rules/base)
   (#:violation #:mallet/violation)
   (#:parser #:mallet/parser))
  (:export #:interned-package-symbol-rule
           #:unused-local-nicknames-rule
           #:unused-imported-symbols-rule))
(in-package #:mallet/rules/forms/package)

(defun defpackage-form-p (expr)
  "Check if EXPR is a defpackage or uiop:define-package form."
  (and (consp expr)
       (stringp (first expr))
       (or (base:symbol-matches-p (first expr) "DEFPACKAGE")
           (base:symbol-matches-p (first expr) "DEFINE-PACKAGE"))))

(defun in-package-form-p (expr)
  "Check if EXPR is an in-package form."
  (and (consp expr)
       (stringp (first expr))
       (base:symbol-matches-p (first expr) "IN-PACKAGE")))

;;; Interned Package Symbol Rule

(defclass interned-package-symbol-rule (base:rule)
  ()
  (:default-initargs
   :name :interned-package-symbol
   :description "Use uninterned symbols (#:symbol) in package definitions"
   :severity :convention
   :type :form))

(defun extract-symbol-source-at (source-text line column)
  "Extract symbol source starting at LINE and COLUMN (0-based column)."
  (let ((lines (uiop:split-string source-text :separator '(#\Newline))))
    (when (and (plusp line) (<= line (length lines)))
      (let ((line-text (nth (1- line) lines)))
        (when (< column (length line-text))
          (let ((result '())
                (state nil)
                (escaped nil))
            (loop for idx from column below (length line-text)
                  for ch = (char line-text idx)
                  do (cond
                       (escaped
                        (push ch result)
                        (setf escaped nil))
                       ((char= ch #\\)
                        (push ch result)
                        (setf escaped t))
                       ;; Delimiters when not inside quoted symbol/string
                       ((and (null state)
                             (find ch '(#\Space #\Tab #\) #\( #\;) :test #'char=))
                        (return (and result (coerce (nreverse result) 'string))))
                       ;; String literal - read until closing quote
                       ((and (null state) (char= ch #\"))
                        (push ch result)
                        (setf state :string))
                       ((and (eq state :string) (char= ch #\"))
                        (push ch result)
                        (return (coerce (nreverse result) 'string)))
                       ;; Escaped symbol with pipes - read until closing pipe
                       ((and (null state) (char= ch #\|))
                        (push ch result)
                        (setf state :pipe))
                       ((and (eq state :pipe) (char= ch #\|))
                        (push ch result)
                        (return (coerce (nreverse result) 'string)))
                       (t
                        (push ch result))))
            (when result
              (coerce (nreverse result) 'string))))))))

(defun get-source-text-for-expr (expr position-map source-text)
  "Extract original source text for EXPR using POSITION-MAP and SOURCE-TEXT."
  (when (and position-map source-text)
    (let ((pos (gethash expr position-map)))
      (when pos
        (extract-symbol-source-at source-text (car pos) (cdr pos))))))

(defun classify-symbol-type (symbol-expr source-text)
  "Classify SYMBOL-EXPR using parsed value and SOURCE-TEXT.
Returns one of :keyword, :qualified, :uninterned, :bare, :string-literal, or :unknown."
  (cond
    ;; Keywords are detectable from parsed form (start with :)
    ((and (stringp symbol-expr)
          (plusp (length symbol-expr))
          (char= (char symbol-expr 0) #\:))
     (if (position #\: symbol-expr :start 1)
         :qualified
         :keyword))
    ;; Bare symbols in current package (parser adds CURRENT: prefix)
    ((and (stringp symbol-expr)
          (>= (length symbol-expr) 8)
          (string= "CURRENT:" symbol-expr :end2 8))
     :bare)
    ;; Qualified symbols (pkg:foo) - has : not at start and not CURRENT:
    ((and (stringp symbol-expr)
          (position #\: symbol-expr :start 1))
     :qualified)
    ;; Need source text for remaining cases (e.g., uninterned symbols)
    ((or (null source-text) (zerop (length source-text)))
     :unknown)
    ;; String package designator
    ((char= (char source-text 0) #\")
     :string-literal)
    ;; Uninterned symbol
    ((and (>= (length source-text) 2)
          (string= "#:" source-text :end2 2))
     :uninterned)
    ;; Quoted symbols count as bare
    ((char= (char source-text 0) #\')
     :bare)
    ;; Default: bare symbol
    (t :bare)))

(defun symbol-type-description (symbol-type)
  "Human-readable description for SYMBOL-TYPE."
  (case symbol-type
    (:keyword "keyword")
    (:qualified "qualified")
    (:bare "bare symbol")
    (otherwise "unknown")))

(defun check-symbol-interned (symbol-expr context position-map source-text form file rule)
  "Check a single SYMBOL-EXPR in CONTEXT, returning violations if interned."
  (let* ((expr-source (get-source-text-for-expr symbol-expr position-map source-text))
         (symbol-type (classify-symbol-type symbol-expr expr-source)))
    (case symbol-type
      ((:uninterned :string-literal)
       nil)
      ((:keyword :qualified :bare)
       (multiple-value-bind (line column)
           (if position-map
               (parser:find-position symbol-expr position-map
                                     (parser:form-line form)
                                     (parser:form-column form))
               (values (parser:form-line form) (parser:form-column form)))
         (list (make-instance 'violation:violation
                              :rule (base:rule-name rule)
                              :file file
                              :line line
                              :column column
                              :end-line (parser:form-end-line form)
                              :end-column (parser:form-end-column form)
                              :message (format nil "Use uninterned symbol #:~A instead of ~A (~A) in ~A"
                                               (string-upcase (base:symbol-name-from-string symbol-expr))
                                               (or expr-source symbol-expr)
                                               (symbol-type-description symbol-type)
                                               context)
                              :severity (base:rule-severity rule)))))
      (otherwise
       nil))))

(defun check-clause-symbols (clause position-map source-text form file rule)
  "Check symbols within a DEFPACKAGE clause."
  (when (and (consp clause) (stringp (first clause)))
    (let ((keyword (first clause)))
      (cond
        ((string-equal keyword ":use")
         (loop for pkg in (rest clause)
               append (check-symbol-interned pkg ":use clause" position-map source-text form file rule)))
        ((string-equal keyword ":export")
         (loop for sym in (rest clause)
               append (check-symbol-interned sym ":export clause" position-map source-text form file rule)))
        ((string-equal keyword ":shadow")
         (loop for sym in (rest clause)
               append (check-symbol-interned sym ":shadow clause" position-map source-text form file rule)))
        ((string-equal keyword ":intern")
         (loop for sym in (rest clause)
               append (check-symbol-interned sym ":intern clause" position-map source-text form file rule)))
        ((string-equal keyword ":nicknames")
         (loop for sym in (rest clause)
               append (check-symbol-interned sym ":nicknames clause" position-map source-text form file rule)))
        ((or (string-equal keyword ":import-from")
             (string-equal keyword ":shadowing-import-from"))
         (let ((package (second clause))
               (symbols (cddr clause))
               (context (format nil "~A clause" keyword)))
           (append (check-symbol-interned package context position-map source-text form file rule)
                   (loop for sym in symbols
                         append (check-symbol-interned sym context position-map source-text form file rule)))))
        ((string-equal keyword ":local-nicknames")
         (loop for pair in (rest clause)
               append (when (and (consp pair) (= (length pair) 2))
                        (append (check-symbol-interned (first pair) ":local-nicknames clause"
                                                       position-map source-text form file rule)
                                (check-symbol-interned (second pair) ":local-nicknames clause"
                                                       position-map source-text form file rule)))))
        ;; UIOP-specific clauses
        ((member keyword '(":mix" ":reexport" ":use-reexport" ":unintern" ":recycle")
                 :test #'string-equal)
         (let ((context (format nil "~A clause" keyword)))
           (loop for sym in (rest clause)
                 append (check-symbol-interned sym context position-map source-text form file rule))))
        ;; Explicit skips
        ((or (string-equal keyword ":documentation")
             (string-equal keyword ":size"))
         nil)
        (t
         nil)))))

(defun check-defpackage-form (expr form file rule source-text position-map)
  "Check DEFPACKAGE or DEFINE-PACKAGE EXPR for interned symbols."
  (let ((violations (check-symbol-interned (second expr) "defpackage"
                                           position-map source-text form file rule)))
    (dolist (clause (cddr expr))
      (setf violations
            (nconc violations
                   (check-clause-symbols clause position-map source-text form file rule))))
    violations))

(defmethod base:check-form ((rule interned-package-symbol-rule) form file)
  "Detect interned symbols in package definitions."
  (check-type form parser:form)
  (check-type file pathname)

  (let* ((expr (parser:form-expr form)))
    (cond
      ((defpackage-form-p expr)
       (let ((source-text (uiop:read-file-string file)))
         (check-defpackage-form expr form file rule source-text (parser:form-position-map form))))
      ((in-package-form-p expr)
       (let* ((source-text (uiop:read-file-string file))
              (position-map (parser:form-position-map form)))
         (check-symbol-interned (second expr) "in-package"
                                position-map source-text form file rule)))
      (t nil))))

;;; Unused Local Nicknames Rule

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
                              ;; Calculate end position for the specific nickname pair
                              (multiple-value-bind (end-line end-column)
                                  (find-list-end-position text line column)
                                (make-instance 'violation:violation
                                               :rule (base:rule-name rule)
                                               :file file
                                               :line line
                                               :column column
                                               :end-line (or end-line line)
                                               :end-column (or end-column (1+ column))
                                               :message (format nil "Local nickname '~A' (for ~A) is unused"
                                                                nickname package)
                                               :severity (base:rule-severity rule)))))))))))

(defun find-symbol-end-column (line-text start-col)
  "Find the end column of a symbol starting at START-COL in LINE-TEXT."
  (or (position-if (lambda (ch)
                     (or (char= ch #\Space)
                         (char= ch #\Tab)
                         (char= ch #\))
                         (char= ch #\Newline)))
                   line-text
                   :start start-col)
      (length line-text)))

(defun find-symbol-end-position (text line column)
  "Find the end position of a symbol starting at LINE/COLUMN in TEXT.
LINE is 1-indexed. Returns (values end-line end-column) where end-column
is the column after the last character of the symbol."
  (let* ((lines (uiop:split-string text :separator '(#\Newline)))
         (line-text (when (and (<= 1 line) (<= line (length lines)))
                      (nth (1- line) lines))))
    (when (and line-text (<= column (length line-text)))
      (let ((end-col (find-symbol-end-column line-text column)))
        (values line end-col)))))

(defun find-list-end-position (text line column)
  "Find the end position of a list starting at LINE/COLUMN in TEXT.
LINE is 1-indexed. The position should point to an opening paren.
Returns (values end-line end-column) where end-column is the column
after the closing paren."
  (let* ((lines (uiop:split-string text :separator '(#\Newline)))
         (num-lines (length lines))
         (paren-depth 0)
         (started nil))
    (loop for current-line from line to num-lines
          for line-text = (nth (1- current-line) lines)
          for start-col = (if (= current-line line) column 0)
          do (loop for col from start-col below (length line-text)
                   for ch = (char line-text col)
                   do (cond
                        ((char= ch #\()
                         (incf paren-depth)
                         (setf started t))
                        ((char= ch #\))
                         (decf paren-depth)
                         (when (and started (zerop paren-depth))
                           (return-from find-list-end-position
                             (values current-line (1+ col)))))))
          finally (return (values nil nil)))))

(defun make-single-line-deletion-fix (lines violation-line end-col)
  "Generate fix for deleting a single-line expression.
Handles moving trailing close parens to previous line if needed."
  (let* ((line-text (nth (1- violation-line) lines))
         (trailing-text (subseq line-text end-col)))
    (if (and (> (length trailing-text) 0)
             (find #\) trailing-text)
             (> violation-line 1))
        ;; Has trailing close parens - move them to previous line
        (let* ((prev-line (nth (- violation-line 2) lines))
               (trailing-comment-pos (base:find-comment-start trailing-text))
               (parens-only (string-trim '(#\Space #\Tab)
                                         (if trailing-comment-pos
                                             (subseq trailing-text 0 trailing-comment-pos)
                                             trailing-text)))
               (prev-comment-pos (base:find-comment-start prev-line))
               (prev-line-code (string-right-trim '(#\Space #\Tab)
                                                  (if prev-comment-pos
                                                      (subseq prev-line 0 prev-comment-pos)
                                                      prev-line)))
               (prev-line-comment (if prev-comment-pos
                                      (subseq prev-line prev-comment-pos)
                                      ""))
               (replacement (concatenate 'string
                                         prev-line-code
                                         parens-only
                                         (if (> (length prev-line-comment) 0)
                                             (concatenate 'string "  " prev-line-comment)
                                             "")
                                         (string #\Newline))))
          (violation:make-violation-fix
           :type :replace-form
           :start-line (1- violation-line)
           :end-line violation-line
           :replacement-content replacement))
        ;; No trailing close parens - delete entire line
        (violation:make-violation-fix
         :type :delete-range
         :start-line violation-line
         :start-column 0
         :end-line (1+ violation-line)
         :end-column 0))))

(defmethod base:make-fix ((rule unused-local-nicknames-rule) text file violation)
  "Generate minimal fix for unused local nickname - remove just that line.
Preserves comments, formatting, and structural close parens."
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

          (t
           (multiple-value-bind (end-line end-col)
               (base:find-expression-end-position text violation-line violation-col)
             (if (and end-line end-col (= violation-line end-line))
                 (make-single-line-deletion-fix
                  (uiop:split-string text :separator '(#\Newline))
                  violation-line
                  end-col)
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
                        ;; Calculate end position for the specific symbol
                        (multiple-value-bind (end-line end-column)
                            (find-symbol-end-position text line column)
                          (push (make-instance 'violation:violation
                                               :rule (base:rule-name rule)
                                               :file file
                                               :line line
                                               :column column
                                               :end-line (or end-line line)
                                               :end-column (or end-column (1+ column))
                                               :message (format nil "Imported symbol '~A' from ~A is unused"
                                                                (base:symbol-name-from-string sym) pkg)
                                               :severity (base:rule-severity rule))
                                violations))))))))
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

          (t
           (let* ((lines (uiop:split-string text :separator '(#\Newline)))
                  (line-text (nth (1- violation-line) lines))
                  (end-col (find-symbol-end-column line-text violation-col)))
             (if end-col
                 (make-single-line-deletion-fix lines violation-line end-col)
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
