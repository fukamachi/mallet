(in-package #:malvolio/parser)

;;; Eclector parse-result based parser for precise source tracking
;;;
;;; This parser uses Eclector's parse-result protocol to track source positions
;;; for ALL expressions (including nested ones), not just top-level forms.
;;; This enables accurate violation reporting.

;;; Custom client for string-based parsing with source tracking

(defclass string-parse-result-client (eclector.parse-result:parse-result-client)
  ()
  (:documentation "Parse-result client that returns string representations of symbols."))

(defmethod eclector.reader:interpret-symbol
    ((client string-parse-result-client) input-stream package-indicator symbol-name internp)
  "Return a string representation of a symbol without interning.

Symbols are represented as strings:
- Keywords: \":NAME\"
- Qualified: \"PACKAGE:NAME\"
- Unqualified: \"NAME\"

This avoids package resolution issues when linting code with unknown packages."
  (declare (ignore input-stream internp))
  (cond
    ;; Keyword
    ((eq package-indicator :keyword)
     (concatenate 'string ":" symbol-name))
    ;; Unqualified symbol (no package indicator or in current package)
    ((or (null package-indicator)
         (eq package-indicator *package*)
         ;; Check if it's the CURRENT package used during parsing
         (and (packagep package-indicator)
              (or (string-equal (package-name package-indicator) "CURRENT")
                  (string-equal (package-name package-indicator) "COMMON-LISP-USER"))))
     symbol-name)
    ;; Qualified symbol (e.g., config:load-config)
    (t
     (concatenate 'string
                  (if (packagep package-indicator)
                      (package-name package-indicator)
                      (string package-indicator))
                  ":"
                  symbol-name))))

(defmethod eclector.reader:check-feature-expression ((client string-parse-result-client) feature-expression)
  "Evaluate feature expressions for reader conditionals.
Always returns T to include all conditional code during linting."
  (declare (ignore feature-expression))
  ;; Always return T to include all conditional code
  ;; This ensures we lint all code regardless of features
  t)

(defmethod eclector.reader:evaluate-expression ((client string-parse-result-client) expression)
  "Handle read-time evaluation (#. reader macro).
Returns a placeholder list since we cannot evaluate expressions with string-based symbols.
Returns a list to ensure it can be safely processed by recursive form-checking code."
  (declare (ignore expression))
  ;; Return a placeholder list for read-time evaluated expressions
  ;; We can't actually evaluate since symbols are strings
  ;; Using a list ensures it won't cause type errors when embedded in parent forms
  '(:read-time-eval-placeholder))

(defmethod eclector.reader:find-character ((client string-parse-result-client) (designator string))
  "Find character by name, supporting SBCL character name extensions.

SBCL supports these character name extensions:
- Nul (code 0)
- Bel (code 7)
- Esc, Escape (code 27) - both variants accepted
- Backspace (code 8)
- Tab (code 9)
- Newline (code 10)
- Page (code 12)
- Return (code 13)
- Space (code 32)
- Rubout (code 127)

The DESIGNATOR parameter contains the exact character name as written in source,
allowing future linting rules to distinguish between variants like #\\Esc vs #\\Escape."
  ;; First try standard character names
  (or (call-next-method)
      ;; If not found, try SBCL extensions
      (cond
        ;; ESC character: both #\Esc and #\Escape map to code 27
        ((or (string-equal designator "Esc")
             (string-equal designator "Escape"))
         (code-char 27))
        ;; NUL character
        ((string-equal designator "Nul")
         (code-char 0))
        ;; BEL character
        ((string-equal designator "Bel")
         (code-char 7))
        ;; Other SBCL names that might already be in standard set
        ;; but we list them for completeness
        ((string-equal designator "Backspace")
         (code-char 8))
        ((string-equal designator "Tab")
         (code-char 9))
        ((string-equal designator "Newline")
         (code-char 10))
        ((string-equal designator "Page")
         (code-char 12))
        ((string-equal designator "Return")
         (code-char 13))
        ((string-equal designator "Space")
         (code-char 32))
        ((string-equal designator "Rubout")
         (code-char 127))
        ;; Unknown character name - return nil to let Eclector signal error
        (t nil))))

(defmethod eclector.parse-result:make-expression-result
    ((client string-parse-result-client) expression children source)
  "Create a parse result wrapping EXPRESSION with source location.

The result is a plist with :expr, :children, and :source keys.
SOURCE is a cons cell (start . end) of character positions."
  (list :expr expression :children children :source source))

;;; Helper functions for position conversion

(defun build-line-starts (text)
  "Build a vector of line start positions in TEXT.
Returns a vector where element N is the character position of line N (0-based)."
  (let ((len (length text)))
    (coerce
      (cons 0
            (loop for i from 0 below len
                  when (char= (char text i) #\Newline)
                  collect (1+ i)))
      'vector)))

(defun char-pos-to-line-column (char-pos line-starts)
  "Convert character position CHAR-POS to (line . column).
LINE-STARTS is a vector from BUILD-LINE-STARTS.
Returns 1-based line and 0-based column."
  (let ((line-count (length line-starts)))
    (loop for line-num from 1 below line-count
          for line-start = (aref line-starts line-num)
          when (< char-pos line-start)
          do (let ((prev-line-start (aref line-starts (1- line-num))))
               (return (values line-num (- char-pos prev-line-start))))
          finally
          (return (values line-count
                          (- char-pos (aref line-starts (1- line-count))))))))

(defun extract-source (text source)
  "Extract source text from TEXT using SOURCE range (start . end)."
  (when (and source (consp source))
    (let ((start (car source))
          (end (cdr source)))
      (when (and (numberp start) (numberp end)
                 (<= 0 start end (length text)))
        (subseq text start end)))))

;;; Position map - stores source positions for expressions

(defvar *position-map* nil
  "Hash table mapping expressions to source positions.")

(defun make-position-map ()
  "Create a new position map."
  (make-hash-table :test 'equal))

(defun build-position-map (parse-result line-starts)
  "Build a position map from PARSE-RESULT.
Returns a hash table mapping expressions to (line . column) positions."
  (let ((position-map (make-position-map)))
    (labels ((process-result (result)
               (when (and result (listp result))
                 (let ((expr (getf result :expr))
                       (source (getf result :source))
                       (children (getf result :children)))
                   ;; Map this expression to its position
                   (when source
                     (multiple-value-bind (line column)
                         (char-pos-to-line-column (car source) line-starts)
                       (setf (gethash expr position-map) (cons line column))))
                   ;; Process children
                   (when children
                     (dolist (child children)
                       (process-result child)))))))
      (process-result parse-result))
    position-map))

(defun find-position (expr position-map fallback-line fallback-column)
  "Find position for EXPR in POSITION-MAP.
Returns (values line column). If not found, returns fallback values."
  (let ((pos (gethash expr position-map)))
    (if pos
        (values (car pos) (cdr pos))
        (values fallback-line fallback-column))))

;;; Main parsing function

(defun parse-forms (text file)
  "Parse forms from TEXT in FILE using Eclector parse-result.
Returns a list of FORM objects with source location information and position maps.

Uses eclector.parse-result to track positions for all nested expressions,
enabling accurate violation reporting."
  (check-type text string)
  (check-type file pathname)

  (let ((forms '())
        (client (make-instance 'string-parse-result-client))
        (stream (make-string-input-stream text))
        (line-starts (build-line-starts text)))

    ;; Set readtable case to :preserve to maintain original case
    (setf (eclector.readtable:readtable-case eclector.readtable:*readtable*) :preserve)

    ;; Read all forms
    (loop
      (handler-case
          (let ((result (eclector.parse-result:read client stream nil :eof)))
            (when (eq result :eof)
              (return))

            (let* ((expr (getf result :expr))
                   (source (getf result :source))
                   (start-pos (car source))
                   (end-pos (cdr source)))

              (multiple-value-bind (start-line start-column)
                  (char-pos-to-line-column start-pos line-starts)
                (multiple-value-bind (end-line end-column)
                    (char-pos-to-line-column end-pos line-starts)

                  ;; Build position map for this form
                  (let ((position-map (build-position-map result line-starts)))

                    ;; Create form object
                    (push (make-instance 'form
                                         :expr expr
                                         :file file
                                         :line start-line
                                         :column start-column
                                         :end-line end-line
                                         :end-column end-column
                                         :source (extract-source text source)
                                         :position-map position-map)
                          forms))))))

        (end-of-file ()
          (return))
        (error (e)
          ;; Try to provide helpful error message
          (let ((pos (file-position stream)))
            (error "Parse error at position ~A: ~A" pos e)))))

    (nreverse forms)))
