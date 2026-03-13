(defpackage #:mallet/rules/text
  (:use #:cl)
  (:local-nicknames
   (#:base #:mallet/rules/base)
   (#:violation #:mallet/violation))
  (:export #:line-length-rule
           #:line-length-rule-max-length
           #:trailing-whitespace-rule
           #:no-tabs-rule
           #:final-newline-rule
           #:consecutive-blank-lines-rule
           #:consecutive-blank-lines-rule-max
           #:closing-paren-on-own-line-rule))
(in-package #:mallet/rules/text)

;;; Helper functions

(defun get-line-by-number (text line-number)
  "Extract line LINE-NUMBER from TEXT (1-indexed).
Returns the line content as a string, or NIL if line doesn't exist."
  (check-type text string)
  (check-type line-number (integer 1))

  (let ((start 0))
    ;; Skip (line-number - 1) lines to find start position
    (loop repeat (1- line-number)
          for newline-pos = (position #\Newline text :start start)
          if newline-pos
            do (setf start (1+ newline-pos))
          else
            do (return-from get-line-by-number nil))

    ;; Find end of current line
    (let ((end (position #\Newline text :start start)))
      (when (< start (length text))
        (subseq text start (or end (length text)))))))

;;; Line length rule

(defclass line-length-rule (base:rule)
  ((max-length
    :initarg :max
    :initform 100
    :reader line-length-rule-max-length
    :type (integer 1)
    :documentation "Maximum allowed line length"))
  (:default-initargs
   :name :line-length
   :description "Lines should not exceed the maximum length"
   :severity :info
   :category :format
   :type :text)
  (:documentation "Rule to check line length limits."))

(defmethod base:check-text ((rule line-length-rule) text file)
  "Check that lines in TEXT do not exceed MAX-LENGTH."
  (check-type text string)
  (check-type file pathname)

  (let ((violations '())
        (max-length (line-length-rule-max-length rule)))

    (with-input-from-string (stream text)
      (loop for line-number from 1
            for line = (read-line stream nil nil)
            while line
            do (let ((line-len (length line)))
                 (when (> line-len max-length)
                   (push (make-instance 'violation:violation
                                        :rule :line-length
                                        :file file
                                        :line line-number
                                        :column 0
                                        :severity (base:rule-severity rule)
                                        :message (format nil "Line exceeds maximum length of ~A (~A characters)"
                                                         max-length line-len)
                                        :fix nil)
                         violations)))))

    (nreverse violations)))

;;; Trailing whitespace rule

(defclass trailing-whitespace-rule (base:rule)
  ()
  (:default-initargs
   :name :trailing-whitespace
   :description "Lines should not have trailing whitespace"
   :severity :info
   :category :format
   :type :text)
  (:documentation "Rule to check for trailing whitespace on lines."))

(defmethod base:check-text ((rule trailing-whitespace-rule) text file)
  "Check that lines in TEXT do not have trailing whitespace."
  (check-type text string)
  (check-type file pathname)

  (let ((violations '()))

    (with-input-from-string (stream text)
      (loop for line-number from 1
            for line = (read-line stream nil nil)
            while line do
              ;; Check if line ends with whitespace (space or tab)
              (when (and (plusp (length line))
                         (member (char line (1- (length line))) '(#\Space #\Tab)))
                ;; Find the column where trailing whitespace starts
                ;; by finding the last non-whitespace character
                (let ((trailing-start (position-if-not
                                       (lambda (ch) (member ch '(#\Space #\Tab)))
                                       line
                                       :from-end t)))
                  (let ((fix (violation:make-violation-fix
                              :type :replace-line
                              :line-number line-number
                              :replacement-content (string-right-trim '(#\Space #\Tab) line))))
                    (push (make-instance 'violation:violation
                                         :rule :trailing-whitespace
                                         :file file
                                         :line line-number
                                         ;; Column is right after the last non-whitespace char
                                         ;; If the entire line is whitespace, report column 0
                                         :column (if trailing-start (1+ trailing-start) 0)
                                         :severity (base:rule-severity rule)
                                         :message "Line has trailing whitespace"
                                         :fix fix)
                          violations))))))

    (nreverse violations)))

(defmethod base:make-fix ((rule trailing-whitespace-rule) text file violation)
  "Generate fix for trailing whitespace - replace line with trimmed version."
  (declare (ignore file))
  (let* ((line-number (violation:violation-line violation))
         (line (get-line-by-number text line-number)))
    (when line
      (violation:make-violation-fix
       :type :replace-line
       :line-number line-number
       :replacement-content (string-right-trim '(#\Space #\Tab) line)))))

;;; No tabs rule

(defclass no-tabs-rule (base:rule)
  ()
  (:default-initargs
   :name :no-tabs
   :description "Use spaces instead of tab characters"
   :severity :info
   :category :format
   :type :text)
  (:documentation "Rule to check for tab characters."))

(defmethod base:check-text ((rule no-tabs-rule) text file)
  "Check that TEXT does not contain tab characters."
  (check-type text string)
  (check-type file pathname)

  (let ((violations '()))

    (with-input-from-string (stream text)
      (loop for line-number from 1
            for line = (read-line stream nil nil)
            while line
            do ;; Check if line contains tab character
               (let ((tab-pos (position #\Tab line)))
                 (when tab-pos
                   (push (make-instance 'violation:violation
                                        :rule :no-tabs
                                        :file file
                                        :line line-number
                                        :column tab-pos
                                        :severity (base:rule-severity rule)
                                        :message "Tab character found (use spaces instead)"
                                        :fix nil)
                         violations)))))

    (nreverse violations)))

;;; Final newline rule

(defclass final-newline-rule (base:rule)
  ()
  (:default-initargs
   :name :final-newline
   :description "Files must end with a newline"
   :severity :info
   :category :format
   :type :text)
  (:documentation "Rule to check that files end with a newline."))

(defmethod base:check-text ((rule final-newline-rule) text file)
  "Check that TEXT ends with a newline character."
  (check-type text string)
  (check-type file pathname)

  (let ((violations '()))
    ;; Empty files are OK
    (when (and (plusp (length text))
               (not (char= (char text (1- (length text))) #\Newline)))
      (let ((fix (violation:make-violation-fix
                  :type :append-to-file
                  :appended-content (string #\Newline))))
        (push (make-instance 'violation:violation
                             :rule :final-newline
                             :file file
                             :line (count #\Newline text)
                             :column 0
                             :severity (base:rule-severity rule)
                             :message "File must end with a newline"
                             :fix fix)
              violations)))

    violations))

(defmethod base:make-fix ((rule final-newline-rule) text file violation)
  "Generate fix for missing final newline - append newline to file."
  (declare (ignore text file violation))
  (violation:make-violation-fix
   :type :append-to-file
   :appended-content (string #\Newline)))

;;; Consecutive blank lines rule

(defclass consecutive-blank-lines-rule (base:rule)
  ((max-consecutive
    :initarg :max
    :initform 2
    :reader consecutive-blank-lines-rule-max
    :type (integer 1)
    :documentation "Maximum allowed consecutive blank lines"))
  (:default-initargs
   :name :consecutive-blank-lines
   :description "Limit consecutive blank lines"
   :severity :info
   :category :format
   :type :text)
  (:documentation "Rule to check for too many consecutive blank lines."))

(defmethod base:check-text ((rule consecutive-blank-lines-rule) text file)
  "Check that TEXT does not have too many consecutive blank lines."
  (check-type text string)
  (check-type file pathname)

  (let ((violations '())
        (blank-count 0)
        (max-consecutive (consecutive-blank-lines-rule-max rule))
        (violation-line nil))

    (with-input-from-string (stream text)
      (loop for line-number from 1
            for line = (read-line stream nil nil)
            while line
            do ;; A line is blank if it's empty or contains only whitespace
               (cond
                 ((or (zerop (length line))
                      (every (lambda (ch) (member ch '(#\Space #\Tab))) line))
                  (incf blank-count)
                  (when (and (null violation-line)
                             (> blank-count max-consecutive))
                    ;; Mark where the violation starts (first excessive blank line)
                    (setf violation-line line-number)))
                 (t
                  ;; Non-blank line, check if we had a violation
                  (when violation-line
                    (let* ((excess-count (- blank-count max-consecutive))
                           (fix (when (plusp excess-count)
                                  (violation:make-violation-fix
                                   :type :delete-lines
                                   :start-line violation-line
                                   :end-line (+ violation-line excess-count -1)))))
                      (push (make-instance 'violation:violation
                                           :rule :consecutive-blank-lines
                                           :file file
                                           :line violation-line
                                           :column 0
                                           :severity (base:rule-severity rule)
                                           :message (format nil "More than ~A consecutive blank lines" max-consecutive)
                                           :fix fix)
                            violations)
                      (setf violation-line nil)))
                  ;; Reset counter
                  (setf blank-count 0)))))

    (nreverse violations)))

(defmethod base:make-fix ((rule consecutive-blank-lines-rule) text file violation)
  "Generate fix for excessive blank lines - delete the excess lines."
  (declare (ignore file))
  (let ((start-line (violation:violation-line violation))
        (max-consecutive (consecutive-blank-lines-rule-max rule)))
    ;; Count consecutive blank lines starting from start-line
    (with-input-from-string (stream text)
      ;; Skip to start-line
      (loop repeat (1- start-line) do (read-line stream nil nil))

      ;; Count consecutive blank lines
      (let ((blank-count 0))
        (loop for line = (read-line stream nil nil)
              while (and line
                         (or (zerop (length line))
                             (every (lambda (ch) (member ch '(#\Space #\Tab))) line)))
              do (incf blank-count))

        ;; Delete excess blank lines (keep max-consecutive, delete the rest)
        (let ((excess-count (- blank-count max-consecutive)))
          (when (plusp excess-count)
            (violation:make-violation-fix
             :type :delete-lines
             :start-line start-line
             :end-line (+ start-line excess-count -1))))))))

;;; Closing paren on own line rule

(defclass closing-paren-on-own-line-rule (base:rule)
  ()
  (:default-initargs
   :name :closing-paren-on-own-line
   :description "Closing parentheses should not appear alone on a line; place them at the end of the last expression"
   :severity :info
   :category :format
   :type :text)
  (:documentation "Rule to detect lines consisting only of closing parentheses.
In idiomatic Common Lisp, closing parentheses follow the last expression on the
same line rather than appearing on their own line (unlike Algol-family languages)."))

(defun closing-parens-only-p (line)
  "Return the column of the first closing paren if LINE contains only closing
parens (and surrounding whitespace), NIL otherwise.

A line matches if, after stripping leading and trailing whitespace, it consists
entirely of one or more ')' characters."
  (let* ((trimmed (string-trim '(#\Space #\Tab) line))
         (len (length trimmed)))
    (when (and (plusp len)
               (every (lambda (ch) (char= ch #\))) trimmed))
      ;; Return column of the first ')' (0-based position in original line)
      (position #\) line))))

(defun count-string-transitions (line in-string-p)
  "Count unescaped double-quote characters in LINE to track string literal state.
Returns (values new-in-string-p) after processing the entire line.

IN-STRING-P is T if we are already inside a string literal before this line.
Handles escape sequences: backslash before a quote does not toggle string state."
  (let ((in-string in-string-p)
        (escape-next nil))
    (loop for ch across line
          do (cond
               (escape-next
                (setf escape-next nil))
               ((char= ch #\\)
                (setf escape-next t))
               ((char= ch #\")
                (setf in-string (not in-string)))))
    in-string))

(defun line-ends-with-comment-p (line)
  "Return T if LINE ends with a comment (has a semicolon outside of strings).
Tracks string state to avoid matching semicolons inside string literals."
  (let ((in-string nil)
        (escape-next nil)
        (found-comment nil))
    (loop for ch across line
          do (cond
               (escape-next
                (setf escape-next nil))
               ((char= ch #\\)
                (setf escape-next t))
               ((char= ch #\")
                (setf in-string (not in-string)))
               ((and (char= ch #\;) (not in-string))
                (setf found-comment t)
                (return))))
    found-comment))

(defmethod base:check-text ((rule closing-paren-on-own-line-rule) text file)
  "Check that no line consists entirely of closing parentheses.
Tracks string literal state across lines to avoid false positives inside
multi-line strings.  Skips violations when the previous non-blank line ends
with a comment, since the closing paren cannot be appended to such a line."
  (check-type text string)
  (check-type file pathname)

  (let ((violations '())
        (in-string nil)
        (prev-line nil))

    (with-input-from-string (stream text)
      (loop for line-number from 1
            for line = (read-line stream nil nil)
            while line
            do (let ((col (and (not in-string) (closing-parens-only-p line))))
                 (when (and col
                            (not (and prev-line (line-ends-with-comment-p prev-line))))
                   (push (make-instance 'violation:violation
                                        :rule :closing-paren-on-own-line
                                        :file file
                                        :line line-number
                                        :column col
                                        :severity (base:rule-severity rule)
                                        :message "Closing parenthesis on its own line; move to end of previous expression"
                                        :fix nil)
                         violations))
                 ;; Update string state after checking, so we use the state
                 ;; from the START of this line (not end) for the violation check
                 (setf in-string (count-string-transitions line in-string))
                 ;; Track previous non-blank line for comment exception
                 (unless (zerop (length (string-trim '(#\Space #\Tab) line)))
                   (setf prev-line line)))))

    (nreverse violations)))
