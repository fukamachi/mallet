(defpackage #:malvolio/rules/text
  (:use #:cl)
  (:local-nicknames
   (#:base #:malvolio/rules/base)
   (#:violation #:malvolio/violation))
  (:export #:line-length-rule
           #:trailing-whitespace-rule
           #:no-tabs-rule
           #:final-newline-rule
           #:consecutive-blank-lines-rule))
(in-package #:malvolio/rules/text)

;;; Line length rule

(defclass line-length-rule (base:rule)
  ((max-length
    :initarg :max-length
    :initform 80
    :reader line-length-rule-max-length
    :type (integer 1)
    :documentation "Maximum allowed line length"))
  (:default-initargs
   :name :line-length
   :description "Lines should not exceed the maximum length"
   :severity :warning
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
   :severity :warning
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
                (push (make-instance 'violation:violation
                                     :rule :trailing-whitespace
                                     :file file
                                     :line line-number
                                     :column 0
                                     :severity (base:rule-severity rule)
                                     :message "Line has trailing whitespace"
                                     :fix nil)
                      violations))))

    (nreverse violations)))

;;; No tabs rule

(defclass no-tabs-rule (base:rule)
  ()
  (:default-initargs
   :name :no-tabs
   :description "Use spaces instead of tab characters"
   :severity :warning
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
              (when (find #\Tab line)
                (push (make-instance 'violation:violation
                                     :rule :no-tabs
                                     :file file
                                     :line line-number
                                     :column 0
                                     :severity (base:rule-severity rule)
                                     :message "Tab character found (use spaces instead)"
                                     :fix nil)
                      violations))))

    (nreverse violations)))

;;; Final newline rule

(defclass final-newline-rule (base:rule)
  ()
  (:default-initargs
   :name :final-newline
   :description "Files must end with a newline"
   :severity :warning
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
      (push (make-instance 'violation:violation
                           :rule :final-newline
                           :file file
                           :line (count #\Newline text)
                           :column 0
                           :severity (base:rule-severity rule)
                           :message "File must end with a newline"
                           :fix nil)
            violations))

    violations))

;;; Consecutive blank lines rule

(defclass consecutive-blank-lines-rule (base:rule)
  ((max-consecutive
    :initarg :max-consecutive
    :initform 2
    :reader consecutive-blank-lines-rule-max
    :type (integer 1)
    :documentation "Maximum allowed consecutive blank lines"))
  (:default-initargs
   :name :consecutive-blank-lines
   :description "Limit consecutive blank lines"
   :severity :warning
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
              (if (or (zerop (length line))
                      (every (lambda (ch) (member ch '(#\Space #\Tab))) line))
                  (progn
                    (incf blank-count)
                    (when (and (null violation-line)
                               (> blank-count max-consecutive))
                      ;; Mark where the violation starts (first excessive blank line)
                      (setf violation-line line-number)))
                  (progn
                    ;; Non-blank line, check if we had a violation
                    (when violation-line
                      (push (make-instance 'violation:violation
                                           :rule :consecutive-blank-lines
                                           :file file
                                           :line violation-line
                                           :column 0
                                           :severity (base:rule-severity rule)
                                           :message (format nil "More than ~A consecutive blank lines" max-consecutive)
                                           :fix nil)
                            violations)
                      (setf violation-line nil))
                    ;; Reset counter
                    (setf blank-count 0)))))

    (nreverse violations)))
