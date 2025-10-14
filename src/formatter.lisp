(defpackage #:mallet/formatter
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:violation #:mallet/violation))
  (:export #:format-text-file
           #:format-json-file
           #:format-text-summary
           #:format-json-start
           #:format-json-end))
(in-package #:mallet/formatter)

;;; ANSI color codes

(defparameter *color-reset* (format nil "~C[0m" #\Escape))
(defparameter *color-red* (format nil "~C[31m" #\Escape))
(defparameter *color-yellow* (format nil "~C[33m" #\Escape))
(defparameter *color-gray* (format nil "~C[90m" #\Escape))
(defparameter *color-green* (format nil "~C[32m" #\Escape))

(defun use-colors-p (stream)
  "Check if we should use colors for STREAM (only if it's a TTY)."
  (and (member stream (list *standard-output* *error-output*))
       (interactive-stream-p stream)))

(defun colorize (text color stream)
  "Wrap TEXT in COLOR if STREAM supports colors."
  (if (use-colors-p stream)
      (format nil "~A~A~A" color text *color-reset*)
      text))

;;; Streaming formatters (output per-file as processed)

(defun format-text-file (file violations &key (stream *standard-output*))
  "Format VIOLATIONS for a single FILE to STREAM immediately.
Returns a plist of severity counts (:error N :warning M ...)."
  (check-type file pathname)
  (check-type violations list)

  (let ((severity-counts (make-hash-table :test 'eq)))
    (when violations
      (format stream "~%~A~%" (namestring file))
      (dolist (v violations)
        ;; Count this violation
        (let ((severity (violation:violation-severity v)))
          (setf (gethash severity severity-counts 0)
                (1+ (gethash severity severity-counts 0))))

        ;; Format and print violation
        (let* ((line (violation:violation-line v))
               (col (violation:violation-column v))
               (severity (violation:violation-severity v))
               (message (violation:violation-message v))
               (rule (violation:violation-rule v))
               ;; Format location (line:col)
               (location (format nil "~A:~A" line col))
               ;; Format severity with color
               (severity-str (string-downcase (symbol-name severity)))
               (colored-severity (case severity
                                   (:error (colorize severity-str *color-red* stream))
                                   (:warning (colorize severity-str *color-yellow* stream))
                                   (otherwise severity-str)))
               ;; Format rule name with color
               (rule-str (string-downcase (symbol-name rule)))
               (colored-rule (colorize rule-str *color-gray* stream)))
          (format stream "  ~8A ~A~A  ~A  ~A~%"
                  location colored-severity
                  (make-string (max 0 (- 10 (length severity-str)))
                               :initial-element #\Space)
                  message colored-rule))))

    ;; Return counts as plist
    (loop for severity being the hash-keys of severity-counts
          using (hash-value count)
          nconc (list severity count))))

(defun format-text-summary (severity-counts &key (stream *standard-output*))
  "Print summary line based on accumulated SEVERITY-COUNTS plist.
SEVERITY-COUNTS should be a plist like (:error 2 :warning 5)."
  (let* ((error-count (or (getf severity-counts :error) 0))
         (warning-count (or (getf severity-counts :warning) 0))
         (convention-count (or (getf severity-counts :convention) 0))
         (format-count (or (getf severity-counts :format) 0))
         (info-count (or (getf severity-counts :info) 0))
         (total-violations (+ error-count warning-count convention-count
                             format-count info-count)))

    (format stream "~%")
    (if (zerop total-violations)
        (format stream "~A~%"
                (colorize "✓ No problems found." *color-green* stream))
        (let ((problem-word (if (= total-violations 1) "problem" "problems"))
              (parts '()))
          ;; Build severity breakdown
          (when (> error-count 0)
            (push (format nil "~A error~:P" error-count) parts))
          (when (> warning-count 0)
            (push (format nil "~A warning~:P" warning-count) parts))
          (when (> convention-count 0)
            (push (format nil "~A convention~:P" convention-count) parts))
          (when (> format-count 0)
            (push (format nil "~A format~:P" format-count) parts))
          (when (> info-count 0)
            (push (format nil "~A info" info-count) parts))

          (let ((summary (format nil "✗ ~A ~A (~{~A~^, ~})"
                                total-violations
                                problem-word
                                (nreverse parts))))
            (format stream "~A~%"
                    (colorize summary *color-red* stream)))))

    total-violations))

(defun format-json-start (&key (stream *standard-output*))
  "Print JSON array opening bracket."
  (format stream "[~%"))

(defun format-json-end (&key (stream *standard-output*))
  "Print JSON array closing bracket."
  (format stream "~%]~%"))

(defun format-json-file (file violations first-file-p &key (stream *standard-output*))
  "Format VIOLATIONS for a single FILE as JSON to STREAM.
FIRST-FILE-P should be T for the first file, NIL otherwise (controls comma placement).
Returns T if this file had violations, NIL otherwise."
  (check-type file pathname)
  (check-type violations list)

  (when violations
    (unless first-file-p (format stream ",~%"))
    (format stream "  {~%")
    (format stream "    \"file\": ~S,~%" (namestring file))
    (format stream "    \"violations\": [~%")
    (loop for v in violations
          for first-v = t then nil
          do (unless first-v (format stream ",~%"))
             (format stream "      {~%")
             (format stream "        \"rule\": ~S,~%"
                     (string-downcase (symbol-name (violation:violation-rule v))))
             (format stream "        \"severity\": ~S,~%"
                     (string-downcase (symbol-name (violation:violation-severity v))))
             (format stream "        \"line\": ~A,~%"
                     (violation:violation-line v))
             (format stream "        \"column\": ~A,~%"
                     (violation:violation-column v))
             (format stream "        \"message\": ~S~%"
                     (violation:violation-message v))
             (format stream "      }"))
    (format stream "~%    ]~%")
    (format stream "  }")
    t))  ; Return T if violations exist, NIL otherwise
