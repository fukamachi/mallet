(defpackage #:malo/formatter
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:violation #:malo/violation))
  (:export #:format-text
           #:format-json))
(in-package #:malo/formatter)

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

(defun format-text (results &key (stream *standard-output*))
  "Format linting RESULTS as human-readable text to STREAM.
RESULTS is an alist of (file . violations)."
  (check-type results list)

  (let ((severity-counts (make-hash-table :test 'eq))
        (total-violations 0))

    ;; Count violations by severity
    (dolist (result results)
      (let ((violations (cdr result)))
        (dolist (v violations)
          (incf total-violations)
          (let ((severity (violation:violation-severity v)))
            (setf (gethash severity severity-counts 0)
                  (1+ (gethash severity severity-counts 0)))))))

    ;; Print violations grouped by file
    (dolist (result results)
      (let ((file (car result))
            (violations (cdr result)))
        (when violations
          (format stream "~%~A~%" (namestring file))
          (dolist (v violations)
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
              (format stream "  ~8A ~A~A ~A  ~A~%"
                      location colored-severity
                      (make-string (max 0 (- 10 (length severity-str)))
                                   :initial-element #\Space)
                      message colored-rule))))))

    ;; Print summary
    (format stream "~%")
    (if (zerop total-violations)
        (format stream "~A~%"
                (colorize "✓ No problems found." *color-green* stream))
        (let* ((error-count (gethash :error severity-counts 0))
               (warning-count (gethash :warning severity-counts 0))
               (convention-count (gethash :convention severity-counts 0))
               (format-count (gethash :format severity-counts 0))
               (info-count (gethash :info severity-counts 0))
               (problem-word (if (= total-violations 1) "problem" "problems"))
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

(defun format-json (results &key (stream *standard-output*))
  "Format linting RESULTS as JSON to STREAM.
RESULTS is an alist of (file . violations)."
  (check-type results list)

  ;; Simple JSON output without dependencies for now
  (format stream "[~%")
  (loop for (file . violations) in results
        for first-file = t then nil
        when violations do
          (unless first-file (format stream ",~%"))
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
          (format stream "  }"))
  (format stream "~%]~%"))
