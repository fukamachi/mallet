(defpackage #:malo/formatter
  (:use #:cl)
  (:local-nicknames
   (#:violation #:malo/violation))
  (:export #:format-text
           #:format-json))
(in-package #:malo/formatter)

(defun format-text (results &key (stream *standard-output*))
  "Format linting RESULTS as human-readable text to STREAM.
RESULTS is an alist of (file . violations)."
  (check-type results list)

  (let ((total-violations 0)
        (files-with-violations 0))

    (dolist (result results)
      (let ((file (car result))
            (violations (cdr result)))
        (when violations
          (incf files-with-violations)
          (incf total-violations (length violations))

          (format stream "~%~A:~%" (namestring file))
          (dolist (v violations)
            (format stream "  ~A:~A:~A ~A: ~A~%"
                    (namestring (violation:violation-file v))
                    (violation:violation-line v)
                    (violation:violation-column v)
                    (violation:violation-severity v)
                    (violation:violation-message v))))))

    (format stream "~%")
    (if (zerop total-violations)
        (format stream "✓ No violations found.~%")
        (format stream "✗ Found ~A violation~:P in ~A file~:P.~%"
                total-violations files-with-violations))

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
