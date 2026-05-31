(defpackage #:mallet/tests/readme-known-limitations
  (:use #:cl #:rove))
(in-package #:mallet/tests/readme-known-limitations)

(defun readme-content ()
  "Read and return the content of README.md."
  (let ((path (merge-pathnames "README.md"
                               (asdf:system-source-directory "mallet"))))
    (uiop:read-file-string path)))

(defun find-known-limitations-section (content)
  "Extract the README Known limitations or Caveats section from CONTENT.
Returns NIL if neither section exists."
  (loop for heading in '("## Known limitations" "## Caveats")
        for start = (search heading content)
        when start
          return (let ((next-h2 (search (format nil "~%## ")
                                        content
                                        :start2 (1+ start))))
                   (subseq content start (or next-h2 (length content))))))

(defun section-contains-all-p (section &rest needles)
  "Return non-NIL when SECTION contains every string in NEEDLES."
  (and section
       (every (lambda (needle)
                (search needle section))
              needles)))

(defun section-contains-near-p (section anchor needle &key (window 200))
  "Return non-NIL when NEEDLE appears within WINDOW characters of ANCHOR."
  (let ((anchor-pos (and section (search anchor section))))
    (when anchor-pos
      (let ((end (min (length section)
                      (+ anchor-pos (length anchor) window))))
        (search needle section :start2 anchor-pos :end2 end)))))

;;; Gate: README.md documents known inherent limitations so users are not
;;; surprised by parser behavior that Mallet intentionally does not hide or
;;; attempt to resynchronize.

(deftest readme-documents-deep-nesting-limitation
  (testing "README.md contains a Known limitations or Caveats section"
    (let ((section (find-known-limitations-section (readme-content))))
      (ok section
          "README.md must contain a 'Known limitations' or 'Caveats' section")))
  (testing "The section mentions the SBCL stderr banner for deeply nested input"
    (let ((section (find-known-limitations-section (readme-content))))
      (ok (section-contains-all-p section
                                  "deeply nested"
                                  "SBCL"
                                  "Control stack guard"
                                  "stderr")
          "The known limitations section must mention deeply nested input and SBCL stderr output")))
  (testing "The section states the deep-nesting violation is still reported"
    (let ((section (find-known-limitations-section (readme-content))))
      (ok (section-contains-all-p section
                                  "Expression too deeply nested"
                                  "still reported")
          "The known limitations section must state the deep-nesting violation is still reported"))))

(deftest readme-documents-unbalanced-parenthesis-limitation
  (testing "The section explains that an unmatched opening parenthesis stops later analysis"
    (let ((section (find-known-limitations-section (readme-content))))
      (ok (section-contains-all-p section
                                  "unmatched opening"
                                  "stops analysis"
                                  "rest of the file")
          "The known limitations section must say an unmatched opening parenthesis stops analysis of the rest of the file")))
  (testing "The section contrasts stray closing parenthesis recovery"
    (let ((section (find-known-limitations-section (readme-content))))
      ;; Breaker recommendation: require "does not stop" near "analysis" or
      ;; require an affirmative continuation phrase such as "continues analyzing
      ;; later forms".
      (ok (and (section-contains-near-p section
                                        "stray closing"
                                        "does not stop")
               (section-contains-near-p section
                                        "stray closing"
                                        "analysis"))
          "The known limitations section must state a stray closing parenthesis does not stop later analysis"))))
