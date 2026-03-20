;;; Test fixture for coalton cyclomatic complexity rule
;;;
;;; Default variant is :modified (each match/if/etc = +1 regardless of clause count).
;;; Base complexity is 1. Threshold is 15 (default max).
;;; A define needs 15+ decision points to violate.

;; Simple Coalton function - complexity 1 (OK)
(coalton-toplevel
  (declare simple-fn (Integer -> Integer))
  (define (simple-fn x)
    (+ x 1)))

;; Function with 14 if expressions - complexity 15 (OK, exactly at threshold)
(coalton-toplevel
  (declare borderline (Integer -> Integer))
  (define (borderline cmd)
    (if (= cmd 1) 1 0)
    (if (= cmd 2) 1 0)
    (if (= cmd 3) 1 0)
    (if (= cmd 4) 1 0)
    (if (= cmd 5) 1 0)
    (if (= cmd 6) 1 0)
    (if (= cmd 7) 1 0)
    (if (= cmd 8) 1 0)
    (if (= cmd 9) 1 0)
    (if (= cmd 10) 1 0)
    (if (= cmd 11) 1 0)
    (if (= cmd 12) 1 0)
    (if (= cmd 13) 1 0)
    (if (= cmd 14) 1 0)))

;; High complexity - complexity 16 (1 + 15 if expressions, VIOLATION with default 15)
(coalton-toplevel
  (declare handle-command (Integer -> Integer))
  (define (handle-command cmd)
    (if (= cmd 1) 1 0)
    (if (= cmd 2) 1 0)
    (if (= cmd 3) 1 0)
    (if (= cmd 4) 1 0)
    (if (= cmd 5) 1 0)
    (if (= cmd 6) 1 0)
    (if (= cmd 7) 1 0)
    (if (= cmd 8) 1 0)
    (if (= cmd 9) 1 0)
    (if (= cmd 10) 1 0)
    (if (= cmd 11) 1 0)
    (if (= cmd 12) 1 0)
    (if (= cmd 13) 1 0)
    (if (= cmd 14) 1 0)
    (if (= cmd 15) 1 0)))
