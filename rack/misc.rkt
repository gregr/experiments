#lang racket/base

;; Lexicographic natural numbers for filesystem-friendly ordering
(define (nat->lexicographic-nat n)
  (let loop ((n n) (digits '()))
    (define-values (quo rem) (quotient/remainder n 10))
    (let ((digits (cons rem digits)))
      (if (< 0 quo)
        (loop quo digits)
        (apply string-append
               (map number->string
                    (let loop ((digit-count (length digits)))
                      (cond ((<= 10 digit-count) (cons 9 (loop (- digit-count 9))))
                            ((=   1 digit-count) (cons                    0 digits))
                            (else                (list* (- digit-count 1) 0 digits))))))))))

(define (lexicographic-nat->nat str)
  (let loop ((i 0))
    (if (eqv? (string-ref str i) #\0)
      (string->number (substring str (+ i 1)))
      (loop (+ i 1)))))
