#lang racket/base
(provide
  s->list
  s-take
  s-append
  s-map
  solve1
  solve
  )

(define (s->list s) (s-take #f s))

(define (s-take n s)
  (cond
    ((or (eq? n 0) (null? s)) '())
    ((procedure? s)           (s-take n (s)))
    (else                     (cons (car s) (s-take (and n (- n 1)) (cdr s))))))

(define (s-append s1 s2)
  (let loop ((s1 s1))
    (cond
      ((null?      s1) s2)
      ((procedure? s1) (lambda () (loop (s1))))
      (else            (cons (car s1) (loop (cdr s1)))))))

(define (s-map f s)
  (let loop ((s s))
    (cond
      ((null?      s) '())
      ((procedure? s) (lambda () (loop (s))))
      (else           (cons (f (car s)) (loop (cdr s)))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Naive SAT solver ;;;
;;;;;;;;;;;;;;;;;;;;;;;;
(define (atom->literal-value literal atom-value)
  (* (if (< literal 0) -1 1) atom-value))
(define (literal->atom-value literal literal-value)
  ;; Coincidentally the same as atom->literal-value in this implementation
  (* (if (< literal 0) -1 1) literal-value))

(define (hash->vector x=>v)
  (let* ((len (+ (apply max (hash-keys x=>v)) 1))
         (v*  (make-vector len 0)))
    (let loop ((i 0))
      (when (< i len)
        (vector-set! v* i (hash-ref x=>v i 0))
        (loop (+ i 1))))
    v*))

(define (solve1 clause*)
  (let loop ((c* clause*) (var=>val (hash)))
    (if (null? c*)
        (hash->vector var=>val)
        (let ((c (car c*)) (c* (cdr c*)))
          (let clause-loop ((literal* c) (var=>val var=>val))
            (define (get literal)
              (atom->literal-value literal (hash-ref var=>val (abs literal) 0)))
            (define (put literal value)
              (hash-set var=>val (abs literal) (literal->atom-value literal value)))
            (and (not (null? literal*))
                 (let ((l (car literal*)) (l* (cdr literal*)))
                   (case (get l)
                     ((-1) (clause-loop l* var=>val))
                     ((1)  (loop c* var=>val))
                     (else (or (loop c* (put l 1))
                               (clause-loop l* (put l -1))))))))))))

(define (solve clause*)
  (let loop ((c* clause*) (var=>val (hash)))
    (if (null? c*)
        (list (hash->vector var=>val))
        (let ((c (car c*)) (c* (cdr c*)))
          (let clause-loop ((literal* c) (var=>val var=>val))
            (define (get literal)
              (atom->literal-value literal (hash-ref var=>val (abs literal) 0)))
            (define (put literal value)
              (hash-set var=>val (abs literal) (literal->atom-value literal value)))
            (if (null? literal*)
                '()
                (let ((l (car literal*)) (l* (cdr literal*)))
                  (case (get l)
                    ((-1) (clause-loop l* var=>val))
                    ((1)  (loop c* var=>val))
                    (else (s-append (loop c* (put l 1))
                                    (lambda () (clause-loop l* (put l -1)))))))))))))
