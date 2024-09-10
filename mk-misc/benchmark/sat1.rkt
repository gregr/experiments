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
  (let* ((len (+ (apply max 0 (hash-keys x=>v)) 1))
         (v*  (make-vector len 0)))
    (let loop ((i 0))
      (when (< i len)
        (vector-set! v* i (hash-ref x=>v i 0))
        (loop (+ i 1))))
    v*))

;(define (solve1 clause*)
;  (let loop ((c* clause*) (var=>val (hash)))
;    (if (null? c*)
;        (hash->vector var=>val)
;        (let ((c (car c*)) (c* (cdr c*)))
;          (let clause-loop ((literal* c) (var=>val var=>val))
;            (define (get literal)
;              (atom->literal-value literal (hash-ref var=>val (abs literal) 0)))
;            (define (put literal value)
;              (hash-set var=>val (abs literal) (literal->atom-value literal value)))
;            (and (not (null? literal*))
;                 (let ((l (car literal*)) (l* (cdr literal*)))
;                   (case (get l)
;                     ((-1) (clause-loop l* var=>val))
;                     ((1)  (loop c* var=>val))
;                     (else (or (loop c* (put l 1))
;                               (clause-loop l* (put l -1))))))))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Otherwise naive SAT solver with special treatment for 2-literal clauses ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (literal->var lit) (abs lit))

;; This solver improves nqueens performance by almost 4000x, but still cannot solve sudoku
;; problems in a reasonable amount of time.
(define (solve1 clause*)
  (define (continue c* var=>val var=>2c*)
    (define (get var=>val literal)
      (atom->literal-value literal (hash-ref var=>val (literal->var literal) 0)))
    (define (put var=>val l value)
      (let loop ((c*.2     (hash-ref var=>2c* (literal->var l) '()))
                 (var=>val (hash-set var=>val (literal->var l) (literal->atom-value l value))))
        (and var=>val
             (if (null? c*.2)
                 var=>val
                 (let* ((clause (car  c*.2))
                        (l1     (car  clause))
                        (l2     (cadr clause)))
                   (loop (cdr c*.2)
                         (if (= (atom->literal-value l1 (literal->atom-value l value)) 1)
                             var=>val
                             (case (get var=>val l2)
                               ;((-1) #f)
                               ((-1) var=>val)
                               ((1)  var=>val)
                               (else (put var=>val l2 1))))))))))
    (let loop ((c* clause*) (var=>val (hash)))
      (if (null? c*)
          (hash->vector var=>val)
          (let ((c (car c*)) (c* (cdr c*)))
            (let clause-loop ((literal* c) (var=>val var=>val))
              (and (not (null? literal*))
                   (let ((l (car literal*)) (l* (cdr literal*)))
                     (case (get var=>val l)
                       ((-1) (clause-loop l* var=>val))
                       ((1)  (loop c* var=>val))
                       (else (or (let ((var=>val (put var=>val l 1)))
                                   (and var=>val (loop c* var=>val)))
                                 (let ((var=>val (put var=>val l -1)))
                                   (and var=>val (clause-loop l* var=>val)))))))))))))
  (let loop ((c* clause*) (c*.long '()) (c*.2 '()) (var=>val (hash)) (var=>2c* (hash)))
    (define (put literal value)
      (hash-set var=>val (literal->var literal) (literal->atom-value literal value)))
    (if (null? c*)
        (continue (append (reverse c*.long) (reverse c*.2)) var=>val var=>2c*)
        (let ((c (car c*)) (c* (cdr c*)))
          (case (length c)
            ((0)  #f)
            ((1)  (loop c* c*.long c*.2 (put (car c) 1) var=>2c*))
            ((2)  (loop c* c*.long (cons c c*.2) var=>val
                        (hash-update (hash-update var=>2c* (literal->var (car c))
                                                  (lambda (c*) (cons c c*))
                                                  '())
                                     (literal->var (cadr c))
                                     (lambda (c*) (cons (reverse c) c*))
                                     '())))
            (else (loop c* (cons c c*.long) c*.2 var=>val var=>2c*)))))))
