#lang racket
(provide
  denote-eval
  noisy-consume
  )

(require
  "substitution.rkt"
  "syntax-abstract.rkt"
  gregr-misc/list
  )

(module+ test
  (require rackunit))

(define (null-consume v) '())
(define (noisy-consume val) (displayln (format "produced: ~v" val)) '())

(define (denote-eval consume term) ((denote consume term) denote-env-empty))

(define (denote consume term)
  (define den-val (curry denote-value consume))
  (match term
    ((subst sub tm) (denote consume (substitute sub tm)))
    ((value val)    (den-val val))
    ((produce tm)   (compose1 consume (denote consume tm)))
    ((pair-access vbit vpair)
     (match-let (((list dbit dpair) (map* den-val vbit vpair)))
       (lambda (env) ((vector-ref (vector car cdr) (dbit env)) (dpair env)))))
    ((lam-apply t0 t1)
     (let ((d0 (denote consume t0)) (d1 (denote consume t1)))
       (lambda (env) ((d0 env) (d1 env)))))))

(define (denote-value consume val)
  (match val
    ((bit vb)   (denote-value-bit vb))
    ((uno)      (lambda (env) '()))
    ((pair l r) (let ((dl (denote-value consume l))
                      (dr (denote-value consume r)))
                  (lambda (env) (cons (dl env) (dr env)))))
    ((bvar idx) (lambda (env) (denote-env-lookup env idx)))
    ((lam attr body) (let ((db (denote consume body)))
                       (lambda (env)
                         (lambda (arg) (db (denote-env-extend env arg))))))))
(define (denote-value-bit vb)
  (match vb
    ((b-0) (lambda (env) 0))
    ((b-1) (lambda (env) 1))))

(define denote-env-empty            '())
(define (denote-env-lookup env idx) (list-ref env idx))
(define (denote-env-extend env v)   (cons v env))

(module+ test
  (define dt
    (denote null-consume (value (pair (bvar 0) (pair (bit (b-0)) (uno))))))
  (check-equal? (dt (denote-env-extend denote-env-empty 'a))
                '(a 0))
  )

(module+ test
  (define test-term-0
    (lam-apply
      (value (lam lattr-void
                  (pair-access (bvar 0)
                               (pair (uno)
                                     (pair (bvar 0) (bvar 1))))))
      (value (bvar 0))))
  (define test-term-1
    (lam-apply (value (lam lattr-void test-term-0)) (value (bit (b-1)))))
  (define completed ((denote null-consume test-term-1) denote-env-empty))
  (check-equal?
    completed
    '(1 . 1)
    ))
