#lang racket
(provide (all-defined-out))

(require
  "substitution.rkt"
  "syntax-abstract.rkt"
  "util.rkt"
  )

(module+ test
  (require rackunit))

(define (null-consume v) '())
(define (noisy-consume val) (displayln (format "produced: ~v" val)) '())

(define (denote-eval consume term) ((denote consume term) denote-env-empty))

(define (denote consume term)
  (match term
    ((value val)    (denote-value consume val))
    ((produce tm)   (compose1 consume (denote consume tm)))
    ((subst sub tm) (denote consume (substitute sub tm)))
    ((action-2 act t0 t1)
     (let ((d0 (denote consume t0))
           (d1 (denote consume t1))
           (dact (denote-action-2 act)))
       (lambda (env) (dact (d0 env) (d1 env)))))))
(define (denote-action-2 act)
  (match act
    ((pair-access) (lambda (vbit vpair)
                     ((vector-ref (vector car cdr) vbit) vpair)))
    ((lam-apply)   (lambda (vproc varg) (vproc varg)))))
(define (denote-value consume val)
  (match val
    ((bit vb)   (denote-value-bit vb))
    ((uno)      (lambda (env) '()))
    ((pair l r) (let ((dl (denote-value consume l))
                      (dr (denote-value consume r)))
                  (lambda (env) (cons (dl env) (dr env)))))
    ((bvar idx) (lambda (env) (denote-env-lookup env idx)))
    ((lam body) (let ((db (denote consume body)))
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
