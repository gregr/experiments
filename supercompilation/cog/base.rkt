#lang racket

(require "util.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; abstract syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(data value-bit
  (b-0 ())
  (b-1 ()))

(data term-value
  (bit  (b))
  (uno  ())
  (pair (l r))
  (bvar (idx))
  (lam  (body)))

(data term-action-2
  (pair-access ())
  (lam-apply   ()))

(data term
  (value     (v))
  (action-2  (act t0 t1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; denotational semantics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (denote-eval term) ((denote term) denote-env-empty))

(define (denote term)
  (match term
    ((value val) (denote-value val))
    ((action-2 act t0 t1)
     (let ((d0 (denote t0)) (d1 (denote t1)) (dact (denote-action-2 act)))
       (lambda (env) (dact (d0 env) (d1 env)))))))
(define (denote-action-2 act)
  (match act
    ((pair-access) (lambda (vbit vpair)
                     ((vector-ref (vector car cdr) vbit) vpair)))
    ((lam-apply)   (lambda (vproc varg) (vproc varg)))))
(define (denote-value val)
  (match val
    ((bit vb)   (denote-value-bit vb))
    ((uno)      (lambda (env) '()))
    ((pair l r) (let ((dl (denote-value l)) (dr (denote-value r)))
                  (lambda (env) (cons (dl env) (dr env)))))
    ((bvar idx) (lambda (env) (denote-env-lookup env idx)))
    ((lam body) (let ((db (denote body)))
                  (lambda (env)
                    (lambda (arg) (db (denote-env-extend env arg))))))))
(define (denote-value-bit vb)
  (match vb
    ((b-0) (lambda (env) 0))
    ((b-1) (lambda (env) 1))))

(define denote-env-empty            '())
(define (denote-env-lookup env idx) (list-ref env idx))
(define (denote-env-extend env v)   (cons v env))
