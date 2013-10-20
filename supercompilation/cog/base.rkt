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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; operational semantics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pair-map f l r) (apply pair (map f (list l r))))
(define (action-2-map f act t0 t1)
  (apply (curry action-2 act) (map f (list t0 t1))))

(define (lift-bvars-value idx val)
  (match val
    ((pair l r)   (pair-map (curry lift-bvars-value idx) l r))
    ((bvar index) (if (< index idx) (bvar index) (bvar (+ index 1))))
    ((lam body)   (lam (lift-bvars (+ idx 1) body)))
    (_            val)))
(define (lift-bvars idx term)
  (match term
    ((value val)          (value (lift-bvars-value idx val)))
    ((action-2 act t0 t1) (action-2-map (curry lift-bvars idx) act t0 t1))))

(define (substitute-value idx val tv)
  (match tv
    ((pair l r)   (pair-map (curry substitute-value idx val) l r))
    ((bvar index) (if (< index idx) (bvar index)
                    (if (> index idx) (bvar (- index 1)) val)))
    ((lam body)   (lam (substitute (+ idx 1) (lift-bvars-value 0 val) body)))
    (_            tv)))
(define (substitute idx val term)
  (match term
    ((value tv)           (value (substitute-value idx val tv)))
    ((action-2 act t0 t1) (action-2-map
                            (curry substitute idx val) act t0 t1))))

(define/match (execute-action-2 act v0 v1)
  (((lam-apply)   (lam body)  _)            (just (substitute 0 v1 body)))
  (((pair-access) (bit (b-0)) (pair p0 p1)) (just (value p0)))
  (((pair-access) (bit (b-1)) (pair p0 p1)) (just (value p1)))
  ((_             _           _)            (nothing)))

(define (step term)
  (match term
    ((value _) (left (format "cannot step irreducible term: ~v" term)))
    ((action-2 act (value v0) (value v1))
     (maybe->either (format "cannot step stuck term: ~v" term)
                    (execute-action-2 act v0 v1)))
    ((action-2 act (value val) t1-0)
     (do either-monad
       t1-1 <- (step t1-0)
       (pure (action-2 act val t1-1))))
    ((action-2 act t0-0 t1)
     (do either-monad
       t0-1 <- (step t0-0)
       (pure (action-2 act t0-1 t1))))))
