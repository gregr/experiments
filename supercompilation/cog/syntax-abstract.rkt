#lang racket
(require "util.rkt")
(provide (all-defined-out))

(records value-bit
  (b-0)
  (b-1))

(records term-value
  (uno)
  (bit  b)
  (pair l r)
  (bvar idx)
  (lam  body))

(records term-substitution
  (bvar-lift k)
  (bvar-use  v s))

(records term-action-2
  (pair-access)
  (lam-apply))

(records term
  (value    v)
  (produce  t)
  (subst    s t)
  (action-2 act t0 t1))

(define (pair-map f l r) (apply pair (map f (list l r))))
(define (action-2-map f act t0 t1)
  (apply (curry action-2 act) (map f (list t0 t1))))
