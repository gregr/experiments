#lang racket
(require "util.rkt")
(provide (all-defined-out))

(data value-bit
  (b-0 ())
  (b-1 ()))

(data term-value
  (bit  (b))
  (uno  ())
  (pair (l r))
  (bvar (idx))
  (lam  (body)))

(data term-substitution
  (bvar-lift (k))
  (bvar-use  (v s)))

(data term-action-2
  (pair-access ())
  (lam-apply   ()))

(data term
  (value     (v))
  (produce   (t))
  (subst     (s t))
  (action-2  (act t0 t1)))

(define (pair-map f l r) (apply pair (map f (list l r))))
(define (action-2-map f act t0 t1)
  (apply (curry action-2 act) (map f (list t0 t1))))
