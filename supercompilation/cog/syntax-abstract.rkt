#lang racket
(provide
  b-0
  b-1

  uno
  bit
  pair
  bvar
  lam

  bvar-lift
  bvar-use

  lam-apply

  (struct-out subst)
  (struct-out value)
  (struct-out produce)
  (struct-out pair-access)
  (struct-out action-2)

  lattr
  lattr-name
  lattr-void
  )

(require
  gregr-misc/record
  )

(record lattr arg-name arg-syntax lam-syntax)
(define lattr-void (lattr (void) (void) (void)))
(define (lattr-name name) (lattr name (void) (void)))

(records value-bit
  (b-0)
  (b-1))

(records term-value
  (uno)
  (bit  b)
  (pair l r)
  (bvar idx)
  (lam  attr body))

(records term-substitution
  (bvar-lift k)
  (bvar-use  attr v s))

(records term-action-2
  (lam-apply))

(records term
  (subst       s t)
  (value       v)
  (produce     t)
  (pair-access index pair)
  (action-2    act t0 t1))
