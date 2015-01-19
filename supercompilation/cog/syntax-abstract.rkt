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

  subst
  produce
  pair-access
  action-2
  term?
  (struct-out value)

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
