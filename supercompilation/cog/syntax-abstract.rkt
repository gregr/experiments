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

  pair-access
  lam-apply

  (struct-out value)
  (struct-out produce)
  (struct-out subst)
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
  (pair-access)
  (lam-apply))

(records term
  (value    v)
  (produce  t)
  (subst    s t)
  (action-2 act t0 t1))
