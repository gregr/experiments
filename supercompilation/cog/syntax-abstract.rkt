#lang racket
(provide
  b-0
  b-1

  uno
  bit
  (struct-out pair)
  bvar
  lam
  term-value?

  (struct-out substitution-use)
  (struct-out substitution)

  subst
  produce
  pair-access
  lam-apply
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

(record substitution-use attr v)
(record substitution uses lift)

(records term
  (subst       s t)
  (value       v)
  (produce     t)
  (pair-access index pair)
  (lam-apply   proc arg))
