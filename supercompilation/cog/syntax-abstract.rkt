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

  subst-scope-size
  term-frees
  term-frees-safe
  term-value-frees
  )

(require
  gregr-misc/record
  gregr-misc/sugar
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

(define set-empty (set))

(def (subst-scope-size (substitution uses lift)) (- (length uses) lift))

(define (term-frees term (scope 0))
  (match term
    ((subst s t)            (term-frees t (+ scope (subst-scope-size s))))
    ((value       v)        (term-value-frees v scope))
    ((produce     t)        (term-frees t))
    ((pair-access idx pr)   (set-union (term-value-frees idx scope)
                                       (term-value-frees pr scope)))
    ((lam-apply   proc arg) (set-union (term-frees proc scope)
                                       (term-frees arg scope)))))

(define (term-value-frees tv (scope 0))
  (match tv
    ((uno)            set-empty)
    ((bit  b)         set-empty)
    ((pair l r)       (set-union (term-value-frees l scope)
                                 (term-value-frees r scope)))
    ((bvar idx)       (if (< idx scope) set-empty (set (- idx scope))))
    ((lam  attr body) (term-frees body (+ 1 scope)))))

(define (term-frees-safe tv)
  (if (term? tv) (term-frees tv) (term-value-frees tv)))
