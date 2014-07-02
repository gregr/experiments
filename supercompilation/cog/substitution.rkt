#lang racket
(provide (all-defined-out))

(require
  "syntax-abstract.rkt"
  "util.rkt"
  )

(define (substitute-bvar idx substitution)
  (match substitution
    ((bvar-lift k)  (bvar (+ idx k)))
    ((bvar-use v s) (if (equal? 0 idx) v
                      (substitute-bvar (- idx 1) s)))))

(define (substitution-pop substitution count)
  (match substitution
    ((bvar-lift k)    (bvar-lift (+ k count)))
    ((bvar-use v sub) (if (equal? 0 count) substitution
                        (substitution-pop sub (- count 1))))))

(define (substitute-substitution outer inner)
  (match inner
    ((bvar-lift k)    (substitution-pop outer k))
    ((bvar-use v sub) (bvar-use (substitute-explicit-value outer v)
                                (substitute-substitution outer sub)))))

(define (substitute-explicit-value sub tv)
  (match tv
    ((pair l r)   (pair-map (curry substitute-explicit-value sub) l r))
    ((bvar index) (substitute-bvar index sub))
    ((lam body)   (lam (subst (bvar-use (bvar 0) (substitute-substitution
                                                  (bvar-lift 1) sub))
                              body)))
    (_            tv)))

(define (substitute-explicit sub term)
  (match term
    ((value tv)           (value (substitute-explicit-value sub tv)))
    ((produce tm)         (produce (subst sub tm)))
    ((subst inner tm)     (subst (substitute-substitution sub inner) tm))
    ((action-2 act t0 t1) (action-2-map (curry subst sub) act t0 t1))))
