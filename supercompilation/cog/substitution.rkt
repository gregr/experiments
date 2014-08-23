#lang racket
(provide (all-defined-out))

(require
  "syntax-abstract.rkt"
  "util.rkt"
  )

(define (substitute-lift substitution count)
  (match substitution
    ((bvar-lift k)    (bvar-lift (+ k count)))
    ((bvar-use attr v sub) (if (equal? 0 count)
                             substitution
                             (substitute-lift sub (- count 1))))))

(define (substitute-substitution outer inner)
  (match inner
    ((bvar-lift k)    (substitute-lift outer k))
    ((bvar-use attr v sub) (bvar-use
                             attr
                             (substitute-value outer v)
                             (substitute-substitution outer sub)))))

(define (substitute-bvar substitution idx)
  (match substitution
    ((bvar-lift k)  (bvar (+ idx k)))
    ((bvar-use attr v s) (if (equal? 0 idx)
                           v
                           (substitute-bvar s (- idx 1))))))

(define (substitute-value sub tv)
  (match tv
    ((pair l r)   (pair-map (curry substitute-value sub) l r))
    ((bvar index) (substitute-bvar sub index))
    ((lam attr body)
     (lam attr (subst (bvar-use attr (bvar 0)
                                (substitute-substitution (bvar-lift 1) sub))
                      body)))
    (_            tv)))

(define (substitute sub term)
  (match term
    ((value tv)           (value (substitute-value sub tv)))
    ((produce tm)         (produce (subst sub tm)))
    ((subst inner tm)     (subst (substitute-substitution sub inner) tm))
    ((action-2 act t0 t1) (action-2-map (curry subst sub) act t0 t1))))
