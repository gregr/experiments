#lang racket
(provide
  substitute
  )

(require
  "syntax-abstract.rkt"
  gregr-misc/list
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
    ((pair l r)   (apply-map* pair (curry substitute-value sub) l r))
    ((bvar index) (substitute-bvar sub index))
    ((lam attr body)
     (lam attr (subst (bvar-use attr (bvar 0)
                                (substitute-substitution (bvar-lift 1) sub))
                      body)))
    (_            tv)))

(define (substitute sub term)
  (define sub-value (curry substitute-value sub))
  (match term
    ((subst inner tm)     (subst (substitute-substitution sub inner) tm))
    ((value tv)           (value (sub-value tv)))
    ((produce tm)         (produce (subst sub tm)))
    ((action-2 act t0 t1) (apply-map* (curry action-2 act)
                                      (curry subst sub) t0 t1))))
