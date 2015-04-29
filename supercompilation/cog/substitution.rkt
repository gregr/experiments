#lang racket
(provide
  substitute
  substitute-full
  substitute-lam-apply
  substitute-value
  substitution-lam-applied
  )

(require
  "syntax-abstract.rkt"
  gregr-misc/list
  gregr-misc/sugar
  )

(def (substitute-lift (substitution uses k) count)
  use-count = (length uses)
  drop-count = (min count use-count)
  lift-count = (max 0 (- count use-count))
  (substitution (drop uses drop-count) (+ k lift-count)))

(def (substitute-substitution outer (substitution inner-uses inner-lift))
  uses-prefix =
  (forl
    (substitution-use attr v) <- inner-uses
    (substitution-use attr (substitute-value outer v)))
  (substitution uses-suffix lift) = (substitute-lift outer inner-lift)
  (substitution (append uses-prefix uses-suffix) lift))

(def (substitute-bvar (substitution uses lift) idx)
  use-count = (length uses)
  (if (< idx use-count)
    (substitution-use-v (list-ref uses idx))
    (bvar (+ lift (- idx use-count)))))

(define (substitute-value sub tv)
  (define sub-lift-1 (substitution '() 1))
  (match tv
    ((pair l r)   (apply-map* pair (curry substitute-value sub) l r))
    ((bvar index) (substitute-bvar sub index))
    ((lam attr body)
     (lets
       (substitution uses lift) = (substitute-substitution sub-lift-1 sub)
       uses = (list* (substitution-use attr (bvar 0)) uses)
       (lam attr (subst (substitution uses lift) body))))
    ((? term-value?) tv)))

(define (substitution-lam-applied attr arg)
  (substitution (list (substitution-use attr arg)) 0))
(define (substitute-lam-apply attr body arg)
  (subst (substitution-lam-applied attr arg) body))

(define (substitute sub term)
  (define sub-value (curry substitute-value sub))
  (match term
    ((subst inner tm)     (subst (substitute-substitution sub inner) tm))
    ((value tv)           (value (sub-value tv)))
    ((produce tm)         (produce (subst sub tm)))
    ((pair-access idx pr) (apply-map* pair-access sub-value idx pr))
    ((lam-apply proc arg) (apply-map* lam-apply (curry subst sub) proc arg))))

(define (substitute-full t/v)
  (match t/v
    ((subst sub tm)       (substitute-full (substitute sub tm)))
    ((value v)            (value (substitute-full v)))
    ((produce tm)         (produce (substitute-full tm)))
    ((pair-access idx pr) (apply-map* pair-access substitute-full idx pr))
    ((lam-apply proc arg) (apply-map* lam-apply substitute-full proc arg))
    ((pair l r)           (apply-map* pair substitute-full l r))
    ((lam attr body)      (lam attr (substitute-full body)))
    ((? term-value?)      t/v)))
