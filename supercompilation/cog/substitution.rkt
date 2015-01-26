#lang racket
(provide
  substitute
  substitute-lam-apply
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

(define (substitute sub term)
  (define sub-value (curry substitute-value sub))
  (match term
    ((subst inner tm)     (subst (substitute-substitution sub inner) tm))
    ((value tv)           (value (sub-value tv)))
    ((produce tm)         (produce (subst sub tm)))
    ((pair-access idx pr) (apply-map* pair-access sub-value idx pr))
    ((lam-apply proc arg) (apply-map* lam-apply (curry subst sub) proc arg))))

(define (substitute-lam-apply attr body arg)
  (subst (substitution (list (substitution-use attr arg)) 0) body))
