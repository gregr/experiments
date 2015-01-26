#lang racket
(provide
  step-complete
  step-complete-safe
  step-safe
  )

(require
  "substitution.rkt"
  "syntax-abstract.rkt"
  gregr-misc/cursor
  gregr-misc/either
  gregr-misc/list
  gregr-misc/match
  gregr-misc/maybe
  gregr-misc/monad
  gregr-misc/record
  )

(module+ test
  (require rackunit))

(define (step-continuation-downward cterm)
  (match (::.* cterm)
    ((subst sub tm)       (just cterm))
    ((value _)            (nothing))
    ((produce tm)         (step-continuation-downward (::@ cterm '(tm))))
    ((pair-access (bit _) (pair _ _))    (just cterm))
    ((pair-access _ _)    (nothing))
    ((lam-apply proc arg)
     (define (sub-cont choice)
       (step-continuation-downward (::@ cterm (list choice))))
     (maybe-or (sub-cont 'proc) (sub-cont 'arg) (just cterm)))))

(define (step-continuation-upward-with key cterm)
  (match* (key (::.* cterm))
    (('proc (lam-apply _ _)) (just (::@ cterm '(arg))))
    (('arg (lam-apply _ _))  (just cterm))
    ((_   _)                 (step-continuation-upward cterm))))

(define (step-continuation-upward cterm)
  (match (cursor-trail cterm)
    ('()          (nothing))
    ((cons key _) (step-continuation-upward-with key (::^ cterm)))))

(define (step-continuation cterm)
  (maybe-or
    (step-continuation-downward cterm)
    (begin/with-monad
      maybe-monad
      upward <- (step-continuation-upward cterm)
      (step-continuation upward))))

(define (step-execute cterm)
  (maybe-map
    (curry ::=* cterm)
    (match (::.* cterm)
      ((subst sub tm) (just (substitute sub tm)))
      ((value _)      (nothing))
      ((produce _)    (nothing))
      ((pair-access (bit bt) (pair p0 p1))
       (just (value (match bt ((b-0) p0) ((b-1) p1)))))
      ((pair-access _ _) (nothing))
      ((lam-apply (value (lam attr body)) (value v1))
       (just (substitute-lam-apply attr body v1)))
      ((lam-apply (value _) (value _)) (nothing)))))

(define (step-once cterm)
  (begin/with-monad
    maybe-monad
    cterm <- (step-continuation cterm)
    (step-execute cterm)))

(define step-full (curry maybe-iterate step-once))

(define (step term)
  (begin/with-monad
    either-monad
    cterm <- (maybe->either (format "cannot step irreducible term: ~v" term)
                            (step-once (::0 term)))
    (pure (::^*. cterm))))

(define step-complete (compose1 ::^*. step-full ::0))

(define (step-safe term)
  (if (term? term)
    (step term)
    (left (format "cannot step non-term: ~v" term))))
(define (step-complete-safe term)
  (if (term? term)
    (right (step-complete term))
    (left (format "cannot step-complete non-term: ~v" term))))

(module+ test
  (define test-term-0
    (lam-apply
      (value (lam lattr-void
                  (pair-access (bvar 0)
                               (pair (uno)
                                     (pair (bvar 0) (bvar 1))))))
      (value (bvar 0))))
  (define test-term-1
    (lam-apply (value (lam lattr-void test-term-0)) (value (bit (b-1)))))
  (define completed (step-complete test-term-1))
  (check-equal?
    completed
    (value (pair (bit (b-1)) (bit (b-1)))))
  (check-match
    (step test-term-1)
    (right _))
  (check-match
    (step completed)
    (left _))
  )
