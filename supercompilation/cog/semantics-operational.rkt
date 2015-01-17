#lang racket
(provide (all-defined-out))

(require
  "substitution.rkt"
  "syntax-abstract.rkt"
  "util.rkt"
  )

(module+ test
  (require rackunit))

(define/match (execute-action-2 act v0 v1)
  (((lam-apply)   (lam attr body)  _)
   (just (subst (bvar-use attr v1 (bvar-lift 0)) body)))
  ((_             _           _)            (nothing)))

(define (step-continuation-downward cterm)
  (match (::.* cterm)
    ((subst sub tm)       (just cterm))
    ((value _)            (nothing))
    ((produce tm)         (step-continuation-downward (::@ cterm '(tm))))
    ((pair-access (bit _) (pair _ _))    (just cterm))
    ((pair-access _ _)    (nothing))
    ((action-2 act t0 t1)
     (define (sub-cont choice)
       (step-continuation-downward (::@ cterm (list choice))))
     (maybe-or (sub-cont 't0) (sub-cont 't1) (just cterm)))))

(define (step-continuation-upward-with key cterm)
  (match* (key (::.* cterm))
    (('t0 (action-2 _ _ _)) (just (::@ cterm '(t1))))
    (('t1 (action-2 _ _ _)) (just cterm))
    ((_   _)                (step-continuation-upward cterm))))

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
  (let ((term (::.* cterm)))
    (match term
      ((subst sub tm) (just (::=* cterm (substitute sub tm))))
      ((value _)      (nothing))
      ((produce _)    (nothing))
      ((pair-access (bit bt) (pair p0 p1))
       (just (::=* cterm (value (match bt ((b-0) p0) ((b-1) p1))))))
      ((pair-access _ _) (nothing))
      ((action-2 act (value v0) (value v1))
       (maybe-map
         (curry ::=* cterm)
         (execute-action-2 act v0 v1))))))

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

(define (step-safe term)
  (if (or (value? term) (produce? term) (pair-access? term) (subst? term) (action-2? term))
    (step term)
    (left (format "cannot step non-term: ~v" term))))

(define step-complete (compose1 ::^*. step-full ::0))

(module+ test
  (define test-term-0
    (action-2 (lam-apply)
              (value (lam lattr-void
                          (pair-access (bvar 0)
                                       (pair (uno)
                                             (pair (bvar 0) (bvar 1))))))
              (value (bvar 0))))
  (define test-term-1
    (action-2 (lam-apply) (value (lam lattr-void test-term-0)) (value (bit (b-1)))))
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
