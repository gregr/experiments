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
  (((lam-apply)   (lam body)  _)
   (just (subst (bvar-use v1 (bvar-lift 0)) body)))
  (((pair-access) (bit (b-0)) (pair p0 p1)) (just (value p0)))
  (((pair-access) (bit (b-1)) (pair p0 p1)) (just (value p1)))
  ((_             _           _)            (nothing)))

(define (step-continuation-downward cterm)
  (match (::. cterm)
    ((value _)            (nothing))
    ((produce tm)         (step-continuation-downward (::@ cterm '(tm))))
    ((subst sub tm)       (just cterm))
    ((action-2 act t0 t1)
     (define (sub-cont choice)
       (step-continuation-downward (::@ cterm (list choice))))
     (maybe-or (sub-cont 't0) (sub-cont 't1) (just cterm)))))

(define (step-continuation-upward-with key cterm)
  (match* (key (::. cterm))
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
  (let ((term (::. cterm)))
    (match term
      ((value _)      (nothing))
      ((produce _)    (nothing))
      ((subst sub tm) (just (::= cterm (substitute sub tm))))
      ((action-2 act (value v0) (value v1))
       (maybe-map
         (curry ::= cterm)
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
  (if (or (value? term) (produce? term) (subst? term) (action-2? term))
    (step term)
    (left (format "cannot step non-term: ~v" term))))

(define step-complete (compose1 ::^*. step-full ::0))

; TODO: this may be redundant now that step-complete is performant
(define (step-big term)
  (match term
    ((produce tm)   (produce (step-big tm)))
    ((subst sub tm) (step-big (substitute sub tm)))
    ((action-2 act t0 t1)
     (let ((t0 (step-big t0)) (t1 (step-big t1)))
       (match* (t0 t1)
         (((value v0) (value v1))
          (match (execute-action-2 act v0 v1)
            ((just tm) (step-big tm))
            ((nothing) (action-2 act t0 t1))))
         ((_ _) (action-2 act t0 t1)))))
    (_ term)))

(module+ test
  (define test-term-0
    (action-2 (lam-apply)
              (value (lam (value (pair (bvar 0) (bvar 1)))))
              (value (bvar 0))))
  (define test-term-1
    (action-2 (lam-apply) (value (lam test-term-0)) (value (bit (b-1)))))
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
