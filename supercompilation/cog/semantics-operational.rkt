#lang racket
(provide (all-defined-out))

(require
  "substitution.rkt"
  "syntax-abstract.rkt"
  "util.rkt"
  )

(define/match (execute-action-2-explicit act v0 v1)
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
      ((value _)      (left (format "cannot execute value: ~v" term)))
      ((produce _)    (left (format "cannot execute produce ~v" term)))
      ((subst sub tm) (right (::= cterm (substitute-explicit sub tm))))
      ((action-2 act (value v0) (value v1))
       (either-map
         (curry ::= cterm)
         (maybe->either (format "cannot execute stuck term: ~v" term)
                        (execute-action-2-explicit act v0 v1)))))))

; TODO: using cursors, this is now much slower; improve the performance?
(define (step term)
  (begin/with-monad
    either-monad
    cterm <- (maybe->either (format "cannot step irreducible term: ~v" term)
                            (step-continuation (::0 term)))
    cterm <- (step-execute cterm)
    (pure (::^*. cterm))))

(define (step-safe term)
  (if (or (value? term) (produce? term) (subst? term) (action-2? term))
    (step term)
    (left (format "cannot step non-term: ~v" term))))

(define step-complete (curry either-iterate step-safe))

(define (step-big term)
  (match term
    ((produce tm)   (produce (step-big tm)))
    ((subst sub tm) (step-big (substitute-explicit sub tm)))
    ((action-2 act t0 t1)
     (let ((t0 (step-big t0)) (t1 (step-big t1)))
       (match* (t0 t1)
         (((value v0) (value v1))
          (match (execute-action-2-explicit act v0 v1)
            ((just tm) (step-big tm))
            ((nothing) (action-2 act t0 t1))))
         ((_ _) (action-2 act t0 t1)))))
    (_ term)))
