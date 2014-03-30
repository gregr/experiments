#lang racket
(require "util.rkt")
(require "syntax-abstract.rkt")
(require "substitution.rkt")
(provide (all-defined-out))

(define/match (execute-action-2-explicit act v0 v1)
  (((lam-apply)   (lam body)  _)
   (just (subst (bvar-use v1 (bvar-lift 0)) body)))
  (((pair-access) (bit (b-0)) (pair p0 p1)) (just (value p0)))
  (((pair-access) (bit (b-1)) (pair p0 p1)) (just (value p1)))
  ((_             _           _)            (nothing)))

(define (step term)
  (match term
    ((value _) (left (format "cannot step irreducible term: ~v" term)))
    ((produce tm-0)
     (do either-monad
       tm-1 <- (step tm-0)
       (pure (produce tm-1))))
    ((subst sub tm) (right (substitute-explicit sub tm)))
    ((action-2 act (value v0) (value v1))
     (maybe->either (format "cannot step stuck term: ~v" term)
                    (execute-action-2-explicit act v0 v1)))
    ((action-2 act (value val) t1-0)
     (do either-monad
       t1-1 <- (step t1-0)
       (pure (action-2 act (value val) t1-1))))
    ((action-2 act t0-0 t1)
     (do either-monad
       t0-1 <- (step t0-0)
       (pure (action-2 act t0-1 t1))))))

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
