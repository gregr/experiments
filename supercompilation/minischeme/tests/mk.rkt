#lang racket/base
(provide (all-defined-out))
(require racket/include racket/match)

(define (atom=? a b)
  (define (atom? x) (or (null? x) (boolean? x) (number? x) (symbol? x)))
  (and (or (and (atom? a) (atom? b))
           (error "atom=? called with non-atom" a b))
       (eqv? a b)))

(include "mk.scm")
