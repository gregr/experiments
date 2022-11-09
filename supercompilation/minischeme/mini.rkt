#lang racket/base
(provide DL.mini->E.tiny E.mini->E.tiny)
(require "tiny.rkt" racket/include racket/match)

(define (atom=? a b)
  (define (atom? x) (or (null? x) (boolean? x) (number? x) (symbol? x)))
  (and (or (and (atom? a) (atom? b))
           (error "atom=? called with non-atom" a b))
       (eqv? a b)))

(include "mini.scm")
