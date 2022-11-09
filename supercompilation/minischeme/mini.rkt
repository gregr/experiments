#lang racket/base
(provide parse-definition*-expression parse-expression
         env.empty env-extend:base
         env-extend:base-definition
         env-extend:base-expression
         env-extend:primitive)
(require "tiny.rkt" (rename-in racket/base (error rkt:error)) racket/include racket/match)

(define (atom=? a b)
  (define (atom? x) (or (null? x) (boolean? x) (number? x) (symbol? x)))
  (and (or (and (atom? a) (atom? b))
           (rkt:error "atom=? called with non-atom" a b))
       (eqv? a b)))

(include "mini.scm")
