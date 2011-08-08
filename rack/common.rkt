#lang racket

(provide (all-defined-out))

(define (flip2 fn) (lambda (x y) (fn y x)))
(define (curry2 fn) (lambda (x) (lambda (y) (fn x y))))
