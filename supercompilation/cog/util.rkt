#lang racket
(provide
  flip
  pretty-string
  )

(require
  )

(define (pretty-string x) (call-with-output-string (curry pretty-print x)))

(define ((flip proc) x y) (proc y x))
