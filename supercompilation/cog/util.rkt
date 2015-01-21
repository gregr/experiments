#lang racket
(provide
  pretty-string
  )

(require
  )

(define (pretty-string x) (call-with-output-string (curry pretty-print x)))
