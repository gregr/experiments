#lang racket
(provide
  dict-empty
  dict-add
  dict-get
  flip
  pretty-string
  )

(require
  gregr-misc/maybe
  )

(define (pretty-string x) (call-with-output-string (curry pretty-print x)))

(define ((flip proc) x y) (proc y x))

(define dict-empty (hash))
(define (dict-add dct key val) (dict-set dct key (just val)))
(define (dict-get dct key) (dict-ref dct key (nothing)))
(define (dict-get-default dct key default)
  (maybe-from default (dict-get dct key)))
