#lang racket
(provide
  dict-empty
  dict-add
  dict-get
  flip
  map-monad
  maybe->either
  pretty-string
  )

(require
  gregr-misc/either
  gregr-misc/maybe
  gregr-misc/monad
  )

(define (pretty-string x) (call-with-output-string (curry pretty-print x)))

(define (map-monad monad proc xs)
  (match xs
    ('() (begin/with-monad monad (pure '())))
    ((cons y ys)
      (begin/with-monad monad
        y0 <- (proc y)
        ys0 <- (map-monad monad proc ys)
        (pure (cons y0 ys0))))))

(define (maybe->either left-arg maybe)
  (maybe-fold (left left-arg) right maybe))

(define ((flip proc) x y) (proc y x))

(define dict-empty (hash))
(define (dict-add dct key val) (dict-set dct key (just val)))
(define (dict-get dct key) (dict-ref dct key (nothing)))
(define (dict-get-default dct key default)
  (maybe-from default (dict-get dct key)))
