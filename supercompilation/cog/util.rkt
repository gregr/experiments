#lang racket
(provide
  (all-defined-out)
  (all-from-out
    gregr-misc/cursor
    gregr-misc/either
    gregr-misc/list
    gregr-misc/match
    gregr-misc/maybe
    gregr-misc/monad
    gregr-misc/record
    )
  )

(require
  gregr-misc/cursor
  gregr-misc/either
  gregr-misc/list
  gregr-misc/match
  gregr-misc/maybe
  gregr-misc/monad
  gregr-misc/record
  )

(define (pretty-string x) (call-with-output-string (curry pretty-print x)))

(define-syntax do
  (syntax-rules ()
    ((_ body ...) (begin/with-monad body ...))))

(define (map-monad monad proc xs)
  (match xs
    ('() (do monad (pure '())))
    ((cons y ys)
      (do monad
        y0 <- (proc y)
        ys0 <- (map-monad monad proc ys)
        (pure (cons y0 ys0))))))

(define (maybe->either left-arg maybe)
  (maybe-fold (left left-arg) right maybe))

(define ((flip proc) x y) (proc y x))

(define (list-index lst key)
  (let loop ((lst lst) (key key) (index 0))
    (match lst
      ('() (nothing))
      ((cons key0 lst)
        (if (equal? key0 key) (just index) (loop lst key (+ index 1)))))))

(define dict-empty (hash))
(define (dict-add dct key val) (dict-set dct key (just val)))
(define (dict-get dct key) (dict-ref dct key (nothing)))
(define (dict-get-default dct key default)
  (maybe-from default (dict-get dct key)))

(define set-empty (set))
(define (set-unions ss)
  (match ss ('() set-empty) (_ (apply set-union ss))))
