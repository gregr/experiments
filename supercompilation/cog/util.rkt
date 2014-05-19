#lang racket
(provide (all-defined-out))
(provide (all-from-out
           gregr-misc/cursor
           gregr-misc/list
           gregr-misc/match
           gregr-misc/maybe
           gregr-misc/monad
           gregr-misc/record
           ))

(require gregr-misc/cursor)
(require gregr-misc/list)
(require gregr-misc/match)
(require gregr-misc/maybe)
(require gregr-misc/monad)
(require gregr-misc/record)
(require racket/stxparam)

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

(define (maybe-fold nothing-fold just-fold maybe)
  (match maybe
    ((nothing) nothing-fold)
    ((just x) (just-fold x))))
(define (maybe-from nothing-from maybe)
  (match maybe
    ((nothing) nothing-from)
    ((just x) x)))

(records either
  (left x)
  (right x))
(define (either-fold left-fold right-fold either)
  (match either
    ((left x) (left-fold x))
    ((right x) (right-fold x))))
(define either-monad (monad
  right
  (lambda (prev next)
    (match prev
      ((left x) (left x))
      ((right x) (next x))))))
(define (either-iterate f arg)
  (match (f arg)
    ((left _)    arg)
    ((right arg) (either-iterate f arg))))

(define (maybe->either left-arg maybe)
  (maybe-fold (left left-arg) right maybe))

(define ((flip proc) x y) (proc y x))

(define (zip xs ys) (map cons xs ys))

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

; TODO:
; for1[-monad]: flip last two params of map[-monad]

; testing

;(display
  ;(do-with (lambda (prev next) (+ 1 (next prev)))
    ;a (+ 3 4)
    ;b (+ a 5)
    ;b))
;(newline)

;> (penv-syntax-add penv-empty 'x 'y)
;(penv (dict (list (cons 'x (just 'y)))) '())
;> (penv-vars-add penv-empty 'z)
;(penv (dict '()) '(z))
;> (penv-vars-add (penv-syntax-add penv-empty 'x 'y) 'z)
;(penv (dict (list (cons 'x (just 'y)))) '(z))
;> (penv-syntax-del (penv-vars-add (penv-syntax-add penv-empty 'x 'y) 'z) 'x)
;(penv (dict (list (cons 'x (nothing)) (cons 'x (just 'y)))) '(z))
;>

;> (list-index (list 1 2 3 4 5 'a 'b 'c) 4)
;(just 3)
;> (list-index (list 1 2 3 4 5 'a 'b 'c) 'b)
;(just 6)
;> (list-index (list 1 2 3 4 5 'a 'b 'c) 'd)
;(nothing)
;> (list-index (list 1 2 3 4 5 'a 'b 'c) 7)
;(nothing)
;> (list-index (list 1 2 3 4 5 'a 'b 'c) 0)
;(nothing)
;> (list-index (list 1 2 3 4 5 'a 'b 'c) 1)
;(just 0)
;>
