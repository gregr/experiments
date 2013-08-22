#lang racket

(require racket/stxparam)

(provide (all-defined-out))

(define-syntax variant
  (syntax-rules ()
    ((_) (void))
    ((_ (name fields) more ...)
      (begin (define-struct name fields #:transparent) (variant more ...)))))
(define-syntax data
  (syntax-rules ()
    ((_ name var ...) (variant var ...))))
; TODO use data name to store properties
; for example, collect typeclass/multimethod instances
;   allow do notation to refer only to data name, rather than name-monad

(data monad (monad (pure bind)))

(define-syntax do-with
  (syntax-rules (<-)
    ((_ combine pat <- stmt rest ...)
      (combine stmt (match-lambda (pat (do-with combine rest ...)))))
    ((_ combine pat = stmt rest ...)
      (match-let ((pat stmt)) (do-with combine rest ...)))
    ((_ combine stmt) stmt)))
(define-syntax-parameter pure
  (lambda (stx) (raise-syntax-error #f "must be used inside 'do'" stx)))
(define-syntax do
  (syntax-rules ()
    ((_ monad stmt ...)
      (syntax-parameterize
        ((pure (syntax-rules () ((_ ex) ((monad-pure monad) ex)))))
          (do-with (monad-bind monad) stmt ...)))))

(define (map-monad monad proc xs)
  (match xs
    ('() (right '()))
    ((cons y ys)
      (do monad
        y0 <- (proc y)
        ys0 <- (map-monad monad proc ys)
        (pure (cons y0 ys0))))))

(data maybe
  (nothing ())
  (just (x)))
; TODO: automatically generate folds from data definitions
(define (maybe-fold nothing-fold just-fold maybe)
  (match maybe
    ((nothing) nothing-fold)
    ((just x) (just-fold x))))
(define (maybe-from nothing-from maybe)
  (match maybe
    ((nothing) nothing-from)
    ((just x) x)))
(define maybe-monad (monad
  just
  (lambda (prev next)
    (match prev
      ((nothing) (nothing))
      ((just x) (next x))))))

(data either
  (left (x))
  (right (x)))
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

(define (maybe->either left-arg maybe)
  (maybe-fold (left left-arg) right maybe))

(define ((flip proc) x y) (proc y x))

(define (list-index lst key)
  (let loop ((lst lst) (key key) (index 0))
    (match lst
      ('() (nothing))
      ((cons key0 lst)
        (if (equal? key0 key) (just index) (loop lst key (+ index 1)))))))

(define alist-empty '())
(define (alist-build keys vals) (map cons keys vals))
(define (alist-add alst key val) (cons (cons key val) alst))
(define (alist-get alst key)
  (match alst
    ('() (nothing))
    ((cons (cons key0 val) alst)
      (if (equal? key0 key) (just val)
        (alist-get alst key)))))
(define (alist-get-default alst key default)
  (match (alist-get alst key)
    ((nothing) default)
    ((just x) x)))

(define dict-empty (hash))
(define (dict-add dct key val) (hash-set dct key (just val)))
(define (dict-del dct key) (hash-remove dct key))
(define (dict-get dct key) (hash-ref dct key (nothing)))
(define (dict-get-default dct key default)
  (match (dict-get dct key)
    ((nothing) default)
    ((just x) x)))

; TODO:
; lenses?
; for1[-monad]: flip last two params of map[-monad]

; testing
;(display
  ;(do-with (lambda (prev next) (+ 1 (next prev)))
    ;a (+ 3 4)
    ;b (+ a 5)
    ;b))
;(newline)

;> (alist-add alist-empty 'x 4)
;(list (cons 'x (just 4)))
;> (alist-add (alist-add alist-empty 'x 4) 'y 3)
;(list (cons 'y (just 3)) (cons 'x (just 4)))
;> (alist-del (alist-add (alist-add alist-empty 'x 4) 'y 3) 'x)
;(list (cons 'x (nothing)) (cons 'y (just 3)) (cons 'x (just 4)))
;> (alist-get (alist-del (alist-add (alist-add alist-empty 'x 4) 'y 3) 'x) 'y)
;(just 3)
;> (alist-get (alist-del (alist-add (alist-add alist-empty 'x 4) 'y 3) 'x) 'x)
;(nothing)
;> (alist-get (alist-del (alist-add (alist-add alist-empty 'x 4) 'y 3) 'x) 'z)
;(nothing)

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
