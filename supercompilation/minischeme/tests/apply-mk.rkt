#lang racket/base
(require "mk.rkt" "../mini.rkt" "../tiny.rkt" racket/pretty racket/runtime-path)

(define-runtime-path path.here ".")

(define (read* in)
  (let loop ()
    (let ((x (read in)))
      (if (eof-object? x)
          '()
          (cons x (loop))))))

(define mini.scm (call-with-input-file (build-path path.here "../mini.scm") read*))
(define mk.scm   (call-with-input-file (build-path path.here "mk.scm")      read*))

(define test.mk.lib (append mini.scm (append mk.scm (list ''TODO))))

(define test.mk.example
  '((define-relation (appendo x* y* x*y*)
      (conde
        ((== x* '()) (== x*y* y*))
        ((fresh (a a* a*y*)
           (== x*   (cons a a*))
           (== x*y* (cons a a*y*))
           (appendo a* y* a*y*)))))
    (run* (x* y*) (appendo x* y* '(a b c d e)))))

(let ((E.tiny (parse-mini-scm-program test.mk.lib)))
  ;(pretty-write E.tiny)
  (E.tiny?! '() E.tiny)
  ;(pretty-write `(valid?: ,(E.tiny? '() E.tiny)))
  )

(let ((E.tiny (parse-mk-program test.mk.example)))
  (pretty-write E.tiny)
  (E.tiny?! '() E.tiny)
  (pretty-write `(valid?: ,(E.tiny? '() E.tiny))))
