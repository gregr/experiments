#lang racket/base
(require "../mini.rkt" racket/pretty racket/runtime-path)

(define-runtime-path path.here ".")

(define mini.scm
  (call-with-input-file
    (build-path path.here "mk.scm")
    (lambda (in)
      (let loop ()
        (let ((x (read in)))
          (if (eof-object? x)
              '()
              (cons x (loop))))))))

(define example.scm ''TODO)

(define test.program (append mini.scm (list example.scm)))

(pretty-write (P.mini->E.tiny test.program))
