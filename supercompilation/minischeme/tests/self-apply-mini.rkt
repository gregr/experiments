#lang racket/base
(require "../mini.rkt" racket/pretty)

(define mini.scm
  (call-with-input-file
    "mini.rkt"
    (lambda (in)
      ;; Skip of Racket-specific prelude.
      ;; TODO: have mini.rkt (include "mini.scm") instead, and also read that here.
      (read-line in)
      (read-line in)
      (read-line in)
      (let loop ()
        (let ((x (read in)))
          (if (eof-object? x)
              '()
              (cons x (loop))))))))

(define example.scm
  '((let* ((x `(one ,(+ 1 1) three))
           (y (list x)))
      (cond ((and (pair? x) (or (pair? y) (number? y))) 'ok)
            ((eqv? x y) 'surprising)
            (else (match y
                    ((list (? symbol? u) v) `(got: ,u ,v))))))))

(define test.program (append mini.scm (list example.scm)))

(pretty-write (P.mini->E.tiny test.program))
