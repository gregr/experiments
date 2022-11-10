#lang racket/base
(require "../mini.rkt" "../tiny.rkt" racket/pretty racket/runtime-path)

(define-runtime-path path.here ".")

(define mini.scm
  (call-with-input-file
    (build-path path.here "../mini.scm")
    (lambda (in)
      (let loop ()
        (let ((x (read in)))
          (if (eof-object? x)
              '()
              (cons x (loop))))))))

(define example.scm
  '((let* ((x `(one ,(+ 1 1) three))
           (y (list x)))
      (cond ((and (pair? x) (or (pair? y) (number? y))) 'ok)
            ((atom=? x y) 'surprising)
            (else (match y
                    ((list (? symbol? u) v) `(got: ,u ,v))))))))

(define test.mini.example (append mini.scm (list example.scm)))

(let ((E.tiny (parse-mini-scm-program test.mini.example)))
  (pretty-write E.tiny)
  (E.tiny?! '() E.tiny)
  (pretty-write `(valid?: ,(E.tiny? '() E.tiny))))
