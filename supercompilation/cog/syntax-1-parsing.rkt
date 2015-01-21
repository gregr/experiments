#lang racket
(provide
  new-lam-apply-1
  parse-1
  penv-init-1
  )

(require
  "parsing.rkt"
  "syntax-1-bootstrapping.rkt"
  "syntax-abstract.rkt"
  gregr-misc/either
  gregr-misc/monad
  )

(define (parse-1 pe form)
  (match form
    ('() (right uno-1))
    ((? symbol?) (parse-bvar pe form))
    ((cons op rest) (parse-combination pe op form))
    (_ (left (format "cannot parse: ~s" form)))))

(define map-parse-1 (map-parse parse-1))
(define parse-under-1 (parse-under parse-1))

(define (new-lam-apply-1 proc arg)
  (new-lam-apply (new-lam-apply lam-unwrap proc) arg))
(define (new-lam-1 arg-name body)
  (new-lam-apply
    (new-lam-apply lam-wrap (value (_sym arg-name)))
    (value (lam (lattr-name arg-name) body))))

(define (parse-lam-apply-1 pe form)
  (begin/with-monad either-monad
    form <- (map-parse-1 pe form)
    (cons proc args) = form
    (pure
      (let loop ((proc proc) (args args))
        (match args
          ('() proc)
          ((cons arg args) (loop (new-lam-apply-1 proc arg) args)))))))
(define (parse-lam-1 pe form)
  (begin/with-monad either-monad
    _ <- (check-arity 3 form)
    `(,_ ,names ,body) = form
    _ <- (if (>= (length names) 1) (right (void))
           (left (format "lam must include at least one parameter: ~v" form)))
    body <- (parse-under-1 pe names body)
    (pure (foldr new-lam-1 body names))))

(define penv-init-1
  (foldr (lambda (keyval pe) (apply (curry penv-syntax-add pe) keyval))
         penv-empty
         `((,penv-syntax-op-empty ,parse-lam-apply-1)
           (lam ,parse-lam-1)
           ; TODO:
           ; syntax-lam?
           ; macros?
           )))
