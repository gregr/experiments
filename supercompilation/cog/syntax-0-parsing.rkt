#lang racket
(require "util.rkt")
(require "syntax-abstract.rkt")
(require "data-encoding.rkt")
(require "parsing.rkt")
(provide (all-defined-out))

(define (parse-0 pe form)
  (match form
    ('() (right v-uno))
    ((? integer?) (parse-integer-0 pe form))
    ((? symbol?) (parse-bvar pe form))
    ((cons op rest) (parse-combination pe op form))
    (_ (left (format "cannot parse: ~s" form)))))

(define map-parse-0 (map-parse parse-0))
(define parse-apply-0 (parse-apply map-parse-0))
(define parse-under-0 (parse-under parse-0))

(define parse-produce-0 (parse-apply-0 produce 2))
(define (parse-subst-0 pe form)
  (do either-monad
    _ <- (check-arity 4 form)
    `(,_ ,uses ,lift ,body) = form
    _ <- (if (and (integer? lift) (>= lift 0)) (right (void))
           (left (format "subst 'lift' must be a natural number: ~v" form)))
    uses <- (map-parse-0 pe uses)
    _ <- (if (andmap value? uses) (right (void))
           (left (format "subst 'uses' must be values: ~v" form)))
    uses = (map value-v uses)
    body <- (parse-0 pe body)
    (pure (subst (foldr bvar-use (bvar-lift lift) uses) body))))
(define (parse-lam-apply-0 pe form)
  (do either-monad
    form <- (map-parse-0 pe form)
    (cons proc args) = form
    (pure
      (let loop ((proc proc) (args args))
        (match args
          ('() proc)
          ((cons arg args) (loop (new-lam-apply proc arg) args)))))))
(define (parse-lam-0 pe form)
  (do either-monad
    _ <- (check-arity 3 form)
    `(,_ ,names ,body) = form
    _ <- (if (>= (length names) 1) (right (void))
           (left (format "lam must include at least one parameter: ~v" form)))
    body <- (parse-under-0 pe names body)
    (pure (foldr (lambda (_ body) (value (lam body))) body names))))
(define parse-pair-access-0 (parse-apply-0 new-pair-access 3))
(define (parse-pair-0 pe form)
  (do either-monad
    _ <- (check-arity 3 form)
    `(,_ ,fl ,fr) = form
    l <- (parse-0 pe fl)
    r <- (parse-0 pe fr)
    (new-pair l r)))
(define (parse-as-thunk-0 pe form) (parse-0 pe `(lam (_) ,form)))
(define (parse-tuple-0 pe form)
  (define/match (tuple-value term)
    (((value val)) (right val))
    ((_)           (left (format "tuple elements must be values: ~v" form))))
  (do either-monad
    `(,_ . ,felems) = form
    velems <- (map-parse-0 pe felems)
    elems <- (map-monad either-monad tuple-value velems)
    (pure (value (tuple-encode elems)))))

; derived syntax
(define (parse-if-0-0 pe form)
  (do either-monad
    _ <- (check-arity 4 form)
    `(,_ ,fcnd ,fzero ,fone) = form
    cnd <- (parse-0 pe fcnd)
    zero <- (parse-as-thunk-0 pe fzero)
    one <- (parse-as-thunk-0 pe fone)
    alts <- (new-pair zero one)
    (pure (new-lam-apply (new-pair-access cnd alts) v-uno))))
(define parse-pair-l-0 (parse-apply-0 (curry new-pair-access v-0) 2))
(define parse-pair-r-0 (parse-apply-0 (curry new-pair-access v-1) 2))

; TODO: encode human-friendly numerals and symbols
(define (parse-integer-0 pe form)
  (match form
    (0 (right v-0))
    (1 (right v-1))
    (_ (left (format "expected 0 or 1 but found: ~s" form)))))

(define (parse-fixpoint-0 pe form)
  (do either-monad
    _ <- (check-arity 3 form)
    `(,_ ,names ,body) = form
    _ <- (if (>= (length names) 1) (right (void))
           (left (format "fix must include at least one parameter: ~v" form)))
    body <- (parse-under-0 pe names body)
    proc = (foldr (lambda (_ body) (value (lam body))) body names)
    (pure (new-lam-apply (value Y-combinator) proc))))

(define penv-init-0
  (foldr (lambda (keyval pe) (apply (curry penv-syntax-add pe) keyval))
         penv-empty
         `((,penv-syntax-op-empty ,parse-lam-apply-0)
           (produce ,parse-produce-0)
           (subst ,parse-subst-0)
           (lam ,parse-lam-0)
           (pair ,parse-pair-0)
           (pair-access ,parse-pair-access-0)
           (pair-l ,parse-pair-l-0)
           (pair-r ,parse-pair-r-0)
           (tuple ,parse-tuple-0)
           (if-0 ,parse-if-0-0)
           (fix ,parse-fixpoint-0)
           )))

(define Y-combinator
  (value-v (right-x (parse-0 penv-init-0
                           `(lam (f) ((lam (d) (d d))
                                      (lam (x a) (f (x x) a))))))))
