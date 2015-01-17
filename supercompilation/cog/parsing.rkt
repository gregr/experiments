#lang racket
(provide (all-defined-out))

(require
  "syntax-abstract.rkt"
  "util.rkt"
  )

(record penv syntax vars)
(define penv-empty (penv dict-empty '()))
(define/destruct (penv-syntax-add (penv syntax vars) name op)
  (penv (dict-add syntax name op) vars))
(define/destruct (penv-syntax-del (penv syntax vars) name)
  (penv (dict-remove syntax name) vars))
(define (penv-syntax-get pe name) (dict-get (penv-syntax pe) name))
(define (penv-syntax-rename pe old new)
  (define (check-vars name msg)
    (match (penv-vars-get pe name)
      ((nothing) (right '()))
      ((just _) (left msg))))
  (do either-monad
    _ <- (check-vars old "cannot rename non-keyword")
    _ <- (check-vars new "rename-target already bound as a non-keyword")
    syn-old <- (maybe->either "cannot rename non-existent keyword"
                              (penv-syntax-get pe old))
    pe0 = (penv-syntax-del pe old)
    (pure (penv-syntax-add pe0 new syn-old))))
(define penv-syntax-op-empty '||)
(define (penv-syntax-op-get pe op)
  (match (penv-syntax-get pe op)
    ((just result) (right result))
    ((nothing) (maybe->either (format "invalid operator: ~s" op)
                              (penv-syntax-get pe penv-syntax-op-empty)))))
(define/destruct (penv-vars-add (penv syntax vars) name)
  (penv syntax (cons name vars)))
(define (penv-vars-get pe name) (list-index (penv-vars pe) name))

(define v-uno (value (uno)))
(define v-0 (value (bit (b-0))))
(define v-1 (value (bit (b-1))))
(define new-lam-apply (curry action-2 (lam-apply)))
(define (new-pair-access tcnd tpair)
  (new-lam-apply
    (new-lam-apply
      (value (lam lattr-void
                  (value (lam lattr-void
                              (pair-access (bvar 1) (bvar 0))))))
      tcnd) tpair))
(define/match (new-pair l r)
  (((value vl) (value vr)) (right (value (pair vl vr))))
  ((_ _) (left (format "pair arguments must be values: ~v ~v" l r))))

(define (check-arity arity form)
  (if (equal? (length form) arity)
    (right '())
    (left (format "expected arity-~a form but found arity-~a form: ~s"
                  arity (length form) form))))
(define (check-symbol form)
  (if (symbol? form)
    (right '())
    (left (format "expected symbol but found: ~s" form))))
(define (parse-combination pe op form)
  (do either-monad
    proc <- (penv-syntax-op-get pe op)
    (proc pe form)))
(define ((map-parse parse) pe form)
  (map-monad either-monad (curry parse pe) form))
(define (((parse-apply map-parse) proc arity) pe form)
  (do either-monad
    _ <- (check-arity arity form)
    args <- (map-parse pe (cdr form))
    (pure (apply proc args))))
(define ((parse-under parse) pe params body)
  (do either-monad
    _ <- (map-monad either-monad check-symbol params)
    pe = (foldl (flip penv-vars-add) pe params)
    (parse pe body)))
(define (parse-bvar pe name)
  (do either-monad
    _ <- (check-symbol name)
    _ <- (if (equal? name '_) (left "unexpected reference to _") (right name))
    idx <- (maybe->either (format "unbound variable '~a'" name)
                          (penv-vars-get pe name))
    (pure (value (bvar idx)))))
