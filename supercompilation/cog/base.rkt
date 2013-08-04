#lang racket

(require "util.rkt")

(data value
  (lam (body env))
  (sym (name))
  ; uid?
  (pair (l r))
  (uno ()))

(data term
  (val (x))
  (var (idx))
  (app (op arg))
  (if-eq (sym0 sym1 true false))
  (pair-left (x))
  (pair-right (x))
  (let-rec (defs body)))

(data penv (penv (syntax vars)))
(define penv-empty (penv dict-empty '()))
(define (penv-syntax-add pe name op)
  (match pe
    ((penv syntax vars) (penv (dict-add syntax name op) vars))))
(define (penv-syntax-del pe name)
  (match pe
    ((penv syntax vars) (penv (dict-del syntax name) vars))))
(define (penv-syntax-get pe name) (dict-get (penv-syntax pe) name))
(define (penv-syntax-rename pe old new)
  (let ((check-vars (lambda (name msg)
                      (match (penv-vars-get pe name)
                        ((nothing) (right '()))
                        ((just _) (left msg))))))
    (do either-monad
      _ <- (check-vars old "cannot rename non-keyword")
      _ <- (check-vars new "rename-target already bound as a non-keyword")
      syn-old <- (maybe->either "cannot rename non-existent keyword"
                                (penv-syntax-get pe old))
      pe0 = (penv-syntax-del pe old)
      (pure (penv-syntax-add pe0 new syn-old)))))
(define (penv-vars-add pe name)
  (match pe
    ((penv syntax vars) (penv syntax (cons name vars)))))
(define (penv-vars-get pe name) (list-index (penv-vars pe) name))

(define (check-arity arity form)
  (if (equal? (length form) arity)
    (right '())
    (left (format "expected arity-~a form but found arity-~a form: ~s"
                  arity (length form) form))))
(define (check-symbol form)
  (if (symbol? form)
    (right '())
    (left (format "expected symbol but found: ~s" form))))

(define (parse pe form)
  (match form
    ('() (right (val (uno))))
    ((? symbol?) (parse-var pe form))
    ((cons op rest) (parse-combination pe op form))
    (_ (left (format "cannot parse: ~s" form)))))
(define (parse-combination pe op form)
  ((if (symbol? op)
     (maybe-from parse-app (penv-syntax-get pe op))
     parse-app)
   pe form))

(define (map-parse pe form) (map-monad either-monad (curry parse pe) form))
(define ((parse-apply proc arity) pe form)
  (do either-monad
    _ <- (check-arity arity form)
    args <- (map-parse pe (cdr form))
    (pure (apply proc args))))
(define (parse-under pe param body)
  (do either-monad
    _ <- (check-symbol param)
    pe = (penv-vars-add pe param)
    (parse pe body)))

(define (parse-var pe name)
  (do either-monad
    idx <- (maybe->either (format "unbound variable '~a'" name)
                          (penv-vars-get pe name))
    (pure (var idx))))
(define (parse-app pe form)
  (do either-monad
    form <- (map-parse pe form)
    (cons proc args) = form
    (pure
      (let loop ((proc proc) (args args))
        (match args
          ('() proc)
          ((cons arg args) (loop (app proc arg) args)))))))
(define (parse-lam pe form)
  (do either-monad
    _ <- (check-arity 3 form)
    `(,_ ,name ,body) = form
    body <- (parse-under pe name body)
    (pure (val (lam body '())))))
(define (parse-let-rec pe form)
  (define-struct lrdef (name param body))
  (define (lr-def form)
    (do either-monad
      _ <- (check-arity 3 form)
      `(,name ,param ,body) = form
      _ <- (check-symbol name)
      _ <- (check-symbol param)
      (pure (lrdef name param body))))
  (do either-monad
    _ <- (check-arity 3 form)
    `(,_ ,defs ,body) = form
    defs <- (map-monad either-monad lr-def defs)
    names = (map lrdef-name defs)
    pe = (foldl (flip penv-vars-add) pe names)
    defs <- ((flip (curry map-monad either-monad)) defs
              (lambda (def)
                (parse-under pe (lrdef-param def) (lrdef-body def))))
    body <- (parse pe body)
    (pure (let-rec defs body))))
(define (parse-sym pe form)
  (do either-monad
    _ <- (check-arity 2 form)
    `(,_ ,name) = form
    (pure (val (sym name)))))
(define parse-if-eq (parse-apply if-eq 5))
(define parse-pair (parse-apply pair 3))
(define parse-pair-left (parse-apply pair-left 2))
(define parse-pair-right (parse-apply pair-right 2))

(define penv-init
  (foldr (lambda (keyval pe) (apply (curry penv-syntax-add pe) keyval))
         penv-empty
         `((lam ,parse-lam)
           (sym ,parse-sym)
           (pair ,parse-pair)
           (if-eq ,parse-if-eq)
           (pair-left ,parse-pair-left)
           (pair-right ,parse-pair-right)
           (let-rec ,parse-let-rec))))

; testing
(define tests
  `((lam x x)
    (lam x (lam y x))
    ()
    (pair () ())
    (pair-left (pair () ()))
    (pair-right (pair () ()))
    (sym abc)
    (if-eq (sym abc) (sym def) () ())
    (let-rec ((x y (y x))) (x x))
    (pair () () ())))

(map (lambda (form) (parse penv-init form)) tests)

(displayln "map-parse:")
(map-parse penv-init tests)

;> (define pe (penv-vars-add (penv-vars-add (penv-syntax-add penv-empty 'x 'y) 'z) 'w))
;> (penv-syntax-rename pe 'x 'y)
;(right
 ;(penv
   ;(dict (list (cons 'y (just 'y)) (cons 'x (nothing)) (cons 'x (just 'y))))
   ;'(w z)))
;>
