#lang racket

;; Environments
(define env-empty '())

(define (env-ref env name)
  (define rib (assoc name env))
  (unless rib (error "unbound identifier:" name))
  (cdr rib))

(define (env-ref-syntax? env name)
  (and (symbol? name) (car (env-ref env name))))

(define (env-ref-value env name)
  (cdr (env-ref env name)))

(define (env-remove* env names)
  (remf* (lambda (rib) (member (car rib) names)) env))

(define (env-extend* env bindings)
  (append bindings (env-remove* env (map car bindings))))


;; Evaluation
;; TODO: eval, eval*, @quote, @if, @cond, @lambda, @let, @lambda-syntax, @let-syntax


(define env:base
  `((@             #f . ,(lambda (proc . args) (apply proc args)))
    (eval          #f . ,eval)
    (apply         #f . ,apply)
    (pair?         #f . ,pair?)
    (symbol?       #f . ,symbol?)
    (number?       #f . ,number?)
    (null?         #f . ,null?)
    (procedure?    #f . ,procedure?)
    (equal?        #f . ,equal?)
    (list          #f . ,list)
    (cons          #f . ,cons)
    (car           #f . ,car)
    (cdr           #f . ,cdr)
    (+             #f . ,+)
    (-             #f . ,-)
    (*             #f . ,*)
    (/             #f . ,/)
    (=             #f . ,=)
    (>             #f . ,>)
    (<             #f . ,<)
    (>=            #f . ,>=)
    (<=            #f . ,<=)

    ;(quote         #t . , @quote)
    ;(if            #t . , @if)
    ;(cond          #t . , @cond)
    ;(lambda        #t . , @lambda)
    ;(let           #t . , @let)

    ;(lambda-syntax #t . , @lambda-syntax)
    ;(let-syntax    #t . , @let-syntax)

    ($             #t . ,(lambda (env rator . rands)
                           (apply (eval env rator) env rands)))))


(for-each
  (lambda (form)
    (newline)
    (pretty-write form)
    (displayln '=)
    (flush-output)
    (pretty-write (eval env:base form))
    (flush-output))

  '(
    #t

    (+ 2 8)

    (cons 'a '(b c))

    (if #t 'yes 'no)

    (if #f 'yes 'no)

    ((lambda (x) x) 3)

    ((lambda (lambda) lambda) 3)

    ((@ lambda '() '() 5))

    ((@ lambda '() '(x) 'x) 6)

    ((@ lambda
        ($ (lambda (e) e))
        '(x y)
        '(cons x y))
     11 12)

    (cond (#t 'first)
          (#f 'second)
          (#t 'third))

    (cond (#f 'first)
          (#f 'second)
          (#t 'third))

    (let ((fix (lambda (f)
                 ((lambda (d) (d d))
                  (lambda (x) (f (lambda (a b) ((x x) a b))))))))
      (let ((append
              (fix (lambda (append)
                     (lambda (xs ys)
                       (if (null? xs)
                         ys
                         (cons (car xs) (append (cdr xs) ys))))))))
        (list (append '() '())
              (append '(foo) '(bar))
              (append '(1 2) '(3 4)))))
    ))
