#lang racket

;; Environments
(define env-empty '())

(define (env-ref env name)
  (define rib (assoc name env))
  (unless rib (error "unbound identifier:" name))
  (cdr rib))

(define (env-remove* env names)
  (remf* (lambda (rib) (member (car rib) names)) env))

(define (env-extend* env bindings)
  (append bindings (env-remove* env (map car bindings))))


;; Staged evaluation
;; TODO: stage, stage*, @quote, @if, @cond, @lambda, @let
;; ast:var, ast:quote, ast:if, ast:lambda, ast:apply*


(define env:base
  `(;(quote         #t . , @quote)
    ;(if            #t . , @if)
    ;(cond          #t . , @cond)
    ;(lambda        #t . , @lambda)
    ;(let           #t . , @let)
    ))

(define library:base
  `((apply         . ,apply)
    (pair?         . ,pair?)
    (symbol?       . ,symbol?)
    (number?       . ,number?)
    (null?         . ,null?)
    (procedure?    . ,procedure?)
    (equal?        . ,equal?)
    (list          . ,list)
    (cons          . ,cons)
    (car           . ,car)
    (cdr           . ,cdr)
    (+             . ,+)
    (-             . ,-)
    (*             . ,*)
    (/             . ,/)
    (=             . ,=)
    (>             . ,>)
    (<             . ,<)
    (>=            . ,>=)
    (<=            . ,<=)
    ))

(for-each
  (lambda (form)
    (newline)
    (pretty-write form)
    (displayln '=)
    (flush-output)
    (define program `(lambda ,(map car library:base) ,form))
    (pretty-write (apply ((stage env:base program) env-empty)
                         (map cdr library:base)))
    (flush-output))

  '(
    #t

    (+ 2 8)

    (cons 'a '(b c))

    (if #t 'yes 'no)

    (if #f 'yes 'no)

    ((lambda (x) x) 3)

    ((lambda (lambda) lambda) 3)

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
