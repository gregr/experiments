#lang racket

(require "common.rkt")

(struct val-undefined () #:transparent) ; void type
(struct val-indirect (val) #:mutable #:transparent)
(define undefined (val-undefined))

(define (env-new) (make-immutable-hasheq '()))
(define (env-extend env sym val) (hash-set env sym val))
(define (env-getd env sym default)
  (match (hash-ref env sym default)
    ((val-indirect val) val)
    (val val)))
(define (env-get env sym) (env-getd env sym undefined))
(define (env-get-checked env sym)
  (let ((val (env-get env sym)))
    (if (eq? val undefined)
      (error 'env-get-checked "referenced unbound variable: ~s" sym) val)))

(struct term-var (sym) #:transparent)
(struct term-app (proc arg) #:transparent)
(struct term-proc (param body) #:transparent)
(struct term-letrec (param arg body) #:transparent)
(struct term-if0 (condition consequent alternative) #:transparent)
(struct term-lit (data) #:transparent) ; include native procedures

(struct val-proc (binder body env) #:transparent)

(define (apply-proc proc arg)
  (match proc
    ((val-proc binder body env)
      (eval-term body (env-extend env binder arg)))
    (_ (proc arg)))) ; native proc

(define (eval-term term env)
  (match term
    ((term-var sym) (env-get-checked env sym))
    ((term-app proc arg)
     (let ((proc (eval-term proc env)) (arg (eval-term arg env)))
       (apply-proc proc arg)))
    ((term-proc param body) (val-proc param body env))
    ((term-letrec param arg body)
     (let* ((ind (val-indirect undefined)) (env (env-extend env param ind)) (arg (eval-term arg env)))
       (set-val-indirect-val! ind arg)
       (eval-term body env)))
    ((term-if0 cnd cns alt)
     (if (equal? 0 (eval-term cnd env)) (eval-term cns env) (eval-term alt env)))
    ((term-lit data) data)))

(define (build-term data)
  (match data
    ((? symbol?) (term-var data))
    (`(proc ,param ,body) (term-proc param (build-term body)))
    (`(letrec (,param ,arg) ,body) (term-letrec param (build-term arg) (build-term body)))
    (`(if0 ,cnd ,cns ,alt) (term-if0 (build-term cnd) (build-term cns) (build-term alt)))
    ((list single) (build-term single))
    ((? list?) (foldl (flip2 term-app) (build-term (car data)) (map build-term (cdr data))))
    (_ (term-lit data))))

; example
(define ex-env (foldl (lambda (symval env) (env-extend env (car symval) (cadr symval))) (env-new)
                      `((+ ,(curry2 +)) (- ,(curry2 -)) (* ,(curry2 *)))))
(define ex-term (build-term
  '(letrec (factorial
            (proc acc (proc n (if0 n acc (factorial (* acc n) (- n 1))))))
     (factorial 1 7))
))
(define ex-result (eval-term ex-term ex-env))
