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
(struct term-lit (data) #:transparent)
(struct term-constr (tag args) #:transparent)
(struct term-case (default-cont tags->destrs tval) #:transparent)

(struct val-proc (binder body env) #:transparent)
(struct val-proc-foreign (proc) #:transparent)
(struct val-tagged (tag args) #:transparent)

(define (tagged-construct tag args) (val-tagged tag args))

(define (apply-proc proc arg)
  (match proc
    ((val-proc binder body env)
      (eval-term body (env-extend env binder arg)))
    ((val-proc-foreign proc) (proc arg))))

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
    ((term-lit data) (tagged-construct data '()))
    ((term-constr tag args)
     (let ((tag (eval-term tag env)) (args (map (lambda (arg) (eval-term arg env)) args)))
       (tagged-construct tag args)))
    ((term-case default-cont tags->destrs tval)
     (let ((tval (eval-term tval env)))
       (match tval
         ((val-tagged tag args) ((hash-ref tags->destrs tag default-cont) args env))
         (_ (error 'eval-term "attempted case-analysis of untagged data: ~s" tval)))))))

(define (build-case arg def-alt alts)
  (let ((def-alt (build-term def-alt))
        (tags->destrs (make-hash
                        (map (match-lambda ((list tag params body)
                               (let* ((body (build-term body))
                                      (cont (foldr term-proc body params)))
                                 (cons tag (lambda (args env)
                                             (let ((cont (eval-term cont env)))
                                               (foldl (lambda (arg cont) (apply-proc cont arg)) cont args))))))) alts))))
    (term-case (lambda () (lambda (_ env) (eval-term def-alt env))) tags->destrs (build-term arg))))

(define (build-term data)
  (match data
    ((? symbol?) (term-var data))
    (`(proc ,param ,body) (term-proc param (build-term body)))
    (`(letrec (,param ,arg) ,body) (term-letrec param (build-term arg) (build-term body)))
    (`(constr ,tag ,args) (term-constr (build-term tag) (map build-term args)))
    (`(case ,arg ,def-alt . ,alts) (build-case arg def-alt alts))
    ((list single) (build-term single))
    ((? list?) (foldl (flip2 term-app) (build-term (car data)) (map build-term (cdr data))))
    (_ (term-lit data))))

; example
(define (arith2 op) ; FIXME: this is gross, improve introduction of foreign procs
  (val-proc-foreign (lambda (arg0) (val-proc-foreign (lambda (arg1)
    (match* (arg0 arg1)
      (((val-tagged n0 '()) (val-tagged n1 '())) (tagged-construct (op n0 n1) '()))))))))
(define ex-env (foldl (lambda (symval env) (env-extend env (car symval) (cadr symval))) (env-new)
                      (map (lambda (opsym) (list opsym (arith2 (eval opsym)))) '(+ - *))))
(define ex-term (build-term
  '(letrec (factorial
             (proc acc (proc n (case n (factorial (* acc n) (- n 1)) (0 () acc)))))
     (factorial 1 7))))
(define ex-result (eval-term ex-term ex-env))
