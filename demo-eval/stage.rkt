#lang racket

;; Quasi-map (map over improper lists)
;; e.g., (~map2 list '(a b . c) '(1 2 3 4 5))
;;       =
;;       ((a 1) (b 2) (c (3 4 5)))
(define (~map2 f xs ys)
  (cond ((pair? xs) (cons (f (car xs) (car ys))
                          (~map2 f (cdr xs) (cdr ys))))
        ((null? xs) '())
        (else       (list (f xs ys)))))

;; Environments
(define env-empty '())

(define (env-ref env name)
  (define rib (assoc name env))
  (unless rib (error "unbound identifier:" name))
  (cdr rib))

(define (env-ref-syntax? env name)
  (and (symbol? name) (let ((value (env-ref env name)))
                        (and (procedure? value) value))))

(define (env-remove* env names)
  (remf* (lambda (rib) (member (car rib) names)) env))

(define (env-extend* env bindings)
  (append bindings (env-remove* env (map car bindings))))


;; Code generation: first order representation
;(define (ast:quote datum)       (vector 'quote  datum))
;(define (ast:var address)       (vector 'var    address))
;(define (ast:if c t f)          (vector 'if     c t f))
;(define (ast:apply* proc args)  (vector 'apply* proc args))
;(define (ast:lambda param body) (vector 'lambda param body))

;; Code generation: procedural representation
(define (ast:quote datum) (lambda (env) datum))
(define (ast:var address) (lambda (env) (env-ref env address)))
(define (ast:if c t f)    (lambda (env) (if (c env) (t env) (f env))))
(define (ast:apply* proc args)
  (lambda (env) (apply (proc env) (map (lambda (a) (a env)) args))))
(define (ast:lambda param body)
  (lambda (env)
    (lambda arg
      (body (env-extend* env (~map2 cons param arg))))))

;; Staged evaluation
(define (stage env form)
  (cond ((pair? form)  ;; combinations
         (define syntax-op (env-ref-syntax? env (car form)))
         (define operands (cdr form))
         (if syntax-op
           (apply syntax-op env operands)
           (ast:apply* (stage env (car form)) (stage* env operands))))

        ;; variables
        ((symbol? form) (ast:var (env-ref env form)))

        ;; literals
        (#t             (ast:quote form))))

(define (stage* env forms)
  (map (lambda (form) (stage env form)) forms))

(define (@quote env datum)
  (ast:quote datum))
(define (@if env c t f)
  (ast:if (stage env c) (stage env t) (stage env f)))
(define (@lambda env param body)
  (ast:lambda param (stage (env-extend* env (~map2 cons param param)) body)))
(define (@let env bindings body)
  (ast:apply* (@lambda env (map car bindings) body)
              (stage* env (map cadr bindings))))
(define (@cond env . clauses)
  (if (null? clauses) (ast:quote #t)
    (ast:if (stage env (caar clauses))
            (stage env (cadar clauses))
            (apply @cond env (cdr clauses)))))


(define env:initial
  `((quote  . , @quote)
    (if     . , @if)
    (lambda . , @lambda)
    (let    . , @let)
    (cond   . , @cond)))

(define library:base
  `((apply           . ,apply)
    (pair?           . ,pair?)
    (symbol?         . ,symbol?)
    (number?         . ,number?)
    (null?           . ,null?)
    (procedure?      . ,procedure?)
    (equal?          . ,equal?)
    (list            . ,list)
    (cons            . ,cons)
    (car             . ,car)
    (cdr             . ,cdr)
    (cadr            . ,cadr)
    (cdar            . ,cdar)
    (cddr            . ,cddr)
    (caar            . ,caar)
    (cadar           . ,cadar)
    (+               . ,+)
    (-               . ,-)
    (*               . ,*)
    (/               . ,/)
    (=               . ,=)
    (>               . ,>)
    (<               . ,<)
    (>=              . ,>=)
    (<=              . ,<=)
    (map             . ,map)
    (~map2           . ,~map2)
    (displayln       . ,displayln)
    (env-ref         . ,env-ref)
    (env-ref-syntax? . ,env-ref-syntax?)
    (env-remove*     . ,env-remove*)
    (env-extend*     . ,env-extend*)
    ))

(for-each
  (lambda (form)
    (newline)
    (pretty-write form)
    (displayln '=>)
    (flush-output)
    (define program (@lambda env:initial (map car library:base) form))
    (pretty-write (apply (program env-empty) (map cdr library:base)))
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

    (let ((sign (lambda (n) (cond ((< n 0) 'negative)
                                  ((= n 0) 'zero)
                                  (#t      'positive)))))
      (list (list -3 (sign -3))
            (list 10 (sign 10))
            (list  0 (sign  0))))

    (let ((fix (lambda (f)
                 ((lambda (d) (d d))
                  (lambda (x) (f (lambda arg (apply (x x) arg))))))))
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
