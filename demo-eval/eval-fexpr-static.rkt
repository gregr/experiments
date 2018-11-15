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
  (and (symbol? name) (car (env-ref env name))))

(define (env-ref-value env name)
  (cdr (env-ref env name)))

(define (env-remove* env names)
  (remf* (lambda (rib) (member (car rib) names)) env))

(define (env-extend* env bindings)
  (append bindings (env-remove* env (map car bindings))))


;; Evaluation
(define (eval env form)
  (cond ((pair? form)  ;; combinations
         (define proc (eval env (car form)))
         (define operands (cdr form))
         (if (env-ref-syntax? env (car form))
           (apply proc env operands)
           (apply proc (eval* env operands))))

        ;; variables
        ((symbol? form) (env-ref-value env form))

        ;; literals
        (#t             form)))

(define (eval* env forms)
  (map (lambda (form) (eval env form)) forms))


(define (@quote env datum) datum)

(define (@if env c t f)
  (if (eval env c)
    (eval env t)
    (eval env f)))

(define (@lambda env params body)
  (lambda args
    (define bindings (~map2 (lambda (p a) `(,p #f . ,a)) params args))
    (eval (env-extend* env bindings) body)))


(define env:initial
  `((@               #f . ,(lambda (proc . args) (apply proc args)))
    (eval            #f . ,eval)
    (eval*           #f . ,eval*)
    (apply           #f . ,apply)
    (pair?           #f . ,pair?)
    (symbol?         #f . ,symbol?)
    (number?         #f . ,number?)
    (null?           #f . ,null?)
    (procedure?      #f . ,procedure?)
    (equal?          #f . ,equal?)
    (list            #f . ,list)
    (cons            #f . ,cons)
    (car             #f . ,car)
    (cdr             #f . ,cdr)
    (cadr            #f . ,cadr)
    (cdar            #f . ,cdar)
    (cddr            #f . ,cddr)
    (caar            #f . ,caar)
    (cadar           #f . ,cadar)
    (+               #f . ,+)
    (-               #f . ,-)
    (*               #f . ,*)
    (/               #f . ,/)
    (=               #f . ,=)
    (>               #f . ,>)
    (<               #f . ,<)
    (>=              #f . ,>=)
    (<=              #f . ,<=)
    (map             #f . ,map)
    (~map2           #f . ,~map2)
    (displayln       #f . ,displayln)
    (env-ref         #f . ,env-ref)
    (env-ref-syntax? #f . ,env-ref-syntax?)
    (env-ref-value   #f . ,env-ref-value)
    (env-remove*     #f . ,env-remove*)
    (env-extend*     #f . ,env-extend*)

    (quote           #t . , @quote)
    (if              #t . , @if)
    (lambda          #t . , @lambda)

    ;; $ applies a procedure as if it is a syntax operator.
    ($               #t . ,(lambda (env rator . rands)
                             (apply (eval env rator) env rands)))))

;; Bootstrap env:base, which will contain bindings for:
;;   lambda-syntax, let-syntax, let, cond
(define bootstrap-env:base
  '((lambda (@lambda-syntax)
      ;; Use @lambda-syntax to bind itself as the syntax: lambda-syntax
      (($ @lambda-syntax (lambda-syntax)
          ((lambda-syntax
             (let-syntax)
             (let-syntax ((let (lambda (env bindings body)
                                 (apply (@ lambda env (map car bindings) body)
                                        (eval* env (map cadr bindings))))))

               (let ((fix (lambda (f)
                            ((lambda (d) (d d))
                             (lambda (x) (f (lambda arg
                                              (apply (x x) arg))))))))

                 (let-syntax
                   ((cond (fix (lambda (@cond)
                                 (lambda (env . clauses)
                                   (if (null? clauses) #t
                                     (if (eval env (caar clauses))
                                       (eval env (cadar clauses))
                                       (apply @cond env (cdr clauses)))))))))

                   ;; env:base
                   ($ (lambda (env) env))))))

           ;; let-syntax
           (lambda (env bindings body)
             (apply (@lambda-syntax env (map car bindings) body)
                    (eval* env (map cadr bindings))))))

       ;; lambda-syntax
       @lambda-syntax))

    ;; @lambda-syntax
    (lambda (env param body)
      (lambda arg
        (eval (env-extend* env (~map2 (lambda (p a) (cons p (cons #t a)))
                                      param arg))
              body)))))

(define env:base (eval env:initial bootstrap-env:base))

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
