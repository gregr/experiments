#lang racket

; Lists are syntactic sugar:
; (a b c)    ==> (a . (b . (c . ())))
; (a b . c)  ==> (a . (b . c))

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

(define (env-ref-value env name)
  (define rib (assoc name env))
  (unless rib (error "unbound identifier:" name))
  (cdr rib))

(define (env-remove* env names)
  (remf* (lambda (rib) (member (car rib) names)) env))

;(append
  ;'((a . 1)
    ;(b . 2)
    ;(c . (3 4 5))
    ;)
  ;env ;; removed a b c
  ;)

(define (env-extend* env bindings)
  (append bindings (env-remove* env (map car bindings))))

(struct wrap (>combiner) #:transparent)
(define unwrap wrap->combiner)

;; vau will combine epsilon and lambda: (eps (env) (lambda (x ...) ...))
;; we'll represent this in Racket as: (lambda (env x ...) ...)

;; [combine ⟨λx.T1⟩ T2 T3]   →   T1[x ← T2]
;; [combine ⟨εx.T1⟩ T2 T3]   →   [combine T1[x ← T3] T2 T3]

;; Example rewriting of the combined epsilon lambda:
;; [combine ⟨εenv.⟨λx.T1⟩⟩ T2 T3]    →  [combine ⟨λx.T1⟩[env ← T3] T2 T3]
;; [combine ⟨λx.T1[env ← T3]⟩ T2 T3] →  T1[env ← T3][x ← T2]

;; Evaluation
;;               T1       T3  T2
(define (combine operator env operands)
  (if (wrap? operator)
    (combine (unwrap operator) env (eval* env operands))
    (apply operator env operands)))

(define (eval env form)
        ;; combinations
        ;; [eval (T1 T2) T3]   →   [combine [eval T1 T3] T2 T3]
  (cond ((pair? form)   (combine (eval env (car form)) env (cdr form)))
        ;; variables
        ;; [eval s e]   →   lookup(s,e)     if symbol s is bound in environment e
        ((symbol? form) (env-ref-value env form))
        ;; literals
        ;; [eval d T]   →   d
        ;;   if d is an empty list, boolean, λ-function, ε-function, or environment
        (#t form)))

(define (eval* env forms)
  (map (lambda (form) (eval env form)) forms))

;; It turns out we can derive this instead.
;(define ($quote env datum) datum)

;; We only need to provide $if (unless we want church-encoded booleans) and $vau.
(define ($if env c t f)
  (if (eval env c)
    (eval env t)
    (eval env f)))

(define ($vau env params body)
  (lambda args
    (define bindings (~map2 cons params args))
    (eval (env-extend* env bindings) body)))

(define (lift proc) (lambda (ignored-env . args) (apply proc args)))
(define (lower-arg0 proc)
  (lambda (arg0 . args)
    (apply proc (lambda args (combine (unwrap arg0) 'ignored args)) args)))

(define env:initial
  (append
    (map (lambda (binding) (cons (car binding) (wrap (lift (cdr binding)))))
         `((wrap           . ,wrap)
           (wrap?          . ,wrap?)
           (unwrap         . ,unwrap)
           (eval           . ,eval)
           (eval*          . ,eval*)
           (combine        . ,combine)
           (pair?          . ,pair?)
           (symbol?        . ,symbol?)
           (number?        . ,number?)
           (null?          . ,null?)
           (procedure?     . ,procedure?)
           (equal?         . ,equal?)
           (list           . ,list)
           (cons           . ,cons)
           (car            . ,car)
           (cdr            . ,cdr)
           (cadr           . ,cadr)
           (cdar           . ,cdar)
           (cddr           . ,cddr)
           (caar           . ,caar)
           (cadar          . ,cadar)
           (+              . ,+)
           (-              . ,-)
           (*              . ,*)
           (/              . ,/)
           (=              . ,=)
           (>              . ,>)
           (<              . ,<)
           (>=             . ,>=)
           (<=             . ,<=)
           (map            . ,(lower-arg0 map))
           (displayln      . ,displayln)
           (env-ref-value  . ,env-ref-value)
           (env-remove*    . ,env-remove*)
           (env-extend*    . ,env-extend*)))
    `(;(quote . ,$quote)
      (if    . ,$if)
      (vau   . ,$vau))))

;; Bootstrap env:base, which will contain bindings for:
;;   lambda, let, cond
(define bootstrap-env:base
  '((wrap
      (vau (_ quote lambda apply)
           ((lambda (let)
              (let ((fix (lambda (f)
                           ((lambda (d) (d d))
                            (lambda (x) (f (lambda arg
                                             (apply (x x) arg))))))))

                (let ((cond (vau (env . clauses)
                                 ((fix (lambda (@cond)
                                         (lambda (env clauses)
                                           (if (null? clauses) #t
                                             (if (eval env (caar clauses))
                                               (eval env (cadar clauses))
                                               (@cond env (cdr clauses)))))))
                                  env clauses))))

                  ;;; env:base
                  ((vau (env) env)))))

            ;;     (let ((x a) (y b) ...) body)
            ;; ==> ((lambda (x y ...) body) a b ...)
            (vau (env bindings body)
                 (combine (combine lambda env (list (map car bindings) body))
                          env
                          (map cadr bindings))
                 ;; Uglier alternative:
                 ;(apply (wrap (combine lambda env
                                       ;(list (map car bindings) body)))
                        ;(map cadr bindings))
                 ))))
    ;; quote
    (vau (env datum) datum)

    ;; lambda
    ;; (lambda param body) ==> (wrap (vau (#f . param) body))
    ;; The #f means we're ignoring the env argument.
    (vau (env param body)
         (wrap (combine vau env (list (cons #f param) body))))

    ;; apply
    (wrap (vau (env proc arg)
               (combine (unwrap proc) env arg)))))

(define env:base (eval env:initial bootstrap-env:base))

(for-each
  (lambda (form)
    (newline)
    (pretty-write form)
    (displayln '=>)
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

    ((combine lambda '() '(() 5)))

    ((combine lambda '() '((x) x)) 6)

    ((combine lambda
              ((vau (e) e))
              '((x y) (cons x y)))
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
