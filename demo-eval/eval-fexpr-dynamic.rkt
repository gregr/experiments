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

(define (env-ref-value env name)
  (define rib (assoc name env))
  (unless rib (error "unbound identifier:" name))
  (cdr rib))

(define (env-remove* env names)
  (remf* (lambda (rib) (member (car rib) names)) env))

(define (env-extend* env bindings)
  (append bindings (env-remove* env (map car bindings))))

(struct wrap (>combiner) #:transparent)
(define unwrap wrap->combiner)


;; Evaluation
(define (combine operator env operands)
  ;; TODO:
  )

(define (eval env form)
        ;; TODO: combinations

        ;; TODO: variables

        ;; TODO:literals
  )

(define (eval* env forms)
  (map (lambda (form) (eval env form)) forms))


;; TODO: $quote, $if, $vau


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

    `(
      ;; TODO:
      ;(quote . ,$quote)
      ;(if    . ,$if)
      ;(vau   . ,$vau)
      )))

;; Bootstrap env:base, which will contain bindings for:
;;   lambda, let, cond
(define bootstrap-env:base
  '((wrap
      (vau (_ lambda apply)
           ((lambda (let)
              (let ((fix (lambda (f)
                           ((lambda (d) (d d))
                            (lambda (x) (f (lambda arg
                                             (apply (x x) arg))))))))

                (let ((cond (vau (env . clauses)
                                 ((fix (lambda (@cond)
                                         (lambda (env clauses)
                                           ;; TODO:
                                           )))
                                  env clauses))))

                  ;;; env:base
                  ((vau (env) env)))))

            ;; let
            (vau (env bindings body)
                 ;; TODO:
                 ))))

    ;; lambda
    (vau (env param body)
         ;; TODO:
         )

    ;; apply
    (wrap (vau (env combiner arg)
               ;; TODO:
               ))))

;; TODO:
(define env:base env:initial)
;(define env:base (eval env:initial bootstrap-env:base))

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
