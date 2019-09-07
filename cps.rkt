#lang racket
(require racket/pretty)

;; This CPS transformation does not introduce any new administrative redexes.
;; If an expression already contains such redexes, they will remain.


;; op1 ::= car | cdr
;; op2 ::= + | * | cons | equal?


;; DS grammar

;; E ::= (var v)
;;     | (lambda (v) E)
;;     | (quote c)
;;     | (prim1 op1 E)
;;     | (prim2 op2 E E)
;;     | (app E E)
;;     | (if E E E)


;; CPS grammar

;; T ::= (var v)
;;     | (lambda (v k) C_k)  ;; NOTE: two parameters.
;;     | (quote c)
;;     | (prim1 op1 T)
;;     | (prim2 op2 T T)
;;
;; K_k ::= (kvar k)
;;       | (lambda (x) C_k)  ;; NOTE: one parameter.
;;
;; C_k ::= (return k T)
;;       | (app T T K_k)
;;       | (if T C_k C_k)                     ;; where k is a var
;;       | (let ((k2 K_k)) (if T C_k2 C_k2))  ;; where K_k is a lambda


(define (fresh-k-var ctx)
  (string->symbol (string-append "x." (number->string (length ctx)))))
(define (rename-var v)
  (string->symbol (string-append "v." (symbol->string v))))

(define (cps/context ctx E)
  (match E
    (`(var ,v)         (cons ctx `(var ,(rename-var v))))
    (`(lambda (,v) ,E) (cons ctx `(lambda (,(rename-var v) k)
                                    ,(cps/k 'k E))))
    (`(quote ,c)       (cons ctx E))

    (`(prim1 ,op ,E)
      (match-let (((cons ctx T) (cps/context ctx E)))
        (cons ctx `(prim1 ,op ,T))))

    (`(prim2 ,op ,E1 ,E2)
      (match-let* (((cons ctx T1) (cps/context ctx E1))
                   ((cons ctx T2) (cps/context ctx E2)))
        (cons ctx `(prim2 ,op ,T1 ,T2))))

    (`(app ,E:rator ,E:rand)
      (match-let* (((cons ctx T:rator) (cps/context ctx E:rator))
                   ((cons ctx T:rand)  (cps/context ctx E:rand))
                   (x                  (fresh-k-var ctx)))
        (cons (cons (vector 'app x T:rator T:rand) ctx)
              `(var ,x))))

    (`(if ,E:condition ,E:true ,E:false)
      (match-let* (((cons ctx T:condition) (cps/context ctx E:condition))
                   (x                      (fresh-k-var ctx)))
        (cons (cons (vector 'if x T:condition E:true E:false) ctx)
              `(var ,x))))))

(define (cps/k k E)
  (define (k-minimal x return)
    (match return
      (`(return ,k (var ,y)) #:when (equal? x y) `(kvar ,k))
      (_                                         `(lambda (,x) ,return))))
  (match-define (cons ctx T) (cps/context '() E))
  (foldl
    (lambda (frame return)
      (match frame
        (`#(app ,x ,rator ,rand)
          `(app ,rator ,rand ,(k-minimal x return)))

        (`#(if ,x ,condition ,E:true ,E:false)
          (match (k-minimal x return)
            (`(kvar ,k) `(if ,condition
                           ,(cps/k k E:true)
                           ,(cps/k k E:false)))
            (klam     `(let ((k ,klam))
                         (if ,condition
                           ,(cps/k 'k E:true)
                           ,(cps/k 'k E:false))))))))
    `(return ,k ,T)
    ctx))

(define (cps E) `(lambda (halt) ,(cps/k 'halt E)))

(define test-expr
  '(app (lambda (add1)
          (app (lambda (d) (if (quote #t) (var d) (quote 500)))
               (prim2 cons
                      (if (prim2 equal? (quote 0) (app (var add1) (quote -1)))
                        (app (var add1)
                             (app (var add1) (quote 5)))
                        (quote 11))
                      (app (app (lambda (u) (var u)) (var add1))
                           (quote 2)))))
        (lambda (n) (prim2 + (var n) (quote 1)))))

(pretty-print (cps test-expr))
