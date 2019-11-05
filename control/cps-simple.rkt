#lang racket
(print-as-expression #f)

;; This is a one-pass CPS transformation that uses both a first-order and
;; higher-order representation of continuations to avoid introducing any new
;; administrative redexes.

(define (rename n)
  (string->symbol (string-append (symbol->string n) ".cps")))
(define (fresh-k-var i)
  (string->symbol (string-append "r." (number->string i))))

(define (return k i v)      (if (procedure? k)
                              (k i v)
                              `(,k ,v)))
(define (return/push k i r) (if (procedure? k)
                              `(lambda (,r) ,(k (+ i 1) r))
                              k))

(define (cps/k i E k)
  (match E
    (`(quote ,c)          (return k i `(quote ,c)))
    ((? symbol? n)        (return k i (rename n)))
    (`(lambda (,p) ,body) (return k i `(lambda (,(rename p) k)
                                         ,(cps/k 0 body 'k))))
    (`(,(and op (or '+ '* 'equal?)) ,E1 ,E2)
      (cps/k i E1
             (lambda (i r1)
               (cps/k i E2
                      (lambda (i r2)
                        (return k i `(,op ,r1 ,r2)))))))
    (`(if ,E:condition ,E:true ,E:false)
      (cps/k i E:condition
             (lambda (_ r:condition)
               `(if ,r:condition
                  ,(cps/k i E:true k)
                  ,(cps/k i E:false k)))))
    (`(,E:rator ,E:rand)
      (define r (fresh-k-var i))
      (cps/k i E:rator
             (lambda (i r:rator)
               (cps/k i E:rand
                      (lambda (i r:rand)
                        `(,r:rator ,r:rand ,(return/push k i r)))))))))

(define (cps E) (cps/k 0 E (lambda (_ r) `(halt ,r))))


(define examples
  '((+ (f a)
       (* (if (g b) (h c) '5)
          (j d)))

    (+ (f ((z a) (x w)))
       (* (if (y (g b)) (h c) '5)
          (j d)))

    (lambda (q)
      (+ (f ((z a) (x w)))
         (* (if (y (g b)) (h c) '5)
            (j d))))

    (lambda (q) (q '11))
    ))

(for-each (lambda (e)
            (newline)
            (pretty-print e)
            (displayln '==>)
            (pretty-print (cps e)))
          examples)
