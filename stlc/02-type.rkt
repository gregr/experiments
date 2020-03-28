#lang racket

(define (type-lookup gamma name)
  (match gamma
    (`((,n ,val) . ,gamma)
      (if (eq? n name)
        val                        ;; Reference binding
        (type-lookup gamma name))))) ;; Traverse gamma

(define (type-extend gamma name val) `((,name ,val) . ,gamma))

(define (type-checker gamma term)
  (match term
    (`((lambda ,name ,body) : (,type1 -> ,type2))
      (let* ((gamma2 (type-extend gamma name type1))
             (type-of-result (type-checker gamma2 body)))
        (if (equal? type-of-result type2)
          `(,type1 -> ,type2)
          (error (format "expected type '~a' but found '~a'"
                         type2 type-of-result)))))
    (`(,fn ,arg)
      (match (type-checker gamma fn)
        (`(,fnin -> ,fnout)
          (let ((type-of-arg (type-checker gamma arg)))
            (if (equal? type-of-arg fnin)
              fnout
              (error (format "expected type '~a' but found '~a'"
                             fnin type-of-arg)))))))
    (_ (type-lookup gamma term))))


(define x (gensym `A))
(define y (gensym `B))

; Produces unique variable names given gamma
; EXTERNAL API:
(define (type-inf-outer gamma term)
  (type-inf gamma term '()))

; internal recursive accumulative thingy
(define (type-inf gamma term sub) ;sub = empty to start
  (match term
    (`(lambda ,name ,body) ;unannotated type

      (let* ((input-type (gensym)) ;name -> A
             (output-type (gensym)) ;body -> B
             (gamma2 (type-extend gamma name input-type))
             ((list type-of-result newsub)
              (type-inf gamma2 body sub)) ;?
        )
        (list `(,input-type -> ,output-type) ;(or type-of-result?)
              (unify newsub output-type type-of-result))))
    (`((lambda ,name ,body) : (,type1 -> ,type2))
      ; a bit of repetition but whatever
      (let* ((gamma2 (type-extend gamma name type1))
             ((list type-of-result newsub)
              (type-inf gamma2 body sub)))
        (list `(,type1 -> ,type2)
              (unify newsub type2 type-of-result))))
    (`(,fn ,arg)
      (let* ((fn-in (gensym))
             (fn-out (gensym))
             ((list fn-type sub2) (type-inf gamma fn sub))
             (sub3 (unify fn-type `(,fn-in -> ,fn-out) sub2))
             ((list arg-type sub4) (type-inf gamma arg sub3))
             (sub5 (unify arg-type fn-in sub4)))
        `(,fnout ,sub5)))
    (_ (list (type-lookup gamma term) sub))))

; gamma { c: (1->2), n: 1 }

;x = (lambda (c n) (lambda c c) (lambda c c))
;g = ((n `(1 -> 1)) . (c `(1 -> 1)))
;;(type-checker g x)
;(type-checker g `(,term : ,type) ...)

;x = [(lambda (c n) [(lambda c c): T] [(lambda c c): T]) : T]


;c is a function [1 -> 2]
;n is an inert crap [1]
;inner lambda [1 -> 2]
;outer lambda [(1 -> 2) -> (1 -> 2)]

;(lambda (c n) (c n)) ;; (lambda c (lambda n (c n))[1->2] ) [(1->2) -> (1->2)]

;g = ((n `1) . (c `(1 -> 2)))
;output => `((1 -> 2) -> 1 -> 2)
