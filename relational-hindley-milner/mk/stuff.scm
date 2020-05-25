(load "mk.scm")
(load "evalo.scm")

;(1 2 3)
;(1 . (2 . (3 . ())))

;(1 2 3 . 4)
;(1 . (2 . (3 . 4)))

;(cond (T1 E1)
      ;(T2 E2)
      ;...
      ;(else ...))


;(define (employee-jobo name job)
  ;(conde
    ;((== name 'Billy)  (== job 'Janitor))
    ;((== name 'Bob)    (== job 'Farmer))
    ;((== name 'Rob)    (== job 'Programmer))
    ;((== name 'Robby)  (== job 'Cook))
    ;((== name 'Robert) (== job 'Janitor))
    ;((== name 'Bobby)  (== job 'Programmer))
    ;))

;(define (job-salaryo job salary)
  ;(conde
    ;((== job 'Farmer)     (== salary 2))
    ;((== job 'Janitor)    (== salary 8))
    ;((== job 'Programmer) (== salary 1))
    ;))



;(define (my-append xs ys)
  ;(if (null? xs)
    ;ys
    ;(cons (car xs) (my-append (cdr xs) ys))))

;(define (multiple-answers-please x y)
  ;(list (cons x 1) (cons y 2)))

;(define (multiple-answers-pleaseo x y answer1 answer2)
  ;(== (cons y 2) answer2)
  ;(== answer1 (cons x 1)))

;(display (my-append '(a b c) '(d e f)))


(define (my-append xs ys)
  (cond
    ((null? xs) ys)
    (else       (let* ((x       (car xs))
                       (xs-rest (cdr xs))
                       (zs      (my-append xs-rest ys)))
                  (cons x zs)))))

(define (appendo xs ys xsys)
  (conde
    ((== '() xs) (== ys xsys))
    ((fresh (x xs-rest zs)
       (== `(,x . ,xs-rest) xs)
       (== `(,x . ,zs) xsys)
       (appendo xs-rest ys zs)))))

;(display (run 30 (q)
           ;;(evalo (quote (quote 5)) q)
           ;(evalo q 5)

           ;;(appendo q r '(1 2 3 4 5 6))

           ;;(employee-jobo name job)
           ;;(job-salaryo        job salary)

           ;;(== salary 8)
           ;;(conde
             ;;((== q 1) (== r 2))
             ;;((== q s) (== r 3)))



           ;;(== s 2)

           ;;(numbero s)
           ;;(=/= `(,t . 4) `(3 . ,u))
           ;;(== `(,q 4 ,r 5 . ,u) `(3 ,s 6 ,t))
           ;;(== `(,t . (4 . ())) `(3 ,u))
           ;;(== (cons t (cons 4 '())) `(3 ,u))
           ;;(== u 4)
           ;;(== t 3)
           ;;(=/= 5 s)
           ;;(== s t)
           ;;(== 1 q)
           ;;(== r 2)

           ;))
;(newline)
