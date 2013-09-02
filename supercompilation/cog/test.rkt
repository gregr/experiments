#lang racket
(require "util.rkt")
(require "base.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (denote-form form)
  (do either-monad
    term <- (parse-default form)
    (pure (denote term))))
(define (denote-eval-form form)
  (do either-monad
    term <- (parse-default form)
    (pure (denote-eval term))))

; TODO: use racket's test facilities
(define tests
  `((((lam x (lam y ())) (sym one)) (sym two))
    (((lam x (lam y x)) (sym one)) (sym two))
    (((lam x (lam y y)) (sym one)) (sym two))
    (((lam x (lam y (pair-left (pair x y)))) (sym left)) (sym right))
    (((lam x (lam y (pair-right (pair x y)))) (sym left)) (sym right))
    (let-rec ((x arg (y arg)) (y arg arg)) (x ()))
    (let-rec ((x y (y x))) (x x))
    (let-rec ((x arg (y arg)) (y arg (x arg))) (x ()))
    (lam x x)
    (lam x (lam y x))
    ()
    (lam x (pair x x))
    (sym abc)
    (if-eq (sym abc) (sym def) () ())
    (x y)
    (pair () ())
    (pair () () ())))

(define parsed-tests (map (lambda (form) (parse penv-init form)) tests))
parsed-tests

(displayln "map-parse:")
(map-parse penv-init tests)

(displayln "eval:")
(denote-eval (right-x (list-ref parsed-tests 0)))
(denote-eval (right-x (list-ref parsed-tests 1)))
(denote-eval (right-x (list-ref parsed-tests 2)))
(denote-eval (right-x (list-ref parsed-tests 3)))
(denote-eval (right-x (list-ref parsed-tests 4))) ; 'right
(denote-eval (right-x (list-ref parsed-tests 5))) ; '()
;(denote-eval (right-x (list-ref parsed-tests 6))) ; infinite loop
;(denote-eval (right-x (list-ref parsed-tests 7))) ; another infinite loop

(define tstart (term->state (right-x (list-ref parsed-tests 4))))
tstart

(define (step-n-show st count)
  (match (step-n st count (lambda (sti) (printf "~a\n\n\n" (state-show sti))))
    ((left msg) (begin (displayln msg) st))
    ((right st) (begin (displayln (state-show st)) st))))

(define tinfloop (term->state (right-x (list-ref parsed-tests 7))))
tinfloop

(displayln "\n\ninterpreting examples from file:")
(step-eval-port (open-input-file "examples.cog"))
;(denote-eval-port (open-input-file "examples.cog"))

;(displayln "")
;(state-interact tstart)
;(displayln "")
;(state-interact tinfloop)

;(left "halted on: #(struct:sym right)")

;> (denote-eval (right-x (list-ref parsed-tests 0)))
;'()
;> (denote-eval (right-x (list-ref parsed-tests 1)))
;'one
;> (denote-eval (right-x (list-ref parsed-tests 2)))
;'two
;>

;> (define pe (penv-vars-add (penv-vars-add (penv-syntax-add penv-empty 'x 'y) 'z) 'w))
;> (penv-syntax-rename pe 'x 'y)
;(right
 ;(penv
   ;(dict (list (cons 'y (just 'y)) (cons 'x (nothing)) (cons 'x (just 'y))))
   ;'(w z)))
;
