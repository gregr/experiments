#lang pie

;((the (Pi ((n Nat))
;          (Vec Nat n))
;      (lambda (n)
;        (vec:: 8 vecnil)))
; 1)
;;==> vec:: requires that the length have add1 on top, not n  [,bt for context]

;(the (Vec Nat 1)
;     ((the (-> Nat (Vec Nat 1))
;           (lambda (n)
;             (the (Vec Nat n) (vec:: 8 vecnil))))
;      1))
;;==> vec:: requires that the length have add1 on top, not n  [,bt for context]

(claim n Nat)
(define n 1)
(the (Vec Nat n) (vec:: 8 vecnil))
