#lang racket/base
(provide (struct-out closure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tiny Scheme Grammars ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Atom: a null, boolean, number, or symbol.
; A ::= () | #f | #t | <number> | <symbol>

;; Value: an atom, singleton vector, pair, or procedure (represented as a closure).
; V ::= A | #(V) | (V . V) | #s(closure (<symbol> ...) E ((<symbol> . V) ...))

;; Lambda expression:
; LAM ::= (lambda (<symbol> ...) E)

;; Expression:
; E ::=
;     ;; variable
;     <symbol>
;     ;; constructors
;     | (quote A)
;     | (vector E)
;     | (cons E E)
;     | LAM
;     ;; quasi-constructor
;     | (+ E E)
;     ;; accessors
;     | (vector-ref E E)
;     | (car E)
;     | (cdr E)
;     ;; predicates
;     | (eqv? E E)
;     | (null? E)
;     | (vector? E)
;     | (pair? E)
;     | (number? E)
;     | (symbol? E)
;     | (procedure? E)
;     ;; case analysis
;     | (if E E E)
;     ;; procedure call
;     | (call E E ...)
;     ;; recursive procedure binding
;     | (letrec ((<symbol> LAM) ...) E)

(struct closure (param* body env) #:prefab)
