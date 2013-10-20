#lang racket

(require "util.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; abstract syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(data value-bit
  (b-0 ())
  (b-1 ()))

(data term-value
  (bit  (b))
  (uno  ())
  (pair (l r))
  (bvar (idx))
  (lam  (body)))

(data term-action-2
  (pair-access ())
  (lam-apply   ()))

(data term
  (value     (v))
  (action-2  (act t0 t1)))
