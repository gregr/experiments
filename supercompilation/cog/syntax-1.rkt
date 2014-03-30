#lang racket
(require "util.rkt")
(require "syntax-1-bootstrapping.rkt")
(require "syntax-1-parsing.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; syntax-1 program building
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (std-1 prog)
  (let ((proc (right-x (parse-1 penv-init-1
          `(lam (sym? lam? bit? uno? pair?
                 sym-eq? 0b 1b pair pair-access
                 produce error gen-sym)
              ,prog)))))
    (foldl (lambda (arg proc) (new-lam-apply-1 proc arg)) proc
           std-1-input)))
