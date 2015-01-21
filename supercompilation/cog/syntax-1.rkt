#lang racket
(provide
  std-1
  )

(require
  "syntax-1-bootstrapping.rkt"
  "syntax-1-parsing.rkt"
  gregr-misc/either
  gregr-misc/function
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; syntax-1 program building
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (std-1 prog)
  (let ((proc (right-x (parse-1 penv-init-1
          `(lam (sym? lam? bit? uno? pair?
                 sym-eq? 0b 1b pair pair-access
                 produce error gen-sym)
              ,prog)))))
    (foldl (flip new-lam-apply-1) proc std-1-input)))
