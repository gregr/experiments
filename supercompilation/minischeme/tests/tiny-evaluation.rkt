#lang racket/base
(require "../mini.rkt" "mk.rkt" "../tiny.rkt" racket/pretty racket/runtime-path)

(define-runtime-path path.here ".")

(define (read* in)
  (let loop ()
    (let ((x (read in)))
      (if (eof-object? x)
          '()
          (cons x (loop))))))

(define mini.scm (call-with-input-file (build-path path.here "../mini.scm") read*))

(define parse-mini.scm `((let () . ,(append mini.scm (list 'parse-mini-scm-program)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; self-desugaring example ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; self-application via Racket evaluation
(displayln "self-apply via Racket: desugar the parse-mini code")
(define parse-mini.tiny (time (parse-mini-scm-program parse-mini.scm)))
;(pretty-write parse-mini.tiny)
(E.tiny?! '() parse-mini.tiny)

(displayln "desugar a quotation of the parse-mini code to pass as an argument")
(define quoted-parse-mini.tiny (time (parse-mini-scm-program `((quote ,parse-mini.scm)))))
(E.tiny?! '() quoted-parse-mini.tiny)

(define self-apply.tiny `(call ,parse-mini.tiny ,quoted-parse-mini.tiny))
(E.tiny?! '() self-apply.tiny)

; self-application via Tiny evaluation
(displayln "self-apply via Tiny: desugar the parse-mini code")
(define parse-mini-2.tiny (time (eval-tiny-expression self-apply.tiny)))
(E.tiny?! '() parse-mini-2.tiny)

(displayln "Are the self-application results equal?")
(equal? parse-mini.tiny parse-mini-2.tiny)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; miniKanren example ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mk.scm   (call-with-input-file (build-path path.here "mk.scm")      read*))

(define parse-mk.scm (append mini.scm (append mk.scm (list 'parse-mk-program))))

(define test-mk.scm
  '((define-relation (appendo x* y* x*y*)
      (conde
        ((== x* '()) (== x*y* y*))
        ((fresh (a a* a*y*)
           (== x*   (cons a a*))
           (== x*y* (cons a a*y*))
           (appendo a* y* a*y*)))))
    (list
      `((run* (x) (== 5 5)) ==> ,(run* (x) (== 5 5)))
      `((run* (x) (== 77 x)) ==> ,(run* (x) (== 77 x)))
      `((run* (x* y*) (appendo x* y* '(a b c d e))) ==> ,(run* (x* y*) (appendo x* y* '(a b c d e)))))))

(displayln "desugar the miniKanren parser")
(define parse-mk.tiny (time (parse-mini-scm-program parse-mk.scm)))
;(pretty-write parse-mk.tiny)
(E.tiny?! '() parse-mk.tiny)

(displayln "parse a miniKanren test")
(define test-mk.tiny (time (parse-mk-program test-mk.scm)))
;(pretty-write test-mk.tiny)
(E.tiny?! '() test-mk.tiny)

(displayln "test miniKanren via Tiny")
(pretty-write (time (eval-tiny-expression test-mk.tiny)))

(displayln "desugar a quotation of the parse-mk code to pass as an argument")
(define quoted-parse-mk.tiny (time (parse-mini-scm-program `((quote ,parse-mk.scm)))))
(E.tiny?! '() quoted-parse-mk.tiny)

(displayln "desugar the miniKanren parser via Tiny")
(define parse-mk-2.tiny (time (eval-tiny-expression `(call ,parse-mini.tiny ,quoted-parse-mk.tiny))))
(E.tiny?! '() parse-mk-2.tiny)

(displayln "Are the parse-mk results equal?")
(equal? parse-mk.tiny parse-mk-2.tiny)

(displayln "desugar a quotation of the test-mk code to pass as an argument")
(define quoted-test-mk.tiny (time (parse-mini-scm-program `((quote ,test-mk.scm)))))
(E.tiny?! '() quoted-test-mk.tiny)

(displayln "parse a miniKanren test via Tiny")
(define test-mk-2.tiny (time (eval-tiny-expression `(call ,parse-mk.tiny ,quoted-test-mk.tiny))))
(E.tiny?! '() test-mk-2.tiny)

(displayln "Are the test-mk parsings equal?")
(equal? test-mk.tiny test-mk-2.tiny)
