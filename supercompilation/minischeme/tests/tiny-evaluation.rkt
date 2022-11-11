#lang racket/base
(require "../mini.rkt" "../tiny.rkt" racket/pretty racket/runtime-path)

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

;(pretty-write self-apply.tiny)

;; self-application via Tiny evaluation
(displayln "self-apply via Tiny: desugar the parse-mini code")
(define parse-mini-2.tiny (time (eval-tiny-expression self-apply.tiny)))
;(pretty-write parse-mini-2.tiny)
(E.tiny?! '() parse-mini-2.tiny)

(displayln "Are the self-application results equal?")
(equal? parse-mini.tiny parse-mini-2.tiny)
