#lang racket
(require readline)
(require "util.rkt")
(require "base.rkt")

(define (interpret-port parse interpret return inp)
  (let ((form (read inp)))
    (if (eq? form eof) (right (void))
      (do either-monad
        term <- (parse form)
        (begin
          (return (interpret term))
          (interpret-port parse interpret return inp))))))

(define (denote-eval-port parse inp)
  (interpret-port
    parse denote-eval (lambda (x) (displayln (format "~v" x))) inp))

(define (step-eval-port parse inp)
  (interpret-port
    parse interact-with (lambda (x) (displayln (left-x x))) inp))

(define eval-port (make-parameter denote-eval-port))
(define program-source (make-parameter (current-input-port)))

(command-line
 #:once-each
 (("-f" "--file") filename "Read program terms from file"
                  (program-source (open-input-file filename)))
 #:once-any
 (("-d" "--denotational") "Choose denotational interpretation"
                          (eval-port denote-eval-port))
 (("-s" "--small-step") "Choose small-step interpretation"
                        (eval-port step-eval-port)))

(match ((eval-port) (curry parse penv-init) (program-source))
  ((left err) (displayln err))
  ((right result) result))