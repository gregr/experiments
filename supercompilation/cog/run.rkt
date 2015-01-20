#lang racket
(require
  "interaction.rkt"
  "semantics-denotational.rkt"
  "syntax-0-parsing.rkt"
  gregr-misc/either
  gregr-misc/monad
  )

(define (interpret-port parse interpret return inp)
  (let ((form (read inp)))
    (if (eq? form eof) (right (void))
      (begin/with-monad either-monad
        term <- (parse form)
        (begin
          (return (interpret term))
          (interpret-port parse interpret return inp))))))

(define (denote-eval-port parse inp)
  (interpret-port
    parse (curry denote-eval noisy-consume)
    (lambda (x) (displayln (format "~v" x))) inp))

(define (step-eval-port parse inp)
  (interpret-port
    parse interact-with (lambda (x) (displayln (left-x x))) inp))

(define eval-port (make-parameter denote-eval-port))
(define program-source (make-parameter (current-input-port)))

(module+ main
  (require readline)
  (command-line
    #:once-each
    (("-f" "--file") filename "Read program terms from file"
                     (program-source (open-input-file filename)))
    #:once-any
    (("-d" "--denotational") "Choose denotational interpretation"
                             (eval-port denote-eval-port))
    (("-s" "--small-step") "Choose small-step interpretation"
                           (eval-port step-eval-port)))

  (match ((eval-port) (curry parse-0 penv-init-0) (program-source))
    ((left err) (displayln err))
    ((right result) result))
  )
