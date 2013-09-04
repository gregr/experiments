#lang racket
(require readline)
(require "util.rkt")
(require "base.rkt")

(define eval-port (make-parameter denote-eval-port))
(define program-source (make-parameter (current-input-port)))

(command-line
 #:once-each
 (("-f" "--file") filename "Read program terms from file"
                  (program-source (open-input-file filename)))
 #:once-any
 (("-s" "--small-step") "Choose small-step interpretation"
                        (eval-port step-eval-port))
 (("-d" "--denotational") "Choose denotational interpretation"
                          (eval-port denote-eval-port)))

(match ((eval-port) (program-source))
  ((left err) (displayln err))
  ((right result) result))
