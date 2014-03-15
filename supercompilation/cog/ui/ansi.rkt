#lang racket
(provide (all-defined-out))

(define (ansi-escape codes)
  (format "\e[~am" (string-join (map number->string codes) ";")))
(define (ansi-escaped codes str)
  (string-append (ansi-escape codes) str (ansi-escape '(0))))

(define ansi-colors
  (hash-set
    (make-immutable-hash
      (for/list ((name '(black red green yellow blue magenta cyan white))
                 (idx (in-naturals)))
                (cons name idx)))
    'default 9))
(define (ansi-fg-color-code color) (+ 30 (hash-ref ansi-colors color)))
(define (ansi-bg-color-code color) (+ 40 (hash-ref ansi-colors color)))

(define ansi-modifiers
  (hash
    'reset 0
    'bold 1
    'underline 4
    'blink 5
    'invert 7
    'no-bold 22
    'no-underline 24
    'no-blink 25
    'no-invert 27
    ))
(define (ansi-modifier-code modifier) (hash-ref ansi-modifiers modifier))

(define (ansi-decorated mods fg-color bg-color str)
  (ansi-escaped
    (cons (ansi-fg-color-code fg-color)
          (cons (ansi-bg-color-code bg-color)
                (map ansi-modifier-code mods)))
    str))
