#lang racket
(require "../util.rkt")
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

(record ansi-decorator mods fg-color bg-color)
(define ansi-dec-default (ansi-decorator '() 'default 'default))
(define (ansi-decorated decorator str)
  (match decorator
    ((ansi-decorator mods fg-color bg-color)
     (ansi-escaped
       (cons (ansi-fg-color-code fg-color)
             (cons (ansi-bg-color-code bg-color)
                   (map ansi-modifier-code mods)))
       str))))

(record ansi-string str decorator)
(define (ansi-string-new str) (ansi-string str ansi-dec-default))
(define (ansi-string-decorated astr)
  (ansi-decorated (ansi-string-decorator astr) (ansi-string-str astr)))

; from: http://rosettacode.org/wiki/Keyboard_input/Keypress_check
(define-syntax-rule (with-stty-raw body ...)
  (let ([saved #f])
    (define (stty x) (system (~a "stty " x)) (void))
    (dynamic-wind
      (lambda ()
        (set! saved (with-output-to-string (lambda () (stty "-g"))))
        (stty "raw -echo opost"))
      (lambda () body ...)
      (lambda () (stty saved)))))

(define (maybe-read-char)
  (if (char-ready?)
    (just (read-char))
    (nothing)))
