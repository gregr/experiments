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

(define-struct ansi-decorator (mods fg-color bg-color) #:transparent)
(define ansi-dec-default (ansi-decorator '() 'default 'default))
(define (ansi-dec-set-fg color dec)
  (match dec
    ((ansi-decorator mods _ bg-color) (ansi-decorator mods color bg-color))))
(define (ansi-dec-set-bg color dec)
  (match dec
    ((ansi-decorator mods fg-color _) (ansi-decorator mods fg-color color))))
(define (ansi-dec-modify mods dec)
  (match dec
    ((ansi-decorator mods-old fgc bgc)
     (ansi-decorator (append mods-old mods) fgc bgc))))
(define (ansi-decorated decorator str)
  (match decorator
    ((ansi-decorator mods fg-color bg-color)
     (ansi-escaped
       (cons (ansi-fg-color-code fg-color)
             (cons (ansi-bg-color-code bg-color)
                   (map ansi-modifier-code mods)))
       str))))

(define-struct ansi-string (str decorator) #:transparent)
(define (ansi-string-new str) (ansi-string str ansi-dec-default))
(define (ansi-string-redec update astr)
  (match astr
    ((ansi-string str dec) (ansi-string str (update dec)))))
(define (ansi-string-decorated astr)
  (ansi-decorated (ansi-string-decorator astr) (ansi-string-str astr)))
