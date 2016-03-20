#lang racket

;(write (current-command-line-arguments))
;(newline)

;(define (rl)
  ;(display "input a line\n")
  ;(read-line))

;;(rl)
;;(rl)

;(define: (something)
  ;ok)

;'((((((((((((((((((((((((()))))))))))))))))))))))))

;(define str10 "01245678")
;(define l10
  ;(list str10 str10 str10 str10 str10
        ;str10 str10 str10 str10 str10))
;(define l7
  ;(list str10 str10 str10 str10 str10
        ;str10 str10 ))
;(define str100 (apply string-append l10))

;(match (list 3 4)
  ;[(list 1 2) 'ok]
  ;[`(,y ,x) (cons y x)])
  ;['(3 x) x])

;(define/match (something a b)
  ;[]
  ;[])

;box
;set-box!
;unbox
;
;eof
;
;port->string
;port->lines
;file->string
;file->lines
;current-input-port
;current-output-port
;current-error-port
;open-input-file
;open-output-file
;open-input-string
;open-output-string
;get-output-string
;close-output-port
;close-input-port
;call-with-input-string
;call-with-output-string
;call-with-input-file
;call-with-output-file
;copy-port
;regexp-match?
;
;read
;
;eval
;
;print
;print-graph
;pretty-print
;pretty-format
;pretty-print-columns
;
;write
;pretty-write
;
;> (define server (tcp-listen 12345))
;> (define-values (c-in c-out) (tcp-connect "localhost" 12345))
;> (define-values (s-in s-out) (tcp-accept server))
;> (display "hello\n" c-out)
;> (close-output-port c-out)
;> (read-line s-in)
;"hello"
;> (read-line s-in)
;#<eof>
;
;> (define-values (p stdout stdin stderr)
    ;(subprocess #f #f #f "/usr/bin/wc" "-w"))
;> (display "a b c\n" stdin)
;> (close-output-port stdin)
;> (read-line stdout)
;"       3"
;> (close-input-port stdout)
;> (close-input-port stderr)
;
;> (define-values (in out) (make-pipe))
;> (display "garbage" out)
;> (close-output-port out)
;> (read-line in)
;"garbage"
