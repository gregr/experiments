#lang racket
;(require web-server/servlet
         ;web-server/servlet-env)
;(provide/contract (start (request? . -> . response?)))

;(define (start request)
  ;(displayln (url->string (request-uri request)))
  ;(response/xexpr
    ;'(html
       ;(head (title "Something something"))
       ;(body (h1 "It doesn't work yet.")))))

;(serve/servlet start
               ;#:command-line? #t
               ;;#:launch-browser? #f
               ;;#:quit? #f
               ;#:listen-ip #f
               ;#:port 8000
               ;;#:extra-files-paths
               ;;(list (build-path "" "htdocs"))
               ;#:servlet-regexp #rx""
               ;;#:servlet-path
               ;;"/"
               ;)
               ;;"/servlets/stuff.rkt")
