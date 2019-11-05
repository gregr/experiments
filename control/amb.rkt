#lang racket
(provide (all-defined-out) (all-from-out racket/control))
(require racket/control)

(define s-null      '())
(define (s-null? s) (equal? s s-null))
(define (s-force s) (if (procedure? s) (s-force (s)) s))
;(define (s-first s) (car (s-force s)))
;(define (s-rest s)  (cdr (s-force s)))
;(define-syntax s-cons
  ;(syntax-rules ()
    ;((_ a d) (thunk (cons a d)))))

(define (s-append-eager s t)
  (cond ((procedure? s) (thunk (s-append-eager (s-force s) t)))
        ((s-null? s)    t)
        (else           (cons (car s) (s-append-eager (cdr s) t)))))

(define-syntax s-append*
  (syntax-rules ()
    ((_)         s-null)
    ((_ s t ...) (s-append-eager (thunk s) (s-append* t ...)))))

(define (s-take n s)
  (if (= n 0) '()
    (let ((s (s-force s)))
      (if (s-null? s) '()
        (cons (car s) (s-take (- n 1) (cdr s)))))))

(define-syntax -<
  (syntax-rules ()
    ((_ e ...) (shift k (s-append* (k e) ...)))))

(define-syntax next!
  (syntax-rules ()
    ((_ x)
     (let ((s (s-force x)))
       (if (s-null? s)
         'DONE
         (begin (set! x (cdr s))
                (car s)))))))

(define-syntax do/-<
  (syntax-rules ()
    ((_ e) (thunk (reset (with-handlers
                           (((lambda _ #t)
                             (lambda (x)
                               ;; Optional error handling.
                               (printf "exception: ~s\n" (exn-message x))
                               s-null)))
                           (list e)))))))


;; Testing
(define x (do/-< (/ (-< 1 2 3) (-< 4 5 0))))

(s-take 50 x)

(next! x)
(next! x)
(next! x)
(next! x)
(next! x)
(next! x)
(next! x)
(next! x)
