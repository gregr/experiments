#lang racket/base
(require "../tiny.rkt" racket/pretty)

(define (valid?! stx)
  (let ((valid? (E.tiny? '() stx)))
    (if valid?
        (pretty-write `(success: (valid?! ,stx)))
        (pretty-write `(FAIL: (valid?! ,stx))))))

(define (invalid?! stx)
  (let ((valid? (E.tiny? '() stx)))
    (if valid?
        (pretty-write `(FAIL: (invalid?! ,stx)))
        (pretty-write `(success: (invalid?! ,stx))))))

(valid?!
  '(letrec ((map (lambda (f lst) (if (null? lst)
                                     lst
                                     (cons (call f (car lst))
                                           (call map f (cdr lst))))))
            (plusthree (lambda (x) (+ x '3))))
     (call map plusthree (cons '1 (cons '2 (cons '3 '()))))))

(invalid?!
  '(letrec ((map (lambda (f lst) (if (null? lst)
                                       lst
                                       (cons (call f (car lst))
                                             (call map f (cdr lst))))))
              (plusthree (lambda (x) (+ x '3))))
       (call map plusthree (cons '1 (cons '2 (cons '3))))))

(invalid?!
  '(letrec ((map (lambda (f lst) (if (null? lst)
                                     lst
                                     (cons (call f (car lst))
                                           (call map f (cdr lst))))))
            (plusthree (lambda (x) (+ x '3))))
     (call map plusthree (cons '1 (cons 2 (cons '3 '()))))))
