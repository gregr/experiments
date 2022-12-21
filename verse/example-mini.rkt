#lang racket/base
(require "mini.rkt" racket/pretty)

(define (show A B)
  (newline)
  (pretty-write A)
  (pretty-write '==>*)
  (pretty-write B))

(for-each
  ;(lambda (E) (time (show E (trace-run 0 E))))  ; view the initial "start" expression without taking any steps
  ;(lambda (E) (time (show E (trace-run* E))))   ; show the entire "step" history
  (lambda (E) (time (show E (run* E))))          ; show only the final result
  '(
    ;;;;;;;;;;;;;
    ;;; Basic ;;;
    ;;;;;;;;;;;;;

    #t

    (cons 'a 'b)

    (all (exist (X)
           (== X (alt 1 2))
           (cons X X)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; From the VC paper ;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;

    (exist (x y z)
      (== x (cons y 3))
      (== x (cons 2 z))
      y)

    (exist (first)
      (== first (lambda (x) (exist (a b) (== x (cons a b)) a)))
      (exist (x y)
        (== x (cons y 5))
        (== (first x) 2)
        y))

    ;; Simplification of the previous example:
    (exist (first)
      (== first (lambda (x) (exist (a b) (== x (cons a b)) a)))
      (exist (y)
        (== (first (cons y 5)) 2)
        y))

    (exist (x y)
      (== x (+ 3 y))
      (== y 7)
      x)

    (all (exist (x)
           (== x (alt 7 5)) (cons 3 x)))

    (all (exist (x y)
           (== x (alt 3 4))
           (== y (alt 20 30))
           (cons x y)))

    (all (exist (x)
           (alt
             (begin (== x 3) (+ x 1))
             (begin (== x 4) (* x 2)))))

    (all (exist (append)
           (== append
               (lambda (xs ys)
                 (alt
                   (begin (== xs '()) ys)
                   (exist (x xrest)
                     (== xs (cons x xrest))
                     (cons x (append xrest ys))))))
           (exist (single)
             (== single (list 1))
             (exist (zs)
               (== (append zs single) single)
               zs))))

    ;; A more miniKanren-like example of running backwards:
    (all (exist (append)
           (== append
               (lambda (xs ys)
                 (alt
                   (begin (== xs '()) ys)
                   (exist (x xrest)
                     (== xs (cons x xrest))
                     (cons x (append xrest ys))))))
           (exist (as bs)
             (== (append as bs) (list 1 2 3))
             (list as bs))))

    ;; Unground list elements are fine:
    (all (exist (append)
           (== append
               (lambda (xs ys)
                 (alt
                   (begin (== xs '()) ys)
                   (exist (x xrest)
                     (== xs (cons x xrest))
                     (cons x (append xrest ys))))))
           (exist (Q as bs)
             (== (append as bs) (list 1 Q 3))
             (list as bs))))

    (all (exist (x y)
           (== y ((one (alt (begin (== x 0) (lambda () 3))
                            (lambda () 4)))))
           (== x 7)
           y))

    (all (exist (t)
           (== t (vector 10 27 32))
           (vector-ref t (alt 1 0 1))))

    (all (exist (t)
           (== t (vector 10 27 32))
           (exist (i) (vector-ref t i))))

    (one (alt 1 (exist (loop)
                  (== loop (lambda () (loop)))
                  (loop))))

    ; Infinite loop (we can sometimes detect loops and stop):
    ;(all (alt 1 (exist (loop)
    ;              (== loop (lambda () (loop)))
    ;              (loop))))

    (for/exist (x) (begin (== x (alt 2 3 5))
                          (> x 2))
               (+ x 1))

    (for/exist (x y) (begin (== x (alt 10 20))
                            (== y (alt 1 2 3)))
               (+ x y))

    (all (for/exist (x) (== x (alt 10 20))
                    (alt x (+ x 1))))

    (all (exist (y)
           (== y (+ 3 4))
           ((lambda (x) (+ x 1)) y)))

    (all (exist (x)
           (== (vector-ref (vector 2 3 2 7 9) x) 2)
           x))

    (exist (x)
      (== x (if (begin (== x 0)
                       (> x 1))
                33
                55)))

    (all (exist (x y)
           (== y (alt (begin (== x 3) (* x 2))
                      (== x 4)))
           (vector (+ x 1) y)))

    (all (exist (x)
           (if (> x 0) 55 44)
           (== x 1)
           (alt 77 99)))

    ;; Known to get stuck:
    (all (exist (f)
           (f)
           (alt (== #t #f) (== 3 (alt 1 3)))))

    (all (exist (f x y)
           (== f (lambda (p) (== x 7) p))
           (== y (if (> x 0) 7 8))
           (f y)))

    (all (+ 3 (alt 20 30)))

    (all (alt 1 7 2))

    (exist (i) (vector-ref (vector 1 7 2) i))
    ))
