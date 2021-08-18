#lang racket/base
(require "dk.rkt" "evalo.rkt" racket/pretty)

(define-relation (appendo xs ys zs)
  (conde ((== xs '()) (== zs ys))
         ((fresh (a d ws)
            (== xs (cons a d))
            (== zs (cons a ws))
            (appendo d ys ws)))))

(define-relation (reverseo ys sy)
  (conde
    ((== '() ys) (== '() sy))
    ((fresh (first rest prefix)
       (== `(,first . ,rest) ys)
       ;; With a typical search strategy, there is no refutationally complete
       ;; ordering of the following two goals.  This ordering works well when
       ;; running in the forward direction, but not in the backward direction.
       (reverseo rest prefix)
       (appendo prefix `(,first) sy)))))

(define-relation (nevero x) (nevero x))
(define-relation (alwayso x)
  (conde ((== #t x))
         ((alwayso x))))
(define-relation (sometimeso x)
  (conde ((nevero x))
         ((alwayso x))))

(define-relation (color c)
  (conde ((== c 'red))
         ((== c 'green))
         ((== c 'blue))
         ((== c 'cyan))
         ((== c 'magenta))
         ((== c 'yellow))
         ((== c 'black))
         ((== c 'white))))
(define-relation (shape s)
  (conde ((== s 'circle))
         ((== s 'triangle))
         ((== s 'rectangle))
         ((== s 'pentagon))
         ((== s 'hexagon))))
(define-relation (shape-or-color sc)
  (conde ((shape sc)) ((color sc))))

(define-syntax test
  (syntax-rules ()
    ((_ name e-actual e-expected)
     (time (begin
             (printf "Testing ~s:\n" name)
             (let ((actual e-actual) (expected e-expected))
               (unless (equal? actual expected)
                 (printf "FAILURE\nEXPECTED: ~s\nACTUAL: ~s\n"
                         expected actual))))))))

(test 'basic-1
  (run* (q) (== 5 q))
  '((5)))

(test 'appendo-1
  (run* (xs ys) (appendo xs ys '(a b c d)))
  '((()        (a b c d))
    ((a)       (b c d))
    ((a b)     (c d))
    ((a b c)   (d))
    ((a b c d) ())))

(test 'reverseo-forward
  (run* (xs) (reverseo '(1 2 3 4 5) xs))
  '(((5 4 3 2 1))))

(test 'reverseo-backward
  (run* (xs) (reverseo xs '(1 2 3 4 5)))
  ;(run 1 (xs) (reverseo xs '(1 2 3 4 5)))
  '(((5 4 3 2 1))))

(test 'sometimeso-1
  (run 5 (q) (sometimeso q))
  '((#t) (#t) (#t) (#t) (#t)))

(test 'choices-1
  (run 20 (sc) (shape-or-color sc))
  '((circle)
    (triangle)
    (red)
    (green)
    (rectangle)
    (blue)
    (pentagon)
    (cyan)
    (hexagon)
    (magenta)
    (yellow)
    (black)
    (white)))
(test 'choices-2
  (run 20 (s c) (shape s) (color c))
  '((circle red)
    (circle green)
    (triangle red)
    (circle blue)
    (triangle green)
    (circle cyan)
    (circle magenta)
    (triangle blue)
    (circle yellow)
    (rectangle red)
    (circle black)
    (triangle cyan)
    (circle white)
    (rectangle green)
    (triangle magenta)
    (triangle yellow)
    (rectangle blue)
    (pentagon red)
    (triangle black)
    (triangle white)))

(test 'evalo-literal
  (run 1 (e) (evalo e 5))
  '(((quote 5))))

;; cpu time: 295 real time: 319 gc time: 97
(test 'evalo-quine
  (run 1 (e) (evalo e e))
  '(((app (lambda (cons (quote app)
                        (cons (var ())
                              (cons (cons (quote quote)
                                          (cons (var ()) (quote ()))) (quote ())))))
          (quote (lambda (cons (quote app)
                               (cons (var ())
                                     (cons (cons (quote quote)
                                                 (cons (var ()) (quote ()))) (quote ()))))))))))

(displayln "\nBeginning slower tests...")
;; cpu time: 9776 real time: 9972 gc time: 4201
(test 'evalo-twine
  (run 1 (p q) (evalo p q) (evalo q p))
  '(((quote (app (lambda (cons (quote quote)
                               (cons (cons (quote app)
                                           (cons (var ())
                                                 (cons (cons (quote quote)
                                                             (cons (var ()) (quote ()))) (quote ())))) (quote ()))))
                 (quote (lambda (cons (quote quote)
                                      (cons (cons (quote app)
                                                  (cons (var ())
                                                        (cons (cons (quote quote)
                                                                    (cons (var ()) (quote ()))) (quote ())))) (quote ())))))))
     (app (lambda (cons (quote quote)
                        (cons (cons (quote app)
                                    (cons (var ())
                                          (cons (cons (quote quote)
                                                      (cons (var ()) (quote ()))) (quote ())))) (quote ()))))
          (quote (lambda (cons (quote quote)
                               (cons (cons (quote app)
                                           (cons (var ())
                                                 (cons (cons (quote quote)
                                                             (cons (var ()) (quote ()))) (quote ())))) (quote ())))))))))

(displayln "\nThe next test may take many seconds...")
;; cpu time: 219963 real time: 224929 gc time: 58448
(test 'evalo-thrine
  (run 1 (p q r) (evalo p q) (evalo q r) (evalo r p))
  '(((quote (quote (app (lambda (cons (quote quote) (cons (cons (quote quote) (cons (cons (quote app) (cons (var ()) (cons (cons (quote quote) (cons (var ()) (quote ()))) (quote ())))) (quote ()))) (quote ()))))
                        (quote (lambda (cons (quote quote) (cons (cons (quote quote) (cons (cons (quote app) (cons (var ()) (cons (cons (quote quote) (cons (var ()) (quote ()))) (quote ())))) (quote ()))) (quote ()))))))))
     (quote (app (lambda (cons (quote quote) (cons (cons (quote quote) (cons (cons (quote app) (cons (var ()) (cons (cons (quote quote) (cons (var ()) (quote ()))) (quote ())))) (quote ()))) (quote ()))))
                 (quote (lambda (cons (quote quote) (cons (cons (quote quote) (cons (cons (quote app) (cons (var ()) (cons (cons (quote quote) (cons (var ()) (quote ()))) (quote ())))) (quote ()))) (quote ())))))))
     (app (lambda (cons (quote quote) (cons (cons (quote quote) (cons (cons (quote app) (cons (var ()) (cons (cons (quote quote) (cons (var ()) (quote ()))) (quote ())))) (quote ()))) (quote ()))))
          (quote (lambda (cons (quote quote) (cons (cons (quote quote) (cons (cons (quote app) (cons (var ()) (cons (cons (quote quote) (cons (var ()) (quote ()))) (quote ())))) (quote ()))) (quote ())))))))))
