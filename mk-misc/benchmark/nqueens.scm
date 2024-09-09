(load "../mk/mk.scm")

;;; N-Queens where N=8
;;; This problem instance is too easy to solve.  We need a larger N to stress miniKanren.

;; Board representation where each x is either 0 or 1:
;; ((x x x x x x x x)
;;  (x x x x x x x x)
;;  (x x x x x x x x)
;;  (x x x x x x x x)
;;  (x x x x x x x x)
;;  (x x x x x x x x)
;;  (x x x x x x x x)
;;  (x x x x x x x x))

(define (nqueenso row*)
  (fresh (a b c d e f g h)
    (== row* (list a b c d e f g h))
    (rowo a) (rowo b) (rowo c) (rowo d) (rowo e) (rowo f) (rowo g) (rowo h)
    (project (row*)
      (let fill ((row (car row*)) (row* (cdr row*)))
        (let fill-row ((i 0) (col (car row)) (col* (cdr row)))
          (conde
            ((== col 0)
             (if (null? col*)
                 (== #t #f)
                 (fill-row (+ i 1) (car col*) (cdr col*))))
            ((== col 1)
             (blanko col*)
             (blanko (map (lambda (row) (list-ref row i)) row*))
             (blanko (let loop ((i i) (row* row*))
                       (if (or (null? row*) (= i 7))
                           '()
                           (let ((i (+ i 1)))
                             (cons (list-ref (car row*) i)
                                   (loop i (cdr row*)))))))
             (blanko (let loop ((i i) (row* row*))
                       (if (or (null? row*) (= i 0))
                           '()
                           (let ((i (- i 1)))
                             (cons (list-ref (car row*) i)
                                   (loop i (cdr row*)))))))
             (if (null? row*)
                 (== #t #t)
                 (fill (car row*) (cdr row*))))))))))

(define (rowo row)
  (fresh (a b c d e f g h)
    (== row (list a b c d e f g h))))

(define (blanko pos*)
  (conde
    ((== pos* '()))
    ((fresh (here next*)
       (== pos* (cons here next*))
       (== here 0)
       (blanko next*)))))


(define (pretty-board b)
  (for-each (lambda (row) (write row) (newline)) b))

(define (pretty-answer a)
  (pretty-board (car a))
  (newline))

;(for-each pretty-answer (time (run 2 (board) (nqueenso board))))
;==>
;(time (run 2 ...))
;    1 collection
;    0.007927582s elapsed cpu time, including 0.000481000s collecting
;    0.007927000s elapsed real time, including 0.000484000s collecting
;    12015056 bytes allocated, including 7991008 bytes reclaimed
;(1 0 0 0 0 0 0 0)
;(0 0 0 0 1 0 0 0)
;(0 0 0 0 0 0 0 1)
;(0 0 0 0 0 1 0 0)
;(0 0 1 0 0 0 0 0)
;(0 0 0 0 0 0 1 0)
;(0 1 0 0 0 0 0 0)
;(0 0 0 1 0 0 0 0)
;
;(1 0 0 0 0 0 0 0)
;(0 0 0 0 0 1 0 0)
;(0 0 0 0 0 0 0 1)
;(0 0 1 0 0 0 0 0)
;(0 0 0 0 0 0 1 0)
;(0 0 0 1 0 0 0 0)
;(0 1 0 0 0 0 0 0)
;(0 0 0 0 1 0 0 0)

;(length (time (run 200 (board) (nqueenso board))))
;==>
;(time (run 200 ...))
;    7 collections
;    0.036020750s elapsed cpu time, including 0.001781000s collecting
;    0.036044000s elapsed real time, including 0.001794000s collecting
;    65846928 bytes allocated, including 57759600 bytes reclaimed
;92
