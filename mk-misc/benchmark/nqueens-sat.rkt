#lang racket/base
(provide
  nqueens-solve1
  nqueens-solve
  )
(require "sat1.rkt" racket/pretty)

;;; N-Queens where N=8

;; Board representation where each x is either 0 or 1:
;; ((x x x x x x x x)
;;  (x x x x x x x x)
;;  (x x x x x x x x)
;;  (x x x x x x x x)
;;  (x x x x x x x x)
;;  (x x x x x x x x)
;;  (x x x x x x x x)
;;  (x x x x x x x x))

;; SAT representation:
;; - one propositional variable for each cell, where truth corresponds to the cell being 1
;;   - 64 variables for N=8
(define N 8)
(define (cell->var row col) (+ (* row 10) col))
(define (neg x)             (* -1 x))

(define (make-nqueens-sat-clause*)
  (define (cell->conflict-clauses* row col)
    (let ((not-var (neg (cell->var row col)))
          (cell*.co-row
           (let loop ((col (+ col 1)))
             (if (<= col N)
                 (cons (cell->var row col) (loop (+ col 1)))
                 '())))
          (cell*.co-col
           (let loop ((row (+ row 1)))
             (if (<= row N)
                 (cons (cell->var row col) (loop (+ row 1)))
                 '())))
          (cell*.co-tlbr
           (let loop ((row (+ row 1)) (col (+ col 1)))
             (if (and (<= row N) (<= col N))
                 (cons (cell->var row col) (loop (+ row 1) (+ col 1)))
                 '())))
          (cell*.co-bltr
           (let loop ((row (+ row 1)) (col (- col 1)))
             (if (and (<= row N) (<= 1 col))
                 (cons (cell->var row col) (loop (+ row 1) (- col 1)))
                 '()))))
      (map (lambda (co-var) (list not-var (neg co-var)))
           (append cell*.co-row cell*.co-col cell*.co-tlbr cell*.co-bltr))))
  (append
   ;; - one clause per row to assert that at least one cell is 1
   (let loop-row ((row 1))
     (if (<= row N)
         (cons (let loop-col ((col 1))
                 (if (<= col N)
                     (cons (cell->var row col) (loop-col (+ col 1)))
                     '()))
               (loop-row (+ row 1)))
         '()))
   ;; - for each cell
   ;;   - for each of its co-row, co-column, co-diagonal cells
   ;;     - one clause to assert that if the cell is 1, the co-cell is 0
   (let loop-row ((row 1))
     (if (<= row N)
         (append (let loop-col ((col 1))
                   (if (<= col N)
                       (append (cell->conflict-clauses* row col) (loop-col (+ col 1)))
                       '()))
                 (loop-row (+ row 1)))
         '()))))

(define (nqueens-reify v=>x)
  (let loop-row ((row 1))
    (if (<= row N)
        (cons (let loop-col ((col 1))
                (if (<= col N)
                    (cons (case (vector-ref v=>x (cell->var row col))
                            ((-1) 0)
                            ((1)  1)
                            (else (error "unassigned cell" row col v=>x)))
                          (loop-col (+ col 1)))
                    '()))
              (loop-row (+ row 1)))
        '())))

(define (nqueens-solve1)
  (nqueens-reify (solve1 (make-nqueens-sat-clause*))))

(define (nqueens-solve)
  (s-map nqueens-reify (solve (make-nqueens-sat-clause*))))

(pretty-write (time (nqueens-solve1)))
;==>
;cpu time: 3780 real time: 3896 gc time: 60
;((1 0 0 0 0 0 0 0)
; (0 0 0 0 1 0 0 0)
; (0 0 0 0 0 0 0 1)
; (0 0 0 0 0 1 0 0)
; (0 0 1 0 0 0 0 0)
; (0 0 0 0 0 0 1 0)
; (0 1 0 0 0 0 0 0)
; (0 0 0 1 0 0 0 0))

;(pretty-write (time (s-take 2 (nqueens-solve))))
;==>
;cpu time: 5464 real time: 5671 gc time: 23
;(((1 0 0 0 0 0 0 0)
;  (0 0 0 0 1 0 0 0)
;  (0 0 0 0 0 0 0 1)
;  (0 0 0 0 0 1 0 0)
;  (0 0 1 0 0 0 0 0)
;  (0 0 0 0 0 0 1 0)
;  (0 1 0 0 0 0 0 0)
;  (0 0 0 1 0 0 0 0))
; ((1 0 0 0 0 0 0 0)
;  (0 0 0 0 0 1 0 0)
;  (0 0 0 0 0 0 0 1)
;  (0 0 1 0 0 0 0 0)
;  (0 0 0 0 0 0 1 0)
;  (0 0 0 1 0 0 0 0)
;  (0 1 0 0 0 0 0 0)
;  (0 0 0 0 1 0 0 0)))
