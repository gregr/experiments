#lang racket/base
(provide
  sudoku25-solve1
  sudoku25-solve
  )
(require "sat1.rkt" racket/list racket/pretty)

;;; Sudoku25 representation where each x is an integer in 1..25:
;;; ((x x x x x x x x x x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x x x x x x x x x x))

;; SAT representation:
;; - one propositional variable for each pair of cell and possible numeric value
;;   - (* 25 25 25) = 15625 variables for N=25

(define N 25)
(define (neg x)                         (* -1 x))
(define (cell-value->var row col value) (+ (* 625 (- row 1)) (* 25 (- col 1)) (- value 1) 1))

(define (make-sudoku-sat-clause* row*)
  (let ((group*
         (append
          (let loop ((row 1))
            (if (<= row N)
                (cons (let loop ((col 1))
                        (if (<= col N)
                            (cons (cons row col) (loop (+ col 1)))
                            '()))
                      (loop (+ row 1)))
                '()))
          (let loop ((col 1))
            (if (<= col N)
                (cons (let loop ((row 1))
                        (if (<= row N)
                            (cons (cons row col) (loop (+ row 1)))
                            '()))
                      (loop (+ col 1)))
                '()))
          (let loop ((row0 1))
            (if (<= row0 N)
                (append
                 (let loop ((col0 1))
                   (if (<= col0 N)
                       (cons
                        (list
                         (cons row0       col0) (cons row0       (+ col0 1)) (cons row0       (+ col0 2)) (cons row0       (+ col0 3)) (cons row0       (+ col0 4))
                         (cons (+ row0 1) col0) (cons (+ row0 1) (+ col0 1)) (cons (+ row0 1) (+ col0 2)) (cons (+ row0 1) (+ col0 3)) (cons (+ row0 1) (+ col0 4))
                         (cons (+ row0 2) col0) (cons (+ row0 2) (+ col0 1)) (cons (+ row0 2) (+ col0 2)) (cons (+ row0 2) (+ col0 3)) (cons (+ row0 2) (+ col0 4))
                         (cons (+ row0 3) col0) (cons (+ row0 3) (+ col0 1)) (cons (+ row0 3) (+ col0 2)) (cons (+ row0 3) (+ col0 3)) (cons (+ row0 3) (+ col0 4))
                         (cons (+ row0 4) col0) (cons (+ row0 4) (+ col0 1)) (cons (+ row0 4) (+ col0 2)) (cons (+ row0 4) (+ col0 3)) (cons (+ row0 4) (+ col0 4)))
                        (loop (+ col0 5)))
                       '()))
                 (loop (+ row0 5)))
                '())))))
    (append
     ;; - one unit clause per known cell value in the initial position
     (let loop ((row 1) (row* row*))
       (if (<= row N)
           (append (let loop ((col 1) (col* (car row*)))
                     (if (<= col N)
                         (let ((value   (car col*))
                               (clause* (loop (+ col 1) (cdr col*))))
                           (if (< 0 value)
                               (cons (list (cell-value->var row col value)) clause*)
                               clause*))
                         '()))
                   (loop (+ row 1) (cdr row*)))
           '()))
     ;; - for each pair of cell and value
     ;;   - for each other value
     ;;     - one clause to assert that if the cell has that value, then it does not have the other value
     (let loop ((row 1))
       (if (<= row N)
           (append
            (let loop ((col 1))
              (if (<= col N)
                  (append
                   (let loop ((value 1))
                     (if (< value N)
                         (let ((not-var (neg (cell-value->var row col value))))
                           (append (map (lambda (co-value)
                                          (list not-var (neg (cell-value->var row col co-value))))
                                        (range (+ value 1) (+ N 1)))
                                   (loop (+ value 1))))
                         '()))
                   (loop (+ col 1)))
                  '()))
            (loop (+ row 1)))
           '()))
     ;; - for each cell
     ;;   - one clause to assert the cell has at least one of the values
     (let loop ((row 1))
       (if (<= row N)
           (append (let loop ((col 1))
                     (if (<= col N)
                         (cons (let loop ((value 1))
                                 (if (<= value N)
                                     (cons (cell-value->var row col value)
                                           (loop (+ value 1)))
                                     '()))
                               (loop (+ col 1)))
                         '()))
                   (loop (+ row 1)))
           '()))
     ;; TODO: update this count for 25x25
     ;; With this representation we will have 3240 clauses before considering initial cell values.
     ;; These clauses are correct, but the feedback they provide is too indirect.
     ;; - for each group (row, column, or block)
     ;;   - for each value
     ;;     - one clause asserting that at least one cell in the group has that value
     ;(let loop ((group* group*))
     ;  (if (null? group*)
     ;      '()
     ;      (append
     ;       (let loop ((value 1))
     ;         (if (<= value N)
     ;             (cons (let loop ((cell* (car group*)))
     ;                     (if (null? cell*)
     ;                         '()
     ;                         (let* ((cell (car cell*))
     ;                                (row  (car cell))
     ;                                (col  (cdr cell)))
     ;                           (cons (cell-value->var row col value) (loop (cdr cell*))))))
     ;                   (loop (+ value 1)))
     ;             '()))
     ;       (loop (cdr group*)))))
     ;; TODO: update this count for 25x25
     ;; With this representation we will have 11745 clauses before considering initial cell values.
     ;; Instead of the above, these clauses are more direct, catching bad choices more quickly.
     ;; - "all cells in the group are different"
     ;; - we can say this as "if a cell has a value, no other cell has that value"
     ;; - for each group (row, column, or block)
     ;;   - for each value
     ;;     - for each cell in the group
     ;;       - for each co-cell in the group
     ;;         - if the cell has the value, then the co-cell does not have the value
     (let loop ((group* group*))
       (if (null? group*)
           '()
           (append
            (let loop ((value 1))
              (if (<= value N)
                  (append
                   (let ((cell* (car group*)))
                     (let loop ((cell (car cell*)) (cell* (cdr cell*)))
                       (if (null? cell*)
                           '()
                           (let* ((row     (car cell))
                                  (col     (cdr cell))
                                  (not-var (neg (cell-value->var row col value))))
                             (append
                              (let loop ((cell* cell*))
                                (if (null? cell*)
                                    '()
                                    (let* ((co-cell (car cell*))
                                           (row     (car co-cell))
                                           (col     (cdr co-cell)))
                                      (cons (list not-var (neg (cell-value->var row col value)))
                                            (loop (cdr cell*))))))
                              (loop (car cell*) (cdr cell*)))))))
                   (loop (+ value 1)))
                  '()))
            (loop (cdr group*))))))))

(define (sudoku-reify v=>x)
  (let loop-row ((row 1))
    (if (<= row N)
        (cons (let loop-col ((col 1))
                (if (<= col N)
                    (cons (let loop-val ((value 1))
                            (if (<= value N)
                                (case (vector-ref v=>x (cell-value->var row col value))
                                  ((-1) (loop-val (+ value 1)))
                                  ((1)  value)
                                  (else (error "unassigned cell-value" row col value v=>x)))
                                (error "cell has no value" row col)))
                          (loop-col (+ col 1)))
                    '()))
              (loop-row (+ row 1)))
        '())))

(define (sudoku25-solve1 row*)
  (sudoku-reify (solve1 (make-sudoku-sat-clause* row*))))

(define (sudoku25-solve row*)
  (s-map sudoku-reify (solve (make-sudoku-sat-clause* row*))))

;; Our basic SAT solver is too slow to solve this puzzle in a reasonable amount of time:
(define board.1 '((16 23 7 0 0 24 0 4 0 0 0 10 0 0 0 1 0 18 0 0 8 21 14 0 17)
                  (0 0 20 0 0 19 15 16 0 0 0 0 0 5 24 4 0 2 14 23 0 0 18 0 7)
                  (9 2 12 0 0 0 0 0 20 11 13 0 0 7 0 0 0 0 0 6 0 0 10 25 1)
                  (4 0 0 0 19 0 0 0 14 0 8 0 0 23 21 10 0 9 7 17 0 0 0 0 0)
                  (18 0 0 0 0 0 1 17 10 0 11 15 19 0 0 12 0 20 0 0 0 13 0 0 0)
                  (0 7 1 3 0 0 12 0 0 0 0 0 16 0 0 8 20 11 0 0 0 0 0 9 21)
                  (0 6 0 10 0 0 2 21 18 0 12 19 23 0 0 0 0 0 24 16 1 0 0 14 0)
                  (8 20 0 18 16 11 0 0 24 0 9 0 0 0 3 0 0 0 22 0 12 0 0 10 4)
                  (0 0 0 0 0 1 0 0 9 22 4 0 0 0 0 0 17 23 2 0 24 8 13 0 0)
                  (15 21 0 17 9 8 0 0 0 0 0 18 7 2 0 0 1 0 0 0 0 0 19 0 0)
                  (0 4 0 16 0 0 0 14 0 0 0 22 0 10 0 0 11 17 8 0 21 24 9 0 0)
                  (0 10 11 22 0 0 0 0 0 21 24 3 0 17 1 7 0 0 18 0 5 0 0 0 14)
                  (0 0 0 0 17 10 4 0 0 20 0 0 0 0 0 25 0 0 9 5 16 0 0 0 0)
                  (25 0 0 0 6 0 16 0 0 19 14 13 0 8 9 23 0 0 0 0 0 12 4 18 0)
                  (0 0 23 21 20 0 7 18 13 0 0 4 0 6 0 0 0 3 0 0 0 17 0 19 0)
                  (0 0 10 0 0 0 0 0 17 0 0 7 14 12 0 0 0 0 0 4 25 16 0 22 19)
                  (0 0 14 11 13 0 10 19 12 0 0 0 0 0 16 18 15 0 0 7 0 0 0 0 0)
                  (5 16 0 0 24 0 14 0 0 0 17 0 0 0 11 0 19 0 0 1 6 10 0 4 18)
                  (0 18 0 0 3 21 11 0 0 0 0 0 6 13 22 0 25 24 10 0 0 5 0 23 0)
                  (12 19 0 0 0 0 0 8 2 23 0 0 9 0 0 0 0 0 6 0 0 7 15 11 0)
                  (0 0 0 9 0 0 0 12 0 7 0 0 10 24 14 0 5 19 1 0 0 0 0 0 13)
                  (0 0 0 0 0 22 23 24 0 14 21 12 0 0 17 0 9 0 0 0 10 0 0 0 3)
                  (23 25 18 0 0 4 0 0 0 0 0 9 0 0 20 6 24 0 0 0 0 0 12 1 16)
                  (14 0 19 0 0 15 3 1 0 9 7 5 0 0 0 0 0 8 11 12 0 0 17 0 0)
                  (11 0 16 5 1 0 0 13 0 8 0 0 0 25 0 0 0 10 0 14 0 0 24 2 23)))

(for-each (lambda (board) (pretty-write (time (sudoku25-solve1 board))))
          (list
           board.1
           ))
