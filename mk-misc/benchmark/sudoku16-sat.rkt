#lang racket/base
(provide
  sudoku16-solve1
  sudoku16-solve
  )
(require "sat1.rkt" racket/list racket/pretty)

;;; Sudoku16 representation where each x is an integer in 1..16:
;;; ((x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x)
;;;  (x x x x x x x x x x x x x x x x))

;; SAT representation:
;; - one propositional variable for each pair of cell and possible numeric value
;;   - (* 16 16 16) = 4096 variables for N=16

(define N 16)
(define (neg x)                         (* -1 x))
(define (cell-value->var row col value) (+ (* 256 (- row 1)) (* 16 (- col 1)) (- value 1) 1))

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
                         (cons row0       col0) (cons row0       (+ col0 1)) (cons row0       (+ col0 2)) (cons row0       (+ col0 3))
                         (cons (+ row0 1) col0) (cons (+ row0 1) (+ col0 1)) (cons (+ row0 1) (+ col0 2)) (cons (+ row0 1) (+ col0 3))
                         (cons (+ row0 2) col0) (cons (+ row0 2) (+ col0 1)) (cons (+ row0 2) (+ col0 2)) (cons (+ row0 2) (+ col0 3))
                         (cons (+ row0 3) col0) (cons (+ row0 3) (+ col0 1)) (cons (+ row0 3) (+ col0 2)) (cons (+ row0 3) (+ col0 3)))
                        (loop (+ col0 4)))
                       '()))
                 (loop (+ row0 4)))
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
     ;; TODO: update this count for 16x16
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
     ;; TODO: update this count for 16x16
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

(define (sudoku16-solve1 row*)
  (pretty-write (length (make-sudoku-sat-clause* row*)))
  (sudoku-reify (solve1 (make-sudoku-sat-clause* row*))))

(define (sudoku16-solve row*)
  (s-map sudoku-reify (solve (make-sudoku-sat-clause* row*))))

;; Our basic SAT solver is too slow to solve this puzzle in a reasonable amount of time:
(define board.1 '((9 10 0 5 0 4 0 2 0 14 11 3 0 7 0 0)
                  (0 0 0 14 11 0 0 12 0 15 0 6 13 0 0 9)
                  (0 0 2 13 16 0 6 0 0 0 0 0 15 12 0 0)
                  (7 0 0 0 0 0 5 0 0 0 0 0 4 3 0 11)
                  (13 0 0 0 0 10 0 0 0 5 0 2 0 0 11 0)
                  (0 5 0 0 0 0 7 0 0 0 0 0 0 0 0 15)
                  (8 0 9 0 0 0 0 0 15 0 0 0 7 0 0 6)
                  (0 0 0 0 0 0 0 0 0 0 8 0 1 16 5 0)
                  (0 0 0 7 0 0 0 0 8 13 0 0 0 10 0 0)
                  (14 9 8 0 0 15 0 7 0 0 6 11 0 0 0 0)
                  (0 0 3 0 1 9 0 6 0 0 15 0 16 0 0 0)
                  (5 6 0 0 3 16 11 0 9 4 7 10 0 0 14 0)
                  (0 0 0 0 0 0 0 11 4 8 3 1 0 0 0 16)
                  (0 0 11 0 14 6 0 13 0 0 0 0 5 0 0 0)
                  (0 2 14 1 5 0 0 0 0 11 16 0 0 0 13 0)
                  (0 0 0 15 9 0 12 0 13 2 0 0 0 0 0 0)))

(for-each (lambda (board) (pretty-write (time (sudoku16-solve1 board))))
          (list
           board.1
           ))
