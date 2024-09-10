#lang racket/base
(provide
  sudoku-solve1
  sudoku-solve
  )
(require "sat1.rkt" racket/list racket/pretty)

;;; Sudoku representation where each x is an integer in 1..9:
;;; ((x x x x x x x x x)
;;;  (x x x x x x x x x)
;;;  (x x x x x x x x x)
;;;  (x x x x x x x x x)
;;;  (x x x x x x x x x)
;;;  (x x x x x x x x x)
;;;  (x x x x x x x x x)
;;;  (x x x x x x x x x)
;;;  (x x x x x x x x x))

;; SAT representation:
;; - one propositional variable for each pair of cell and possible numeric value
;;   - (* 9 9 9) = 729 variables for N=9
(define N 9)
(define (neg x)                         (* -1 x))
(define (cell-value->var row col value) (+ (* 100 row) (* 10 col) value))

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
                         (cons row0       col0) (cons row0       (+ col0 1)) (cons row0       (+ col0 2))
                         (cons (+ row0 1) col0) (cons (+ row0 1) (+ col0 1)) (cons (+ row0 1) (+ col0 2))
                         (cons (+ row0 2) col0) (cons (+ row0 2) (+ col0 1)) (cons (+ row0 2) (+ col0 2)))
                        (loop (+ col0 3)))
                       '()))
                 (loop (+ row0 3)))
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
     ;; - for each group (row, column, or block)
     ;;   - for each value
     ;;     - one clause asserting that at least one cell in the group has that value
     (let loop ((group* group*))
       (if (null? group*)
           '()
           (append
            (let loop ((value 1))
              (if (<= value N)
                  (cons (let loop ((cell* (car group*)))
                          (if (null? cell*)
                              '()
                              (let* ((cell (car cell*))
                                     (row  (car cell))
                                     (col  (cdr cell)))
                                (cons (cell-value->var row col value) (loop (cdr cell*))))))
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

(define (sudoku-solve1 row*)
  (sudoku-reify (solve1 (make-sudoku-sat-clause* row*))))

(define (sudoku-solve row*)
  (s-map sudoku-reify (solve (make-sudoku-sat-clause* row*))))

;; ?s
(define really-easy.1 '((4 6 9 8 3 2 1 5 7)
                        (7 3 5 1 9 6 2 8 4)
                        (2 8 1 7 4 0 3 9 6)
                        (9 2 6 3 7 8 4 1 5)
                        (1 5 8 4 2 9 7 6 3)
                        (3 4 0 5 6 1 9 2 8)
                        (5 1 3 0 8 4 6 7 2)
                        (6 9 4 2 5 7 0 3 1)
                        (0 7 2 6 1 3 5 4 9)))

;; ?s
(define easy.1 '((0 0 0 0 3 2 0 5 7)
                 (0 0 5 1 0 0 0 0 0)
                 (2 8 1 7 4 5 0 9 6)
                 (0 0 0 0 7 0 0 0 0)
                 (0 0 8 0 0 9 7 6 0)
                 (0 4 0 5 0 1 0 0 8)
                 (5 0 3 9 8 4 0 7 0)
                 (6 0 4 0 5 7 0 3 1)
                 (0 0 2 0 1 0 0 0 9)))

;; ?s
(define easy.2 '((0 3 0 0 4 0 0 0 7)
                 (0 4 0 0 2 1 3 9 0)
                 (1 9 0 0 0 0 0 8 0)
                 (3 7 8 2 0 0 0 1 0)
                 (4 0 0 5 1 8 7 0 0)
                 (0 0 1 4 0 0 0 2 0)
                 (0 1 3 7 0 0 0 4 0)
                 (2 5 7 9 0 0 8 3 0)
                 (9 0 0 0 0 3 0 7 2)))

;; ?s
(define medium.1 '((0 0 0 0 0 7 9 1 0)
                   (0 0 7 0 0 4 6 0 0)
                   (5 0 8 9 0 3 4 0 0)
                   (9 2 0 3 0 5 0 0 0)
                   (8 0 1 7 2 0 0 0 0)
                   (4 0 0 0 0 1 2 5 0)
                   (6 0 0 0 0 0 0 0 0)
                   (0 0 0 1 0 0 0 0 8)
                   (2 1 9 0 3 0 0 0 0)))

;; ?s
(define medium.2 '((7 0 4 0 0 0 6 0 0)
                   (6 0 0 0 9 8 0 0 0)
                   (0 0 0 0 0 0 2 0 9)
                   (1 0 0 4 2 0 5 0 8)
                   (0 7 0 0 0 0 0 4 2)
                   (0 0 2 6 0 0 1 0 0)
                   (4 0 0 8 0 1 0 2 0)
                   (2 6 0 3 4 9 0 5 0)
                   (0 0 0 0 0 0 0 7 0)))

;; ?s
(define grid.50 '((3 0 0 2 0 0 0 0 0)
                  (0 0 0 1 0 7 0 0 0)
                  (7 0 6 0 3 0 5 0 0)
                  (0 7 0 0 0 9 0 8 0)
                  (9 0 0 0 2 0 0 0 4)
                  (0 1 0 8 0 0 0 5 0)
                  (0 0 9 0 4 0 3 0 1)
                  (0 0 0 7 0 2 0 0 0)
                  (0 0 0 0 0 8 0 0 6)))

;; ?s
(define hard.1 '((0 0 8 6 2 7 0 0 9)
                 (0 0 0 5 0 0 0 0 0)
                 (0 3 0 0 9 0 0 0 0)
                 (0 0 6 9 0 0 3 0 2)
                 (0 0 0 0 0 0 9 5 0)
                 (1 0 0 8 0 0 0 0 0)
                 (0 0 0 0 5 2 0 6 3)
                 (4 0 0 0 8 0 0 0 0)
                 (0 0 0 3 0 0 2 4 0)))

;; ?s
(define hard.2 '((0 0 5 0 7 0 0 0 0)
                 (0 0 6 0 0 9 0 0 0)
                 (0 9 0 0 5 0 0 0 0)
                 (0 7 9 0 0 2 5 0 0)
                 (0 0 0 0 6 0 9 1 0)
                 (0 0 0 5 0 8 4 0 0)
                 (0 0 0 0 0 0 0 0 3)
                 (0 5 0 6 0 0 0 0 1)
                 (9 1 0 4 0 7 0 0 5)))

;; ?s
(define very-hard.1 '((0 5 0 0 0 8 2 6 9)
                      (0 0 2 0 4 3 0 0 0)
                      (0 0 9 0 0 0 0 0 0)
                      (0 0 7 0 0 0 0 0 0)
                      (0 0 0 0 9 0 0 4 0)
                      (5 0 3 0 0 0 0 9 0)
                      (0 0 0 0 2 4 6 0 5)
                      (6 0 0 0 0 0 0 0 3)
                      (0 4 0 0 8 0 0 0 0)))

;; ?s
(define very-hard.2 '((0 4 0 0 0 0 0 0 0)
                      (0 0 6 0 1 0 4 0 0)
                      (0 0 1 0 3 7 0 0 0)
                      (0 0 0 0 0 8 0 1 0)
                      (0 6 0 7 0 0 0 5 0)
                      (0 0 8 0 9 0 0 7 0)
                      (0 5 0 1 0 0 8 6 0)
                      (0 0 0 3 0 0 0 9 0)
                      (9 0 0 0 0 0 2 0 0)))

(define board.empty '((0 0 0 0 0 0 0 0 0)
                      (0 0 0 0 0 0 0 0 0)
                      (0 0 0 0 0 0 0 0 0)
                      (0 0 0 0 0 0 0 0 0)
                      (0 0 0 0 0 0 0 0 0)
                      (0 0 0 0 0 0 0 0 0)
                      (0 0 0 0 0 0 0 0 0)
                      (0 0 0 0 0 0 0 0 0)
                      (0 0 0 0 0 0 0 0 0)))

;; TODO: the naive SAT solver can only solve ridiculously easy problems in a reasonable amount of time.
(for-each (lambda (board) (pretty-write (time (sudoku-solve1 board))))
          (list
           really-easy.1
           ;easy.1
           ;easy.2
           ;medium.1
           ;medium.2
           ;grid.50
           ;hard.1
           ;hard.2
           ;very-hard.1
           ;very-hard.2
           ))
