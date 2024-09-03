(load "../mk/mk.scm")

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

(define (other-=/= x x*)
  (conde
    ((== x* '()))
    ((fresh (y y*)
       ;(== (cons y y*) x*)
       (== x* (cons y y*))  ; faster-mk performance degrades by ~100x with this order
       (=/= x y)
       (other-=/= x y*)))))

(define (all-=/= x*)
  (conde
    ((== x* '()))
    ((fresh (y y*)
       ;(== (cons y y*) x*)
       (== x* (cons y y*))  ; faster-mk performance degrades by ~4x with this order
       (other-=/= y y*)
       (all-=/= y*)))))

(define (all-=/=* x**)
  (conde
    ((== x** '()))
    ((fresh (y* y**)
       ;(== (cons y* y**) x**)
       (== x** (cons y* y**))  ; no performance degradation with this order
       (all-=/= y*)
       (all-=/=* y**)))))

(define (appendo x* y x*y)
  (conde
    ((== x* '()) (== y x*y))
    ((fresh (x z* z*y)
       (== x* (cons x z*))
       (== x*y (cons x z*y))
       (appendo z* y z*y)))))

(define (membero x x*)
  (conde
    ((fresh (y*)
       (== x* (cons x y*))))
    ((fresh (y y*)
       (== x* (cons y y*))
       (=/= x y)
       (membero x y*)))))

(define nineo
  (let ((1..9 (cdr (iota 10))))
    (lambda (x) (membero x 1..9))))

(define (nine*o x*)
  (conde
    ((== x* '()))
    ((fresh (a d)
       (== x* (cons a d))
       (nineo a)
       (nine*o d)))))

(define (rowo row)
  (fresh (a b c d e f g h i)
    (== row (list a b c d e f g h i))))

(define (row*o row*)
  (conde
    ((== row* '()))
    ((fresh (a d)
       (== row* (cons a d))
       (rowo a)
       (row*o d)))))

(define (cons*o a* d* ad*)
  (conde
    ((== a* '()) (== d* '()) (== ad* '()))
    ((fresh (a d a0* d0* y*)
       (== ad* (cons (cons a d) y*))
       (== a* (cons a a0*))
       (== d* (cons d d0*))
       (cons*o a0* d0* y*)))))

(define (transposeo x** y**)
  (conde
    ((fresh (w**)
       (== x** (cons '() w**))
       (== y** '())))
    ((fresh (w* w** z**)
       (== y** (cons w* z**))
       (cons*o w* w** x**)
       (transposeo w** z**)))))

(define (flatteno x** y*)
  (conde
    ((== x** '()) (== y* '()))
    ((fresh (u* u** v*)
       (== x** (cons u* u**))
       (flatteno u** v*)
       (appendo u* v* y*)))))

(define (block*o t* three.in three.out)
  (fresh (t0* t1* t2* rest0* rest1*)
    (cons*o t0* rest0* three.in)
    (cons*o t1* rest1* rest0*)
    (cons*o t2* three.out rest1*)
    (flatteno (list t0* t1* t2*) t*)))

(define (block*3o three.in block*)
  (fresh (t0* t1* t2* out0 out1 out2)
    (== block* (list t0* t1* t2*))
    (block*o t0* three.in out0)
    (block*o t1* out0 out1)
    (block*o t2* out1 out2)))

(define (block*3x3o three-x-three.in block*)
  (fresh (abc def ghi block0* block1* block2*)
    (== three-x-three.in (list abc def ghi))
    (block*3o abc block0*)
    (block*3o def block1*)
    (block*3o ghi block2*)
    (flatteno (list block0* block1* block2*) block*)))

(define (sudokuo row*)
  (fresh (a b c d e f g h i col* block* cell*)
    (== row* (list a b c d e f g h i))
    (row*o row*)
    (transposeo row* col*)
    (block*3x3o (list (list a b c)
                      (list d e f)
                      (list g h i))
                block*)
    (flatteno row* cell*)
    (all-=/=* row*)
    (all-=/=* col*)
    (all-=/=* block*)
    (nine*o cell*)))

;; 0.2s
(define easy.1 '((0 0 0 0 3 2 0 5 7)
                 (0 0 5 1 0 0 0 0 0)
                 (2 8 1 7 4 5 0 9 6)
                 (0 0 0 0 7 0 0 0 0)
                 (0 0 8 0 0 9 7 6 0)
                 (0 4 0 5 0 1 0 0 8)
                 (5 0 3 9 8 4 0 7 0)
                 (6 0 4 0 5 7 0 3 1)
                 (0 0 2 0 1 0 0 0 9)))

;; 0.011s
(define easy.2 '((0 3 0 0 4 0 0 0 7)
                 (0 4 0 0 2 1 3 9 0)
                 (1 9 0 0 0 0 0 8 0)
                 (3 7 8 2 0 0 0 1 0)
                 (4 0 0 5 1 8 7 0 0)
                 (0 0 1 4 0 0 0 2 0)
                 (0 1 3 7 0 0 0 4 0)
                 (2 5 7 9 0 0 8 3 0)
                 (9 0 0 0 0 3 0 7 2)))

;; 0.038s
(define medium.1 '((0 0 0 0 0 7 9 1 0)
                   (0 0 7 0 0 4 6 0 0)
                   (5 0 8 9 0 3 4 0 0)
                   (9 2 0 3 0 5 0 0 0)
                   (8 0 1 7 2 0 0 0 0)
                   (4 0 0 0 0 1 2 5 0)
                   (6 0 0 0 0 0 0 0 0)
                   (0 0 0 1 0 0 0 0 8)
                   (2 1 9 0 3 0 0 0 0)))

;; 0.14s
(define medium.2 '((7 0 4 0 0 0 6 0 0)
                   (6 0 0 0 9 8 0 0 0)
                   (0 0 0 0 0 0 2 0 9)
                   (1 0 0 4 2 0 5 0 8)
                   (0 7 0 0 0 0 0 4 2)
                   (0 0 2 6 0 0 1 0 0)
                   (4 0 0 8 0 1 0 2 0)
                   (2 6 0 3 4 9 0 5 0)
                   (0 0 0 0 0 0 0 7 0)))

;; 5.23s
(define grid.50 '((3 0 0 2 0 0 0 0 0)
                  (0 0 0 1 0 7 0 0 0)
                  (7 0 6 0 3 0 5 0 0)
                  (0 7 0 0 0 9 0 8 0)
                  (9 0 0 0 2 0 0 0 4)
                  (0 1 0 8 0 0 0 5 0)
                  (0 0 9 0 4 0 3 0 1)
                  (0 0 0 7 0 2 0 0 0)
                  (0 0 0 0 0 8 0 0 6)))

;; 8.62s
(define hard.1 '((0 0 8 6 2 7 0 0 9)
                 (0 0 0 5 0 0 0 0 0)
                 (0 3 0 0 9 0 0 0 0)
                 (0 0 6 9 0 0 3 0 2)
                 (0 0 0 0 0 0 9 5 0)
                 (1 0 0 8 0 0 0 0 0)
                 (0 0 0 0 5 2 0 6 3)
                 (4 0 0 0 8 0 0 0 0)
                 (0 0 0 3 0 0 2 4 0)))

;; 115s
(define hard.2 '((0 0 5 0 7 0 0 0 0)
                 (0 0 6 0 0 9 0 0 0)
                 (0 9 0 0 5 0 0 0 0)
                 (0 7 9 0 0 2 5 0 0)
                 (0 0 0 0 6 0 9 1 0)
                 (0 0 0 5 0 8 4 0 0)
                 (0 0 0 0 0 0 0 0 3)
                 (0 5 0 6 0 0 0 0 1)
                 (9 1 0 4 0 7 0 0 5)))

;; 153s
(define very-hard.1 '((0 5 0 0 0 8 2 6 9)
                      (0 0 2 0 4 3 0 0 0)
                      (0 0 9 0 0 0 0 0 0)
                      (0 0 7 0 0 0 0 0 0)
                      (0 0 0 0 9 0 0 4 0)
                      (5 0 3 0 0 0 0 9 0)
                      (0 0 0 0 2 4 6 0 5)
                      (6 0 0 0 0 0 0 0 3)
                      (0 4 0 0 8 0 0 0 0)))

;; 63s
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

(define (board-converto board.in board.out)
  (conde
    ((== board.in '()) (== board.out '()))
    ((fresh (row.in row*.in row.out row*.out)
       (== board.in (cons row.in row*.in))
       (== board.out (cons row.out row*.out))
       (row-converto row.in row.out)
       (board-converto row*.in row*.out)))))

(define (row-converto row.in row.out)
  (conde
    ((== row.in '()) (== row.out '()))
    ((fresh (d.in a.out d.out)
       (== row.in (cons 0 d.in))
       (== row.out (cons a.out d.out))
       (row-converto d.in d.out)))
    ((fresh (a.in d.in d.out)
       (=/= a.in 0)
       (== row.in (cons a.in d.in))
       (== row.out (cons a.in d.out))
       (row-converto d.in d.out)))))

(define (run-sudoku board.in)
  (run 2 (board.out)
      (board-converto board.in board.out)
      (sudokuo board.out)))

(define (pretty-sudoku board.in)
  (define (print-board board) (for-each (lambda (row) (write row) (newline)) board))
  (let ((answer* (time (run-sudoku board.in))))
    (cond
      ((null? answer*)        (print-board board.in)
                              (error 'pretty-sudoku "no solution"))
      ((< 1 (length answer*)) (print-board board.in)
                              (for-each (lambda (a) (newline) (print-board (car a))) answer*)
                              (error 'pretty-sudoku "multiple solutions"))
      (else                   (print-board (caar answer*))))))

(for-each pretty-sudoku
          (list
            easy.1
            easy.2
            medium.1
            medium.2
            grid.50
            hard.1
            hard.2
            very-hard.1
            very-hard.2
            ))
