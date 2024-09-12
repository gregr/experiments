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

;; 5.19s
(define grid.50 '((3 0 0 2 0 0 0 0 0)
                  (0 0 0 1 0 7 0 0 0)
                  (7 0 6 0 3 0 5 0 0)
                  (0 7 0 0 0 9 0 8 0)
                  (9 0 0 0 2 0 0 0 4)
                  (0 1 0 8 0 0 0 5 0)
                  (0 0 9 0 4 0 3 0 1)
                  (0 0 0 7 0 2 0 0 0)
                  (0 0 0 0 0 8 0 0 6)))

;; 8.61s
(define hard.1 '((0 0 8 6 2 7 0 0 9)
                 (0 0 0 5 0 0 0 0 0)
                 (0 3 0 0 9 0 0 0 0)
                 (0 0 6 9 0 0 3 0 2)
                 (0 0 0 0 0 0 9 5 0)
                 (1 0 0 8 0 0 0 0 0)
                 (0 0 0 0 5 2 0 6 3)
                 (4 0 0 0 8 0 0 0 0)
                 (0 0 0 3 0 0 2 4 0)))

;; 116s
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

;; 62s
(define very-hard.2 '((0 4 0 0 0 0 0 0 0)
                      (0 0 6 0 1 0 4 0 0)
                      (0 0 1 0 3 7 0 0 0)
                      (0 0 0 0 0 8 0 1 0)
                      (0 6 0 7 0 0 0 5 0)
                      (0 0 8 0 9 0 0 7 0)
                      (0 5 0 1 0 0 8 6 0)
                      (0 0 0 3 0 0 0 9 0)
                      (9 0 0 0 0 0 2 0 0)))

;; 30s
(define extremely-hard.1 '((0 1 0 0 0 2 5 4 0)
                           (0 9 8 0 0 0 0 0 0)
                           (0 0 0 0 0 0 0 0 0)
                           (6 0 7 0 0 0 3 0 0)
                           (5 0 0 2 0 0 0 0 0)
                           (0 0 0 9 0 0 0 0 0)
                           (0 0 0 0 6 0 0 9 8)
                           (0 0 0 0 3 0 0 0 0)
                           (7 0 0 0 0 0 0 0 0)))

;; 380s
(define extremely-hard.2 '((0 0 0 1 0 2 0 3 0)
                           (8 0 0 7 0 0 0 0 0)
                           (4 0 0 0 0 0 9 0 0)
                           (0 1 0 3 0 0 0 0 0)
                           (0 0 0 0 6 0 4 0 0)
                           (0 0 0 0 0 0 0 0 0)
                           (6 0 9 0 4 0 0 0 0)
                           (0 0 0 0 0 0 0 7 1)
                           (0 0 0 0 5 0 0 0 0)))

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
            extremely-hard.1
            extremely-hard.2
            ))

;(time (run-sudoku board.in))
;    33 collections
;    0.204376376s elapsed cpu time, including 0.011181000s collecting
;    0.204373000s elapsed real time, including 0.011206000s collecting
;    274071200 bytes allocated, including 274380192 bytes reclaimed
;(4 6 9 8 3 2 1 5 7)
;(7 3 5 1 9 6 2 8 4)
;(2 8 1 7 4 5 3 9 6)
;(9 2 6 3 7 8 4 1 5)
;(1 5 8 4 2 9 7 6 3)
;(3 4 7 5 6 1 9 2 8)
;(5 1 3 9 8 4 6 7 2)
;(6 9 4 2 5 7 8 3 1)
;(8 7 2 6 1 3 5 4 9)
;(time (run-sudoku board.in))
;    2 collections
;    0.010825542s elapsed cpu time, including 0.000170000s collecting
;    0.010824000s elapsed real time, including 0.000171000s collecting
;    14445776 bytes allocated, including 16593856 bytes reclaimed
;(8 3 2 6 4 9 1 5 7)
;(7 4 5 8 2 1 3 9 6)
;(1 9 6 3 7 5 2 8 4)
;(3 7 8 2 9 6 4 1 5)
;(4 2 9 5 1 8 7 6 3)
;(5 6 1 4 3 7 9 2 8)
;(6 1 3 7 8 2 5 4 9)
;(2 5 7 9 6 4 8 3 1)
;(9 8 4 1 5 3 6 7 2)
;(time (run-sudoku board.in))
;    6 collections
;    0.037522251s elapsed cpu time, including 0.001143000s collecting
;    0.037521000s elapsed real time, including 0.001150000s collecting
;    51547056 bytes allocated, including 51176832 bytes reclaimed
;(3 4 2 6 8 7 9 1 5)
;(1 9 7 2 5 4 6 8 3)
;(5 6 8 9 1 3 4 2 7)
;(9 2 6 3 4 5 8 7 1)
;(8 5 1 7 2 6 3 4 9)
;(4 7 3 8 9 1 2 5 6)
;(6 8 5 4 7 9 1 3 2)
;(7 3 4 1 6 2 5 9 8)
;(2 1 9 5 3 8 7 6 4)
;(time (run-sudoku board.in))
;    23 collections
;    0.142945624s elapsed cpu time, including 0.005993000s collecting
;    0.142943000s elapsed real time, including 0.006016000s collecting
;    194528624 bytes allocated, including 192354640 bytes reclaimed
;(7 9 4 1 3 2 6 8 5)
;(6 2 1 5 9 8 7 3 4)
;(3 8 5 7 6 4 2 1 9)
;(1 3 9 4 2 7 5 6 8)
;(8 7 6 9 1 5 3 4 2)
;(5 4 2 6 8 3 1 9 7)
;(4 5 3 8 7 1 9 2 6)
;(2 6 7 3 4 9 8 5 1)
;(9 1 8 2 5 6 4 7 3)
;(time (run-sudoku board.in))
;    681 collections
;    5.188113000s elapsed cpu time, including 0.616263000s collecting
;    5.188059000s elapsed real time, including 0.617888000s collecting
;    5710874480 bytes allocated, including 5678872536 bytes reclaimed
;(3 5 1 2 8 6 4 9 7)
;(4 9 2 1 5 7 6 3 8)
;(7 8 6 9 3 4 5 1 2)
;(2 7 5 4 6 9 1 8 3)
;(9 3 8 5 2 1 7 6 4)
;(6 1 4 8 7 3 2 5 9)
;(8 2 9 6 4 5 3 7 1)
;(1 6 3 7 9 2 8 4 5)
;(5 4 7 3 1 8 9 2 6)
;(time (run-sudoku board.in))
;    1050 collections
;    8.605886042s elapsed cpu time, including 1.186783000s collecting
;    8.605831000s elapsed real time, including 1.189613000s collecting
;    8803871376 bytes allocated, including 8803080304 bytes reclaimed
;(5 1 8 6 2 7 4 3 9)
;(2 6 9 5 4 3 7 8 1)
;(7 3 4 1 9 8 5 2 6)
;(8 5 6 9 7 4 3 1 2)
;(3 4 7 2 6 1 9 5 8)
;(1 9 2 8 3 5 6 7 4)
;(9 7 1 4 5 2 8 6 3)
;(4 2 3 7 8 6 1 9 5)
;(6 8 5 3 1 9 2 4 7)
;(time (run-sudoku board.in))
;    8094 collections
;    115.938085583s elapsed cpu time, including 24.049269000s collecting
;    115.938187000s elapsed real time, including 24.076598000s collecting
;    67862130464 bytes allocated, including 67852054280 bytes reclaimed
;(3 4 5 8 7 1 6 2 9)
;(7 2 6 3 4 9 1 5 8)
;(8 9 1 2 5 6 3 7 4)
;(4 7 9 1 3 2 5 8 6)
;(5 3 8 7 6 4 9 1 2)
;(1 6 2 5 9 8 4 3 7)
;(6 8 7 9 1 5 2 4 3)
;(2 5 4 6 8 3 7 9 1)
;(9 1 3 4 2 7 8 6 5)
;(time (run-sudoku board.in))
;    8699 collections
;    153.055432042s elapsed cpu time, including 36.314495000s collecting
;    153.054721000s elapsed real time, including 36.344586000s collecting
;    72934282272 bytes allocated, including 72888455240 bytes reclaimed
;(3 5 4 1 7 8 2 6 9)
;(7 6 2 9 4 3 5 1 8)
;(8 1 9 6 5 2 7 3 4)
;(4 9 7 2 3 1 8 5 6)
;(1 2 6 8 9 5 3 4 7)
;(5 8 3 4 6 7 1 9 2)
;(9 3 1 7 2 4 6 8 5)
;(6 7 8 5 1 9 4 2 3)
;(2 4 5 3 8 6 9 7 1)
;(time (run-sudoku board.in))
;    5087 collections
;    61.871641792s elapsed cpu time, including 12.232564000s collecting
;    61.871278000s elapsed real time, including 12.249296000s collecting
;    42655118128 bytes allocated, including 42703884160 bytes reclaimed
;(3 4 5 9 6 2 7 8 1)
;(7 2 6 8 1 5 4 3 9)
;(8 9 1 4 3 7 5 2 6)
;(4 7 9 6 5 8 3 1 2)
;(1 6 2 7 4 3 9 5 8)
;(5 3 8 2 9 1 6 7 4)
;(2 5 4 1 7 9 8 6 3)
;(6 8 7 3 2 4 1 9 5)
;(9 1 3 5 8 6 2 4 7)
;(time (run-sudoku board.in))
;    2623 collections
;    29.800163291s elapsed cpu time, including 5.333381000s collecting
;    29.800422000s elapsed real time, including 5.340819000s collecting
;    21989139952 bytes allocated, including 21979490456 bytes reclaimed
;(3 1 6 8 9 2 5 4 7)
;(4 9 8 3 5 7 1 6 2)
;(2 7 5 6 4 1 9 8 3)
;(6 2 7 4 8 5 3 1 9)
;(5 4 9 2 1 3 8 7 6)
;(8 3 1 9 7 6 4 2 5)
;(1 5 3 7 6 4 2 9 8)
;(9 6 2 1 3 8 7 5 4)
;(7 8 4 5 2 9 6 3 1)
;(time (run-sudoku board.in))
;    18209 collections
;    380.161110541s elapsed cpu time, including 100.642735000s collecting
;    380.163640000s elapsed real time, including 100.710321000s collecting
;    152675654320 bytes allocated, including 152617784536 bytes reclaimed
;(7 9 6 1 8 2 5 3 4)
;(8 5 3 7 9 4 1 2 6)
;(4 2 1 5 3 6 9 8 7)
;(9 1 4 3 7 5 2 6 8)
;(3 8 7 2 6 9 4 1 5)
;(2 6 5 4 1 8 7 9 3)
;(6 7 9 8 4 1 3 5 2)
;(5 4 8 9 2 3 6 7 1)
;(1 3 2 6 5 7 8 4 9)
