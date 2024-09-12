(load "mk/mk.scm")

(define (ltake n xs)
  (if (or (= n 0) (null? xs)) '()
    (cons (car xs) (ltake (- n 1) (cdr xs)))))
(define (ldrop n xs)
  (if (or (= n 0) (null? xs)) xs
    (ldrop (- n 1) (cdr xs))))

(define (board-parts board)
  (unless (= 81 (length board))
    (error 'run-sudoku (format "invalid board: ~s" board)))
  (let ()
    (define rows
      (let loop ((board board))
        (if (null? board) '()
          (cons (ltake 9 board)
                (loop (ldrop 9 board))))))
    (define cols (apply map list rows))
    (define (block x y)
      (apply append (map (lambda (r) (ltake 3 (ldrop x r)))
                         (ltake 3 (ldrop y rows)))))
    (define offsets (map (lambda (n) (* 3 n)) '(0 1 2)))
    (define blocks
      (apply append (map (lambda (y) (map (lambda (x) (block x y))
                                          offsets))
                         offsets)))
    (list rows cols blocks)))

(define (board-substitute as bs)
  (map (lambda (i)
         (define a (list-ref as i))
         (define b (list-ref bs i))
         (if (= a 0) b a))
       (iota (length as))))

(define (=/=-allo x xs)
  (conde ((== xs '()))
         ((fresh (a d)
            (== `(,a . ,d) xs)
            (=/= x a)
            (=/=-allo x d)))))

(define (all-diffo xs)
  (conde ((== xs '()))
         ((fresh (a d)
            (== `(,a . ,d) xs)
            (=/=-allo a d)
            (all-diffo d)))))

(define (all-diff*o xss)
  (conde ((== xss '()))
         ((fresh (a d)
            (== `(,a . ,d) xss)
            (all-diffo a)
            (all-diff*o d)))))

(define (membero x xs)
  (fresh (a d)
    (== `(,a . ,d) xs)
    (conde ((== x a))
           ((membero x d)))))

(define (nineo x) (membero x (cdr (iota 10))))
(define (nine*o xs)
  (conde ((== xs '()))
         ((fresh (a d)
            (== `(,a . ,d) xs)
            (nineo a)
            (nine*o d)))))

(define (run-sudoku board.in)
  (define _ (board-parts board.in))  ;; just to validate the input board size
  (define result
    (run 1 (x0  x1  x2  x3  x4  x5  x6  x7  x8
            x9  x10 x11 x12 x13 x14 x15 x16 x17
            x18 x19 x20 x21 x22 x23 x24 x25 x26
            x27 x28 x29 x30 x31 x32 x33 x34 x35
            x36 x37 x38 x39 x40 x41 x42 x43 x44
            x45 x46 x47 x48 x49 x50 x51 x52 x53
            x54 x55 x56 x57 x58 x59 x60 x61 x62
            x63 x64 x65 x66 x67 x68 x69 x70 x71
            x72 x73 x74 x75 x76 x77 x78 x79 x80)
      (let* ((board.out (list x0  x1  x2  x3  x4  x5  x6  x7  x8
                              x9  x10 x11 x12 x13 x14 x15 x16 x17
                              x18 x19 x20 x21 x22 x23 x24 x25 x26
                              x27 x28 x29 x30 x31 x32 x33 x34 x35
                              x36 x37 x38 x39 x40 x41 x42 x43 x44
                              x45 x46 x47 x48 x49 x50 x51 x52 x53
                              x54 x55 x56 x57 x58 x59 x60 x61 x62
                              x63 x64 x65 x66 x67 x68 x69 x70 x71
                              x72 x73 x74 x75 x76 x77 x78 x79 x80))
             (b.out      (board-parts board.out))
             (rows.out   (car   b.out))
             (cols.out   (cadr  b.out))
             (blocks.out (caddr b.out)))
        (fresh ()
          (== board.out (board-substitute board.in board.out))
          (all-diff*o rows.out)
          (all-diff*o cols.out)
          (all-diff*o blocks.out)
          (nine*o board.out)))))
  (car (board-parts (caar result))))

;; 0.2s
(define easy.1 '(0 0 0 0 3 2 0 5 7
                 0 0 5 1 0 0 0 0 0
                 2 8 1 7 4 5 0 9 6
                 0 0 0 0 7 0 0 0 0
                 0 0 8 0 0 9 7 6 0
                 0 4 0 5 0 1 0 0 8
                 5 0 3 9 8 4 0 7 0
                 6 0 4 0 5 7 0 3 1
                 0 0 2 0 1 0 0 0 9))

;; 0.011s
(define easy.2 '(0 3 0 0 4 0 0 0 7
                 0 4 0 0 2 1 3 9 0
                 1 9 0 0 0 0 0 8 0
                 3 7 8 2 0 0 0 1 0
                 4 0 0 5 1 8 7 0 0
                 0 0 1 4 0 0 0 2 0
                 0 1 3 7 0 0 0 4 0
                 2 5 7 9 0 0 8 3 0
                 9 0 0 0 0 3 0 7 2))

;; 0.038s
(define medium.1 '(0 0 0 0 0 7 9 1 0
                   0 0 7 0 0 4 6 0 0
                   5 0 8 9 0 3 4 0 0
                   9 2 0 3 0 5 0 0 0
                   8 0 1 7 2 0 0 0 0
                   4 0 0 0 0 1 2 5 0
                   6 0 0 0 0 0 0 0 0
                   0 0 0 1 0 0 0 0 8
                   2 1 9 0 3 0 0 0 0))

;; 0.14s
(define medium.2 '(7 0 4 0 0 0 6 0 0
                   6 0 0 0 9 8 0 0 0
                   0 0 0 0 0 0 2 0 9
                   1 0 0 4 2 0 5 0 8
                   0 7 0 0 0 0 0 4 2
                   0 0 2 6 0 0 1 0 0
                   4 0 0 8 0 1 0 2 0
                   2 6 0 3 4 9 0 5 0
                   0 0 0 0 0 0 0 7 0))

;; 5.17s
(define grid.50 '(3 0 0 2 0 0 0 0 0
                  0 0 0 1 0 7 0 0 0
                  7 0 6 0 3 0 5 0 0
                  0 7 0 0 0 9 0 8 0
                  9 0 0 0 2 0 0 0 4
                  0 1 0 8 0 0 0 5 0
                  0 0 9 0 4 0 3 0 1
                  0 0 0 7 0 2 0 0 0
                  0 0 0 0 0 8 0 0 6))

;; 8.63s
(define hard.1 '(0 0 8 6 2 7 0 0 9
                 0 0 0 5 0 0 0 0 0
                 0 3 0 0 9 0 0 0 0
                 0 0 6 9 0 0 3 0 2
                 0 0 0 0 0 0 9 5 0
                 1 0 0 8 0 0 0 0 0
                 0 0 0 0 5 2 0 6 3
                 4 0 0 0 8 0 0 0 0
                 0 0 0 3 0 0 2 4 0))

;; 116s
(define hard.2 '(0 0 5 0 7 0 0 0 0
                 0 0 6 0 0 9 0 0 0
                 0 9 0 0 5 0 0 0 0
                 0 7 9 0 0 2 5 0 0
                 0 0 0 0 6 0 9 1 0
                 0 0 0 5 0 8 4 0 0
                 0 0 0 0 0 0 0 0 3
                 0 5 0 6 0 0 0 0 1
                 9 1 0 4 0 7 0 0 5))

;; 146s
(define very-hard.1 '(0 5 0 0 0 8 2 6 9
                      0 0 2 0 4 3 0 0 0
                      0 0 9 0 0 0 0 0 0
                      0 0 7 0 0 0 0 0 0
                      0 0 0 0 9 0 0 4 0
                      5 0 3 0 0 0 0 9 0
                      0 0 0 0 2 4 6 0 5
                      6 0 0 0 0 0 0 0 3
                      0 4 0 0 8 0 0 0 0))

;; 61s
(define very-hard.2 '(0 4 0 0 0 0 0 0 0
                      0 0 6 0 1 0 4 0 0
                      0 0 1 0 3 7 0 0 0
                      0 0 0 0 0 8 0 1 0
                      0 6 0 7 0 0 0 5 0
                      0 0 8 0 9 0 0 7 0
                      0 5 0 1 0 0 8 6 0
                      0 0 0 3 0 0 0 9 0
                      9 0 0 0 0 0 2 0 0))

;; 15s
(define extremely-hard.1 '(0 1 0 0 0 2 5 4 0
                           0 9 8 0 0 0 0 0 0
                           0 0 0 0 0 0 0 0 0
                           6 0 7 0 0 0 3 0 0
                           5 0 0 2 0 0 0 0 0
                           0 0 0 9 0 0 0 0 0
                           0 0 0 0 6 0 0 9 8
                           0 0 0 0 3 0 0 0 0
                           7 0 0 0 0 0 0 0 0))

;; 243s
(define extremely-hard.2 '(0 0 0 1 0 2 0 3 0
                           8 0 0 7 0 0 0 0 0
                           4 0 0 0 0 0 9 0 0
                           0 1 0 3 0 0 0 0 0
                           0 0 0 0 6 0 4 0 0
                           0 0 0 0 0 0 0 0 0
                           6 0 9 0 4 0 0 0 0
                           0 0 0 0 0 0 0 7 1
                           0 0 0 0 5 0 0 0 0))

(define board.empty '(0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0))

(define (pretty-sudoku board)
  (for-each
    (lambda (row) (display row) (newline))
    (time (run-sudoku board))))

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
;    0.204662209s elapsed cpu time, including 0.011180000s collecting
;    0.204657000s elapsed real time, including 0.011213000s collecting
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
;    0.010838207s elapsed cpu time, including 0.000170000s collecting
;    0.010838000s elapsed real time, including 0.000170000s collecting
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
;    0.038041957s elapsed cpu time, including 0.001214000s collecting
;    0.038042000s elapsed real time, including 0.001220000s collecting
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
;    0.143453874s elapsed cpu time, including 0.005933000s collecting
;    0.143452000s elapsed real time, including 0.005951000s collecting
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
;    5.174854582s elapsed cpu time, including 0.623217000s collecting
;    5.174849000s elapsed real time, including 0.624684000s collecting
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
;    8.631541667s elapsed cpu time, including 1.195395000s collecting
;    8.631494000s elapsed real time, including 1.198126000s collecting
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
;    115.592896751s elapsed cpu time, including 24.144366000s collecting
;    115.593078000s elapsed real time, including 24.169685000s collecting
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
;    145.986150459s elapsed cpu time, including 35.329635000s collecting
;    145.986971000s elapsed real time, including 35.357951000s collecting
;    72934282272 bytes allocated, including 72888455208 bytes reclaimed
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
;    61.192674458s elapsed cpu time, including 12.215978000s collecting
;    61.192474000s elapsed real time, including 12.231357000s collecting
;    42655118128 bytes allocated, including 42703884128 bytes reclaimed
;(3 4 5 9 6 2 7 8 1)
;(7 2 6 8 1 5 4 3 9)
;(8 9 1 4 3 7 5 2 6)
;(4 7 9 6 5 8 3 1 2)
;(1 6 2 7 4 3 9 5 8)
;(5 3 8 2 9 1 6 7 4)
;(2 5 4 1 7 9 8 6 3)
;(6 8 7 3 2 4 1 9 5)
;(9 1 3 5 8 6 2 4 7)
;(time (run-sudoku board))
;    1410 collections
;    15.439503418s elapsed cpu time, including 2.730549000s collecting
;    15.449670000s elapsed real time, including 2.736899000s collecting
;    11821100272 bytes allocated, including 11793648976 bytes reclaimed
;(3 1 6 8 9 2 5 4 7)
;(4 9 8 3 5 7 1 6 2)
;(2 7 5 6 4 1 9 8 3)
;(6 2 7 4 8 5 3 1 9)
;(5 4 9 2 1 3 8 7 6)
;(8 3 1 9 7 6 4 2 5)
;(1 5 3 7 6 4 2 9 8)
;(9 6 2 1 3 8 7 5 4)
;(7 8 4 5 2 9 6 3 1)
;(time (run-sudoku board))
;    11918 collections
;    242.825417167s elapsed cpu time, including 58.313763000s collecting
;    242.861205000s elapsed real time, including 58.368714000s collecting
;    99932018096 bytes allocated, including 99832518648 bytes reclaimed
;(7 9 6 1 8 2 5 3 4)
;(8 5 3 7 9 4 1 2 6)
;(4 2 1 5 3 6 9 8 7)
;(9 1 4 3 7 5 2 6 8)
;(3 8 7 2 6 9 4 1 5)
;(2 6 5 4 1 8 7 9 3)
;(6 7 9 8 4 1 3 5 2)
;(5 4 8 9 2 3 6 7 1)
;(1 3 2 6 5 7 8 4 9)
