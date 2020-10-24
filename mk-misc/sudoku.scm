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

;; 0.24s
(define easy.1 '(0 0 0 0 3 2 0 5 7
                 0 0 5 1 0 0 0 0 0
                 2 8 1 7 4 5 0 9 6
                 0 0 0 0 7 0 0 0 0
                 0 0 8 0 0 9 7 6 0
                 0 4 0 5 0 1 0 0 8
                 5 0 3 9 8 4 0 7 0
                 6 0 4 0 5 7 0 3 1
                 0 0 2 0 1 0 0 0 9))

;; 0.01s
(define easy.2 '(0 3 0 0 4 0 0 0 7
                 0 4 0 0 2 1 3 9 0
                 1 9 0 0 0 0 0 8 0
                 3 7 8 2 0 0 0 1 0
                 4 0 0 5 1 8 7 0 0
                 0 0 1 4 0 0 0 2 0
                 0 1 3 7 0 0 0 4 0
                 2 5 7 9 0 0 8 3 0
                 9 0 0 0 0 3 0 7 2))

;; 0.04s
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

;; 4s
(define hard.1 '(0 0 8 6 2 7 0 0 9
                 0 0 0 5 0 0 0 0 0
                 0 3 0 0 9 0 0 0 0
                 0 0 6 9 0 0 3 0 2
                 0 0 0 0 0 0 9 5 0
                 1 0 0 8 0 0 0 0 0
                 0 0 0 0 5 2 0 6 3
                 4 0 0 0 8 0 0 0 0
                 0 0 0 3 0 0 2 4 0))

;; 108s
(define hard.2 '(0 0 5 0 7 0 0 0 0
                 0 0 6 0 0 9 0 0 0
                 0 9 0 0 5 0 0 0 0
                 0 7 9 0 0 2 5 0 0
                 0 0 0 0 6 0 9 1 0
                 0 0 0 5 0 8 4 0 0
                 0 0 0 0 0 0 0 0 3
                 0 5 0 6 0 0 0 0 1
                 9 1 0 4 0 7 0 0 5))

;; 136s
(define very-hard.1 '(0 5 0 0 0 8 2 6 9
                      0 0 2 0 4 3 0 0 0
                      0 0 9 0 0 0 0 0 0
                      0 0 7 0 0 0 0 0 0
                      0 0 0 0 9 0 0 4 0
                      5 0 3 0 0 0 0 9 0
                      0 0 0 0 2 4 6 0 5
                      6 0 0 0 0 0 0 0 3
                      0 4 0 0 8 0 0 0 0))

;; 36s
(define very-hard.2 '(0 4 0 0 0 0 0 0 0
                      0 0 6 0 1 0 4 0 0
                      0 0 1 0 3 7 0 0 0
                      0 0 0 0 0 8 0 1 0
                      0 6 0 7 0 0 0 5 0
                      0 0 8 0 9 0 0 7 0
                      0 5 0 1 0 0 8 6 0
                      0 0 0 3 0 0 0 9 0
                      9 0 0 0 0 0 2 0 0))

(define grid.50 '(3 0 0 2 0 0 0 0 0
                  0 0 0 1 0 7 0 0 0
                  7 0 6 0 3 0 5 0 0
                  0 7 0 0 0 9 0 8 0
                  9 0 0 0 2 0 0 0 4
                  0 1 0 8 0 0 0 5 0
                  0 0 9 0 4 0 3 0 1
                  0 0 0 7 0 2 0 0 0
                  0 0 0 0 0 8 0 0 6))

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
            ))
