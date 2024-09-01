(load "../mk/mk.scm")
;;; Each letter is a distinct base-10 digit, and M is not 0:
;;;   S E N D
;;; + M O R E
;;; ---------
;;; M O N E Y

(define (all-=/= x*)
  (conde
    ((== x* '()))
    ((fresh (y y*)
       (== x* (cons y y*))
       (other-=/= y y*)
       (all-=/= y*)))))

(define (other-=/= x x*)
  (conde
    ((== x* '()))
    ((fresh (y y*)
       (== x* (cons y y*))
       (=/= x y)
       (other-=/= x y*)))))

(define (membero x x*)
  (conde
    ((fresh (y*)
       (== x* (cons x y*))))
    ((fresh (y y*)
       (== x* (cons y y*))
       (=/= x y)
       (membero x y*)))))

;; Base 10 little endian addition where the most significant digit is not allowed to be 0
(define (nonzeroo x) (fresh (a d) (== x (cons a d)) (=/= x '(0))))

(define (d+o da db dc carry)
  (membero (list da db dc carry)
           ;; Generated with:
           ;(apply append
           ;       (map (lambda (i)
           ;              (map (lambda (j)
           ;                     (list i j (remainder (+ i j) 10) (quotient (+ i j) 10)))
           ;                   (iota 10)))
           ;            (iota 10)))
           '((0 0 0 0)
             (0 1 1 0) (0 2 2 0) (0 3 3 0) (0 4 4 0) (0 5 5 0)
             (0 6 6 0) (0 7 7 0) (0 8 8 0) (0 9 9 0) (1 0 1 0) (1 1 2 0)
             (1 2 3 0) (1 3 4 0) (1 4 5 0) (1 5 6 0) (1 6 7 0) (1 7 8 0)
             (1 8 9 0) (1 9 0 1) (2 0 2 0) (2 1 3 0) (2 2 4 0) (2 3 5 0)
             (2 4 6 0) (2 5 7 0) (2 6 8 0) (2 7 9 0) (2 8 0 1) (2 9 1 1)
             (3 0 3 0) (3 1 4 0) (3 2 5 0) (3 3 6 0) (3 4 7 0) (3 5 8 0)
             (3 6 9 0) (3 7 0 1) (3 8 1 1) (3 9 2 1) (4 0 4 0) (4 1 5 0)
             (4 2 6 0) (4 3 7 0) (4 4 8 0) (4 5 9 0) (4 6 0 1) (4 7 1 1)
             (4 8 2 1) (4 9 3 1) (5 0 5 0) (5 1 6 0) (5 2 7 0) (5 3 8 0)
             (5 4 9 0) (5 5 0 1) (5 6 1 1) (5 7 2 1) (5 8 3 1) (5 9 4 1)
             (6 0 6 0) (6 1 7 0) (6 2 8 0) (6 3 9 0) (6 4 0 1) (6 5 1 1)
             (6 6 2 1) (6 7 3 1) (6 8 4 1) (6 9 5 1) (7 0 7 0) (7 1 8 0)
             (7 2 9 0) (7 3 0 1) (7 4 1 1) (7 5 2 1) (7 6 3 1) (7 7 4 1)
             (7 8 5 1) (7 9 6 1) (8 0 8 0) (8 1 9 0) (8 2 0 1) (8 3 1 1)
             (8 4 2 1) (8 5 3 1) (8 6 4 1) (8 7 5 1) (8 8 6 1) (8 9 7 1)
             (9 0 9 0) (9 1 0 1) (9 2 1 1) (9 3 2 1) (9 4 3 1) (9 5 4 1)
             (9 6 5 1) (9 7 6 1) (9 8 7 1) (9 9 8 1))))

(define (base10le+/carryo a b carry c)
  (conde
    ((== a '())
     (conde
       ((== b '())
        (conde
          ((==  carry 0) (== c '()))
          ((=/= carry 0) (== c (list carry)))))
       ((nonzeroo b)
        (conde
          ((==  carry 0) (== b c))
          ((=/= carry 0) (base10le+o (list carry) b c))))))
    ((nonzeroo a)
     (conde
       ((== b '())
        (conde
          ((==  carry 0) (== a c))
          ((=/= carry 0) (base10le+o (list carry) a c))))
       ((fresh (da da* db db* dc0 dc dc* carry0 carry1 carry2)
          (== a (cons da da*))
          (== b (cons db db*))
          (== c (cons dc dc*))
          (=/= a '(0))
          (=/= b '(0))
          (=/= c '(0))
          (d+o da db dc0 carry0)
          (d+o dc0 carry dc 0)
          (base10le+/carryo da* db* carry0 dc*)))))))

(define (base10le+o a b c)
  (base10le+/carryo a b 0 c))

(define (print-answer a)
  (let-values (((S E N D M O R Y) (apply values (caar a))))
    (for-each
      (lambda (line) (write line) (newline))
      (list (list '_ S E N D)
            (list '+ M O R E)
            (list '_________)
            (list M O N E Y)))))

;(print-answer
;  (time (run 1 (S E N D M O R Y)
;          (=/= M 0)
;          (all-=/= (list S E N D M O R Y))
;          (base10le+o (list D N E S) (list E R O M) (list Y E N O M)))))
;==>
;(time (run 1 ...))
;    155 collections
;    2.252055916s elapsed cpu time, including 0.014199000s collecting
;    2.252046000s elapsed real time, including 0.014314000s collecting
;    1298218992 bytes allocated, including 1299094064 bytes reclaimed
;(_ 9 5 6 7)
;(+ 1 0 8 5)
;(_________)
;(1 0 6 5 2)

;(length
;  (time (run 10 (S E N D M O R Y)
;          (=/= M 0)
;          (all-=/= (list S E N D M O R Y))
;          (base10le+o (list D N E S) (list E R O M) (list Y E N O M)))))
;==>
;(time (run 10 ...))
;    208 collections
;    3.070436917s elapsed cpu time, including 0.018820000s collecting
;    3.070419000s elapsed real time, including 0.018979000s collecting
;    1747756272 bytes allocated, including 1743194656 bytes reclaimed
;1
