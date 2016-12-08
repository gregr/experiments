#lang racket

(define-syntax implies
  (syntax-rules ()
    ((_ hypothesis conclusion)
     (if hypothesis conclusion #t))))

(define peano-number?
  (lambda (n)
    (match n
      ('() #t)
      (`(s . ,r) (peano-number? r))
      (_ #f))))

(define even?
  (lambda (n)
    (match n
      ('() #t)
      (`(s . ,r) (odd? r)))))

(define odd?
  (lambda (n)
    (match n
      ('() #f)
      (`(s . ,r) (even? r)))))

(define double
  (lambda (n)
    (match n
      ('() '())
      (`(s . ,r) `(s s . ,(double r))))))


;; Assume strict argument evaluation.

;; Forall (n : peano-number?), (== (even? (double n)) #t)
(define theorem-double-even
  (lambda (n)
    (implies (peano-number? n)
             (even? (double n)))))


;; Comment subsequent lines if you'd like to run this file.

;; Proof, with easy steps elided for readability

(define theorem-double-even
  (lambda (n)
    (implies (peano-number? n)
             (even? (double n)))))  ;; lift this

(define theorem-double-even
  (letrec ((h0 (lambda (n)
                 (even? (double n)))))  ;; <-- focus here and remember
    (lambda (n)
      (implies (peano-number? n)
               (h0 n)))))

;; remember (== (h0 n) (even? (double n)))

(define theorem-double-even
  (letrec ((h0 (lambda (n)
                 (even?  ;; move this context into cases
                   (match n
                     ('() '())
                     (`(s . ,r) `(s s . ,(double r))))))))
    (lambda (n)
      (implies (peano-number? n)
               (h0 n)))))

(define theorem-double-even
  (letrec ((h0 (lambda (n)
                 (match n
                   ('() (even? '()))  ;; simplify
                   (`(s . ,r)
                     (even? `(s s . ,(double r))))))))  ;; simplify
    (lambda (n)
      (implies (peano-number? n)
               (h0 n)))))

(define theorem-double-even
  (letrec ((h0 (lambda (n)
                 (match n
                   ('() #t)
                   (`(s . ,r)
                     (even? (double r)))))))  ;; <-- notice this is (h0 r)
    (lambda (n)
      (implies (peano-number? n)
               (h0 n)))))

(define theorem-double-even
  (letrec ((h0 (lambda (n)
                 (match n
                   ('() #t)
                   (`(s . ,r) (h0 r))))))  ;; partial: error or return #t
    (lambda (n)
      (implies (peano-number? n)  ;; <-- focus here
               (h0 n)))))

(define theorem-double-even
  (letrec ((h0 (lambda (n)
                 (match n
                   ('() #t)
                   (`(s . ,r) (h0 r))))))
    (lambda (n)
      (if (peano-number? n)  ;; lift this
        (h0 n)
        #t))))

(define theorem-double-even
  (letrec ((h0 (lambda (n)
                 (match n
                   ('() #t)
                   (`(s . ,r) (h0 r)))))
           (h1 (lambda (n)
                 (if (peano-number? n)  ;; <-- focus here and remember
                   (h0 n)
                   #t))))
    (lambda (n) (h1 n))))

;; remember (== (h1 n) (if (peano-number? n) (h0 n) #t))

(define theorem-double-even
  (letrec ((h0 (lambda (n)
                 (match n
                   ('() #t)
                   (`(s . ,r) (h0 r)))))
           (h1 (lambda (n)
                 (if (match n  ;; move 'if' context into cases
                       ('() #t)
                       (`(s . ,r) (peano-number? r))
                       (_ #f))
                   (h0 n)
                   #t))))
    (lambda (n) (h1 n))))

(define theorem-double-even
  (letrec ((h0 (lambda (n)
                 (match n
                   ('() #t)
                   (`(s . ,r) (h0 r)))))
           (h1 (lambda (n)
                 (match n
                   ('() (if #t (h0 n) #t))  ;; simplify
                   (`(s . ,r)
                     (if (peano-number? r)
                       (h0 n)
                       #t))
                   (_ (if #f (h0 n) #t))))))  ;; simplify
    (lambda (n) (h1 n))))

(define theorem-double-even
  (letrec ((h0 (lambda (n)
                 (match n
                   ('() #t)
                   (`(s . ,r) (h0 r)))))
           (h1 (lambda (n)
                 (match n
                   ('() (h0 n))  ;; substitute n
                   (`(s . ,r)
                     (if (peano-number? r)
                       (h0 n)  ;; substitute n
                       #t))
                   (_ #t)))))
    (lambda (n) (h1 n))))

(define theorem-double-even
  (letrec ((h0 (lambda (n)
                 (match n
                   ('() #t)
                   (`(s . ,r) (h0 r)))))
           (h1 (lambda (n)
                 (match n
                   ('() (h0 '()))  ;; simplify
                   (`(s . ,r)
                     (if (peano-number? r)
                       (h0 `(s . ,r))  ;; simplify
                       #t))
                   (_ #t)))))
    (lambda (n) (h1 n))))

(define theorem-double-even
  (letrec ((h0 (lambda (n)
                 (match n
                   ('() #t)
                   (`(s . ,r) (h0 r)))))
           (h1 (lambda (n)
                 (match n
                   ('() #t)
                   (`(s . ,r)
                     (if (peano-number? r)  ;; <-- notice this is (h1 r)
                       (h0 r)
                       #t))
                   (_ #t)))))
    (lambda (n) (h1 n))))

(define theorem-double-even
  (letrec ((h0 (lambda (n)
                 (match n
                   ('() #t)
                   (`(s . ,r) (h0 r)))))
           (h1 (lambda (n)
                 (match n
                   ('() #t)
                   (`(s . ,r) (h1 r))
                   (_ #t)))))  ;; <-- notice we're total
    (lambda (n) (h1 n))))

(define theorem-double-even
  (letrec ((h0 (lambda (n)
                 (match n
                   ('() #t)
                   (`(s . ,r) (h0 r)))))
           (h1 (lambda (n) #t)))
    (lambda (n) (h1 n))))  ;; simplify

(define theorem-double-even
  (letrec ((h0 (lambda (n)
                 (match n
                   ('() #t)
                   (`(s . ,r) (h0 r)))))
           (h1 (lambda (n) #t)))  ;; remove dead code
    (lambda (n) #t)))

;; Forall (n : peano-number?), (== (even? (double n)) #t)
(define theorem-double-even
  (lambda (n) #t))  ;; QED
