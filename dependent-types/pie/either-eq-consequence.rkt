#lang pie

(claim Dec (-> U U))
(define Dec
  (lambda (A)
    (Either A (-> A Absurd))))

(claim =Dec (-> U U))
(define =Dec
  (lambda (A)
    (Pi ((x A) (y A))
      (Dec (= A x y)))))

(claim Trivial=? (=Dec Trivial))
(define Trivial=?
  (lambda (t1 t2)
    (left (same sole))))

(claim + (-> Nat Nat Nat))
(define +
  (lambda (x y)
    (iter-Nat
      x
      y
      (lambda (y) (add1 y)))))

(claim Nat=consequence (-> Nat Nat U))
(define Nat=consequence
  (lambda (x y)
    (which-Nat
      x
      (which-Nat
        y
        Trivial
        (lambda (_) Absurd))
      (lambda (x-1)
        (which-Nat
          y
          Absurd
          (lambda (y-1) (= Nat x-1 y-1)))))))

(claim Nat=consequence-same (Pi ((n Nat)) (Nat=consequence n n)))
(define Nat=consequence-same
  (lambda (n)
    (ind-Nat
      n
      (lambda (n) (Nat=consequence n n))
      sole
      (lambda (n-1 _) (same n-1)))))

(claim use-Nat=
       (Pi ((x Nat) (y Nat))
           (-> (= Nat x y) (Nat=consequence x y))))
(define use-Nat=
  (lambda (x y x=y)
    (replace
      x=y
      (lambda (n) (Nat=consequence x n))
      (Nat=consequence-same x))))

(claim zero=/=add1
       (Pi ((n Nat))
           (-> (= Nat 0 (add1 n)) Absurd)))
(define zero=/=add1
  (lambda (n 0=n+1)
    (use-Nat= 0 (add1 n) 0=n+1)))

(claim add1=/=zero
       (Pi ((n Nat))
           (-> (= Nat (add1 n) 0) Absurd)))
(define add1=/=zero
  (lambda (n n+1=0)
    (use-Nat= (add1 n) 0 n+1=0)))

(claim Nat=? (=Dec Nat))
(define Nat=?
  (lambda (x y)
    ((ind-Nat
       x
       (lambda (x) (Pi ((y Nat)) (Dec (= Nat x y))))
       (lambda (y)
         (ind-Nat
           y
           (lambda (y) (Dec (= Nat 0 y)))
           (left (same 0))
           (lambda (y-1 proof) (right (zero=/=add1 y-1)))))
       (lambda (x-1 =?.x-1 y)
         (ind-Nat
           y
           (lambda (y) (Dec (= Nat (add1 x-1) y)))
           (right (add1=/=zero x-1))
           (lambda (y-1 _)
             (ind-Either
               (=?.x-1 y-1)
               (lambda (e) (Dec (= Nat (add1 x-1) (add1 y-1))))
               (lambda (x-1=y-1) (left (cong x-1=y-1 (+ 1))))
               (lambda (x-1=/=y-1)
                 (right (lambda (x-1+1=y-1+1)
                          (x-1=/=y-1
                            (use-Nat= (add1 x-1) (add1 y-1) x-1+1=y-1+1))))))))))
     y)))

(claim either-choose-U
       (Pi ((A U) (B U))
           (-> (Either A B) U U U)))
(define either-choose-U
  (lambda (A B e u1 u2)
    (which-Nat
      (ind-Either
        e
        (lambda (_) Nat)
        (lambda (_) 0)
        (lambda (_) 1))
      u1
      (lambda (_) u2))))

(claim decide-Nat
       (Pi ((A U)) (-> (=Dec A) A A Nat)))
(define decide-Nat
  (lambda (A decide a1 a2)
    (ind-Either
      (decide a1 a2)
      (lambda (_) Nat)
      (lambda (_) 0)
      (lambda (_) 1))))

(claim either-pair-code
       (Pi ((A U) (B U))
           (-> (=Dec A) (=Dec B)
               (Pi ((e1 (Either A B)))
                   (-> (Either A B)
                       (Pair Nat (Pair (either-choose-U A B e1 A B)
                                       (either-choose-U A B e1 A B))))))))
(define either-pair-code
  (lambda (A B dec.A dec.B e1 e2)
    (ind-Either
      e1
      (lambda (e1) (Pair Nat (Pair (either-choose-U A B e1 A B)
                                   (either-choose-U A B e1 A B))))
      (lambda (a1)
        (ind-Either
          e2
          (lambda (e2) (Pair Nat (Pair A A)))
          (lambda (a2) (cons (decide-Nat A dec.A a1 a2) (cons a1 a2)))
          (lambda (b2) (cons 1 (cons a1 a1)))))
      (lambda (b1)
        (ind-Either
          e2
          (lambda (e2) (Pair Nat (Pair B B)))
          (lambda (a2) (cons 1 (cons b1 b1)))
          (lambda (b2) (cons (decide-Nat B dec.B b1 b2) (cons b1 b2))))))))

(claim either=consequence
       (Pi ((A U) (B U))
           (-> (=Dec A) (=Dec B) (Either A B) (Either A B) U)))
(define either=consequence
  (lambda (A B dec.A dec.B e1 e2)
    (which-Nat
      (car (either-pair-code A B dec.A dec.B e1 e2))
      (= (either-choose-U A B e1 A B)
         (car (cdr (either-pair-code A B dec.A dec.B e1 e2)))
         (cdr (cdr (either-pair-code A B dec.A dec.B e1 e2))))
      (lambda (_) Absurd))))

(either=consequence Nat Nat Nat=? Nat=? (left 10) (left 10))
(either=consequence Nat Nat Nat=? Nat=? (left 10) (left 11))
(either=consequence Nat Nat Nat=? Nat=? (left 10) (right 10))
(either=consequence Nat Nat Nat=? Nat=? (right 10) (left 10))
(either=consequence Nat Nat Nat=? Nat=? (right 10) (right 10))
(either=consequence Nat Nat Nat=? Nat=? (right 10) (right 11))
;==>
;(the U
;  (= Nat 10 10))
;(the U Absurd)
;(the U Absurd)
;(the U Absurd)
;(the U
;  (= Nat 10 10))
;(the U Absurd)
(either=consequence Nat Trivial Nat=? Trivial=? (left 12) (left 12))
(either=consequence Nat Trivial Nat=? Trivial=? (left 12) (left 11))
(either=consequence Nat Trivial Nat=? Trivial=? (left 12) (right sole))
(either=consequence Nat Trivial Nat=? Trivial=? (right sole) (left 12))
(either=consequence Nat Trivial Nat=? Trivial=? (right sole) (right sole))
;==>
;(the U
;  (= Nat 12 12))
;(the U Absurd)
;(the U Absurd)
;(the U Absurd)
;(the U
;  (= Trivial sole sole))
