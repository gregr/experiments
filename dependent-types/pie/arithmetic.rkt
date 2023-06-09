#lang pie

(claim + (-> Nat Nat Nat))
(define +
  (lambda (a b)
    (iter-Nat a b (lambda (n) (add1 n)))))

(claim * (-> Nat Nat Nat))
(define *
  (lambda (a b)
    (iter-Nat a 0 (+ b))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; + is associative ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(claim proof.+-associative
       (Pi ((a Nat) (b Nat) (c Nat))
           (= Nat (+ (+ a b) c) (+ a (+ b c)))))
(define proof.+-associative
  (lambda (a b c)
    (ind-Nat
      a
      (lambda (a)
        (= Nat
           (+ (+ a b) c)
           (+ a (+ b c))))
      (same (+ b c))
      (lambda (m proof.m) (cong proof.m (+ 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; + is commutative ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(claim proof.+-commutative
       (Pi ((a Nat) (b Nat))
           (= Nat (+ b a) (+ a b))))
(define proof.+-commutative
  (lambda (a b)
    (ind-Nat
      a
      (lambda (a) (= Nat (+ b a) (+ a b)))
      (ind-Nat
        b
        (lambda (b) (= Nat (+ b 0) b))
        (same 0)
        (lambda (n proof.n) (cong proof.n (+ 1)) ))
      (lambda (m proof.m)
        (replace
          (ind-Nat
            b
            (lambda (b) (= Nat (+ 1 (+ b m)) (+ b (+ 1 m))))
            (same (+ 1 m))
            (lambda (n proof.n) (cong proof.n (+ 1))))
          (lambda (x) (= Nat x (+ 1 (+ m b))))
          (cong proof.m (+ 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * distributes over + ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(claim proof.*-distributive
       (Pi ((a Nat) (b Nat) (c Nat))
           (= Nat (* a (+ b c)) (+ (* a b) (* a c)))))
(define proof.*-distributive
  (lambda (a b c)
    (ind-Nat
      a
      (lambda (a)
        (= Nat
           (* a (+ b c))
           (+ (* a b) (* a c))))
      (same 0)
      (lambda (m proof.m)
        (replace
          (proof.+-associative (+ b (* m b)) c (* m c))
          (lambda (x)
            (= Nat
               (+ (+ b c)
                  (* m (+ b c)))
               x))
          (replace
            (symm (proof.+-associative b (* m b) c))
            (lambda (x)
              (= Nat
                 (+ (+ b c)
                    (* m (+ b c)))
                 (+ x (* m c))))
            (replace
              (proof.+-commutative (* m b) c)
              (lambda (x)
                (= Nat
                   (+ (+ b c)
                      (* m (+ b c)))
                   (+ (+ b x)
                      (* m c))))
              (replace
                (proof.+-associative b c (* m b))
                (lambda (x)
                  (= Nat
                     (+ (+ b c)
                        (* m (+ b c)))
                     (+ x (* m c))))
                (replace
                  (symm (proof.+-associative (+ b c) (* m b) (* m c)))
                  (lambda (x)
                    (= Nat
                       (+ (+ b c)
                          (* m (+ b c)))
                       x))
                  (replace
                    proof.m
                    (lambda (x)
                      (= Nat
                         (+ (+ b c)
                            (* m (+ b c)))
                         (+ (+ b c)
                            x)))
                    (same (+ (+ b c) (* m (+ b c))))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; * is commutative ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(claim proof.*-commutative
       (Pi ((a Nat) (b Nat))
           (= Nat (* b a) (* a b))))
(define proof.*-commutative
  (lambda (a b)
    (ind-Nat
      a
      (lambda (a) (= Nat (* b a) (* a b)))
      (ind-Nat
        b
        (lambda (b) (= Nat (* b 0) 0))
        (same 0)
        (lambda (n proof.n) proof.n))
      (lambda (m proof.m)
        (replace
          (symm (proof.*-distributive b 1 m))
          (lambda (x)
            (= Nat
               x
               (+ b (* m b))))
          (replace
            proof.m
            (lambda (x)
              (= Nat
                 (+ (* b 1) (* b m))
                 (+ b x)))
            (replace
              (ind-Nat
                b
                (lambda (b) (= Nat b (* b 1)))
                (same 0)
                (lambda (n proof.n) (cong proof.n (+ 1))))
              (lambda (x)
                (= Nat
                   (+ x (* b m))
                   (+ b (* b m))))
              (same (+ b (* b m))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; * is associative ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(claim proof.*-associative
       (Pi ((a Nat) (b Nat) (c Nat))
           (= Nat (* (* a b) c) (* a (* b c)))))
(define proof.*-associative
  (lambda (a b c)
    (ind-Nat
      a
      (lambda (a)
        (= Nat
           (* (* a b) c)
           (* a (* b c))))
      (same 0)
      (lambda (m proof.m)
        (replace
          (proof.*-commutative (+ b (* m b)) c)
          (lambda (x)
            (= Nat
               x
               (+ (* b c) (* m (* b c)))))
          (replace
            (symm (proof.*-distributive c b (* m b)))
            (lambda (x)
              (= Nat
                 x
                 (+ (* b c) (* m (* b c)))))
            (replace
              (proof.*-commutative b c)
              (lambda (x)
                (= Nat
                   (+ (* c b) (* c (* m b)))
                   (+ x (* m (* b c)))))
              (replace
                (proof.*-commutative c (* m b))
                (lambda (x)
                  (= Nat
                     (+ (* c b) x)
                     (+ (* c b) (* m (* b c)))))
                (replace
                  proof.m
                  (lambda (x)
                    (= Nat
                       (+ (* c b) (* (* m b) c))
                       (+ (* c b) x)))
                  (same (+ (* c b) (* (* m b) c))))))))))))
