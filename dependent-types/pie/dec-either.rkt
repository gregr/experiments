#lang pie

(claim Dec (-> U U))
(define Dec
  (lambda (A)
    (Either A (-> A Absurd))))

(claim Dec=
  (Pi ((a U))
    (-> a a U)))
(define Dec=
  (lambda (a x y) (Dec (= a x y))))

(claim EqDec (-> U U))
(define EqDec
  (lambda (a)
    (Pi ((x a) (y a))
      (Dec= a x y))))

(claim choose-U
       (Pi ((A U) (B U))
           (-> U U (Either A B) U)))
(define choose-U
  (lambda (A B U.A U.B e)
    (which-Nat
      (ind-Either
        e
        (lambda (_) Nat)
        (lambda (_) 0)
        (lambda (_) 1))
      U.A
      (lambda (_) U.B))))

(claim unpack-left
       (Pi ((A U) (B U))
           (-> A (Either A B) A)))
(define unpack-left
  (lambda (A B default e)
    (ind-Either
      e
      (lambda (_) A)
      (lambda (l) l)
      (lambda (r) default))))

(claim unpack-right
       (Pi ((A U) (B U))
           (-> B (Either A B) B)))
(define unpack-right
  (lambda (A B default e)
    (ind-Either
      e
      (lambda (_) B)
      (lambda (l) default)
      (lambda (r) r))))

(claim EqDec-Either
  (Pi ((a U) (b U) (eq-dec-a (EqDec a)) (eq-dec-b (EqDec b)))
    (EqDec (Either a b))))
(define EqDec-Either
  (lambda (a b eq-dec-a eq-dec-b)
    (lambda (x y)
      (ind-Either
        x
        (lambda (x) (Either (= (Either a b) x y)
                            (-> (= (Either a b) x y)
                                Absurd)))
        (lambda (l.x)
          (ind-Either
            y
            (lambda (y) (Either (= (Either a b) (left l.x) y)
                                (-> (= (Either a b) (left l.x) y)
                                    Absurd)))
            (lambda (l.y)
              (ind-Either
                (eq-dec-a l.x l.y)
                (lambda (_) (Either (= (Either a b) (left l.x) (left l.y))
                                    (-> (= (Either a b) (left l.x) (left l.y))
                                        Absurd)))
                (lambda (l.x=l.y)
                  (left (replace
                          l.x=l.y
                          (lambda (y) (= (Either a b) (left l.x) (left y)))
                          (same (left l.x)))))
                (lambda (l.x=/=l.y)
                  (right (lambda (l.x=l.y)
                           (l.x=/=l.y
                             (replace
                               l.x=l.y
                               (lambda (X) (= a
                                              (unpack-left a b l.x (left l.x))
                                              (unpack-left a b l.x X)))
                               (same (unpack-left a b l.x (left l.x))))))))))
            (lambda (r.y)
              (right (lambda (left.l.x=right.r.y)
                       (replace
                         left.l.x=right.r.y
                         (lambda (x) (choose-U a b Trivial Absurd x))
                         sole))))))
        (lambda (r.x)
          (ind-Either
            y
            (lambda (y) (Either (= (Either a b) (right r.x) y)
                                (-> (= (Either a b) (right r.x) y)
                                    Absurd)))
            (lambda (l.y)
              (right (lambda (right.r.x=left.l.y)
                       (replace
                         right.r.x=left.l.y
                         (lambda (x) (choose-U a b Absurd Trivial x))
                         sole))))
            (lambda (r.y)
              (ind-Either
                (eq-dec-b r.x r.y)
                (lambda (_) (Either (= (Either a b) (right r.x) (right r.y))
                                    (-> (= (Either a b) (right r.x) (right r.y))
                                        Absurd)))
                (lambda (r.x=r.y)
                  (left (replace
                          r.x=r.y
                          (lambda (y) (= (Either a b) (right r.x) (right y)))
                          (same (right r.x)))))
                (lambda (r.x=/=r.y)
                  (right (lambda (r.x=r.y)
                           (r.x=/=r.y
                             (replace
                               r.x=r.y
                               (lambda (X) (= b
                                              (unpack-right a b r.x (right r.x))
                                              (unpack-right a b r.x X)))
                               (same (unpack-right a b r.x (right r.x))))))))))))))))
