#lang pie

;;; Choose a function type based on a Nat
(claim choice-result (-> Nat U))
(define choice-result
  (lambda (k) (iter-Nat k (-> Nat Atom) (lambda (_) (-> Atom Nat)))))

(claim choice (Pi ((k Nat)) (choice-result k)))
(define choice
  (lambda (k)
    (ind-Nat
      k
      choice-result
      (lambda (k) 'base-case)
      (lambda (k)
        (lambda (_)
          (lambda (atom) 111))))))

(choice 0 17)          ; ==> (the Atom 'base-case)
(choice 1 'something)  ; ==> (the Nat 111)


(claim bitflip (-> Nat Nat))
(define bitflip
  (lambda (bit) (which-Nat bit 1 (lambda (z) z))))

;; return 0 if even, 1 if odd
(claim even? (-> Nat Nat))
(define even?
  (lambda (k) (iter-Nat k 0 bitflip)))

(even? 7)  ; ==> (the Nat 1)
(even? 8)  ; ==> (the Nat 0)


;;; Choose a function type based on whether a Nat is even or odd
(claim parity-choice (Pi ((k Nat)) (choice-result (even? k)) ))
(define parity-choice (lambda (k) (choice (even? k))))

(parity-choice 0 17)          ; ==> (the Atom 'base-case)
(parity-choice 1 'something)  ; ==> (the Nat 111)
(parity-choice 8 17)          ; ==> (the Atom 'base-case)
(parity-choice 9 'something)  ; ==> (the Nat 111)
