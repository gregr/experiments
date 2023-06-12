#lang pie

;(define ack
;  (lambda (m n)
;    case:
;    m=0         -> (add1 n)
;    m=k+1 n=0   -> (ack k 1)
;    m=k+1 n=j+1 -> (ack k (ack (add1 k) j))
;    ))

;; explicitly curried
;(define ack
;  (lambda (m)
;    (lambda (n)
;      case:
;      m=0         -> (add1 n)
;      m=k+1 n=0   -> ((ack k) 1)
;      m=k+1 n=j+1 -> ((ack k)
;                      ((ack (add1 k))
;                       j))
;      )))

;; push lambda inward
;(define ack
;  (lambda (m)
;    case:
;    m=0   -> (lambda (n) (add1 n))
;    m=k+1 -> (lambda (n)
;               case:
;               n=0   -> ((ack k) 1)
;               n=j+1 -> ((ack k)
;                         ((ack (add1 k))
;                          j)))))

;; explicitly name shared recursive call
;(define ack
;  (lambda (m)
;    case:
;    m=0   -> (lambda (n) (add1 n))
;    m=k+1 -> (lambda (n)
;               (let ((ack-m-minus-1 (ack k)))
;                 case:
;                 n=0   -> (ack-m-minus-1 1)
;                 n=j+1 -> (ack-m-minus-1
;                            ((ack (add1 k))  ; (ack m)
;                             j))))))

;; float let outward
;(define ack
;  (lambda (m)
;    case:
;    m=0   -> (lambda (n) (add1 n))
;    m=k+1 -> (let ((ack-m-minus-1 (ack k)))  ; primitive recursive because k is the predecessor of m
;               (lambda (n)
;                 case:
;                 n=0   -> (ack-m-minus-1 1)
;                 n=j+1 -> (ack-m-minus-1
;                            ((ack (add1 k))  ; (ack m)
;                             j))))))

;; recognize (ack m) loop, which is "primitive recursive" if we see ack-m-minus-1 as a simple operator
;(define ack
;  (lambda (m)
;    case:
;    m=0   -> (lambda (n) (add1 n))
;    m=k+1 -> (let ((ack-m-minus-1 (ack k)))  ; primitive recursive because k is the predecessor of m
;               (letrec ((loop (lambda (n)
;                                case:
;                                n=0   -> (ack-m-minus-1 1)
;                                n=j+1 -> (ack-m-minus-1
;                                           (loop j)))))  ; primitive recursive because j is the predecessor of n
;                 loop))))

;; Now all recursive call arguments are direct predecessors, so we can convert to Pie by replacing them with uses of iter-Nat:

(claim ack (-> Nat (-> Nat Nat)))

(define ack
  (lambda (m)
    (iter-Nat
      m
      (the (-> Nat Nat) (lambda (n) (add1 n)))
      (lambda (ack-m-minus-1)  ; ack-m-minus-1 corresponds to (ack k)
        (lambda (n)
          (iter-Nat
            n
            (ack-m-minus-1 1)
            (lambda (loop-n-minus-1)  ; loop-n-minus-1 corresponds to (loop j)
              (ack-m-minus-1 loop-n-minus-1))))))))

(the Nat (ack 3 2))  ; ==> (the Nat 29)
(the Nat (ack 3 3))  ; ==> (the Nat 61)
