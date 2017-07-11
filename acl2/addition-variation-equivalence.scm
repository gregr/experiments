(define (add1 x y)
  (if (null? x)
    y
    (cons 'S (add1 (cdr x) y))))

(define (add2 x y)
  (if (null? x)
    y
    (add2 (cdr x) (cons 'S y))))

;; lemma alternate
(theorem (equal? (cons 'S (add1 x y)) (add1 x (cons 'S y))))

(define (h0 x y)
  (cons 'S (add1 x y)))

(define (h0 x y)
  (cons 'S (if (null? x)
             y
             (cons 'S (add1 (cdr x) y)))))

(define (h0 x y)
  (if (null? x)
    (cons 'S y)
    (cons 'S (h0 (cdr x) y))))


(define (h1 x y)
  (add1 x (cons 'S y)))

(define (h1 x y)
  (if (null? x)
    (cons 'S y)
    (cons 'S (add1 (cdr x) (cons 'S y)))))

(define (h1 x y)
  (if (null? x)
    (cons 'S y)
    (cons 'S (h1 (cdr x) y))))


;; lemma
(theorem (equal? (add2 x (cons 'S y)) (cons 'S (add2 x y))))

(define (h0 x y)
  (add2 x (cons 'S y)))

(define (h0 x y)
  (if (null? x)
    (cons 'S y)
    (add2 (cdr x) (cons 'S (cons 'S y)))))

(define (h0 x y)
  (if (null? x)
    (cons 'S y)
    (h0 (cdr x) (cons 'S y))))


(define (h1 x y)
  (cons 'S (add2 x y)))

(define (h1 x y)
  (cons 'S
        (if (null? x)
          y
          (add2 (cdr x) (cons 'S y)))))

(define (h1 x y)
  (if (null? x)
    (cons 'S y)
    (cons 'S (add2 (cdr x) (cons 'S y)))))

(define (h1 x y)
  (if (null? x)
    (cons 'S y)
    (h1 (cdr x) (cons 'S y))))


;; prove equivalence
(theorem (equal? (add1 x y) (add2 x y)))

(define (add1 x y)
  (if (null? x)
    y
    (cons 'S (add1 (cdr x) y))))


(define (add2 x y)
  (if (null? x)
    y
    (add2 (cdr x) (cons 'S y))))

(define (add2 x y)
  (if (null? x)
    y
    (cons 'S (add2 (cdr x) y))))  ;; lemma, QED
