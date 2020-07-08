#lang racket

(define (map f xs)
  (if (null? xs)
    '()
    (let ((a (f (car xs)))
          (d (map f (cdr xs))))
      (cons a d))))

(define (map/k k f xs)
  (if (null? xs)
    (k '())
    (f (lambda (a)
         (map/k (lambda (d) (k (cons a d)))
                f (cdr xs)))
       (car xs))))

(define (+1 x) (+ x 1))
(define (*2 x) (* x 2))


;;;;;;;;;;;;;;;;;;
;; direct style ;;
;;;;;;;;;;;;;;;;;;
(define (H Xs) (map +1 (map *2 Xs)))

(let ((Ys (map *2 Xs)))
  (map +1 Ys))

(let ((Ys (if (null? Xs)
            '()
            (let ((A (*2 (car Xs)))
                  (D (map *2 (cdr Xs))))
              (cons A D)))))
  (map +1 Ys))

(if (null? Xs)
  (map +1 '())
  (let ((A (*2 (car Xs)))
        (D (map *2 (cdr Xs))))
    (map +1 (cons A D))))

(if (null? Xs)
  '()
  (let ((A (*2 (car Xs)))
        (D (map *2 (cdr Xs))))  ;; *
    (let ((AA (+1 A))
          (DD (map +1 D)))
      (cons AA DD))))

;; lower let binding to use site (safe to pass +1, which is pure)
(if (null? Xs)
  '()
  (let ((A (*2 (car Xs))))
    (let ((AA (+1 A))
          (DD (let ((D (map *2 (cdr Xs))))  ;; *
                (map +1 D))))
      (cons AA DD))))

(define (H Xs)
  (if (null? Xs)
    '()
    (let ((A (*2 (car Xs))))
      (let ((AA (+1 A))
            (DD (H (cdr Xs))))
        (cons AA DD)))))

(define (H Xs)
  (if (null? Xs)
    '()
    (cons (+1 (*2 (car Xs))) (H (cdr Xs)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cps (assume direct style for primitives +1 *2 for simplicity) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (H/k K Xs)
  (map/k (lambda (Ys) (map/k K +1 Ys))
         *2 Xs))

(map/k (lambda (Ys) (map/k K +1 Ys))
       *2 Xs)

(if (null? Xs)
  ((lambda (Ys) (map/k K +1 Ys)) '())
  (primcall *2 (lambda (A)
                 (map/k (lambda (D) ((lambda (Ys) (map/k K +1 Ys))
                                     (cons A D)))
                        *2 (cdr Xs)))
            (car Xs)))

(if (null? Xs)
  (map/k K +1 '())
  (let ((A (*2 (car Xs))))
    (map/k (lambda (D) ((lambda (Ys) (map/k K +1 Ys))
                        (cons A D)))
           *2 (cdr Xs))))

(if (null? Xs)
  (K '())
  (let ((A (*2 (car Xs))))
    (map/k (lambda (D) (map/k K +1 (cons A D)))
           *2 (cdr Xs))))

(if (null? Xs)
  (K '())
  (let ((A (*2 (car Xs))))
    (map/k (lambda (D)
             (primcall +1 (lambda (AA)
                            (map/k (lambda (DD) (K (cons AA DD)))
                                   +1 D))
                       A))
           *2 (cdr Xs))))

(if (null? Xs)
  (K '())
  (let ((A (*2 (car Xs))))
    (map/k (lambda (D)
             (let ((AA (+1 A)))  ;; *
               (map/k (lambda (DD) (K (cons AA DD)))
                      +1 D)))
           *2 (cdr Xs))))

;; float pure binding
(if (null? Xs)
  (K '())
  (let ((A (*2 (car Xs))))
    (let ((AA (+1 A)))  ;; *
      (map/k (lambda (D)
               (map/k (lambda (DD) (K (cons AA DD)))
                      +1 D))
             *2 (cdr Xs)))))

(if (null? Xs)
  (K '())
  (let ((A (*2 (car Xs))))
    (let ((AA (+1 A)))
      (H/k (lambda (DD) (K (cons AA DD)))
           (cdr Xs)))))

(define (H/k K Xs)
  (if (null? Xs)
    (K '())
    (let ((A (*2 (car Xs))))
      (let ((AA (+1 A)))
        (H/k (lambda (DD) (K (cons AA DD)))
             (cdr Xs))))))

(define (H/k K Xs)
  (if (null? Xs)
    (K '())
    (let ((AA (+1 (*2 (car Xs)))))
      (H/k (lambda (DD) (K (cons AA DD)))
           (cdr Xs)))))
