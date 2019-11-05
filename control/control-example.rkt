#lang racket
(require racket/control)

(define (walk yield! tree)
  (match tree
    ((cons a d) (walk yield! a) (walk yield! d))
    (x          (yield! x))))

(define (extract tree)
  ;; Alternative
  #;(define (yield! x)
    (match x
      ('STOP (abort   (cons #f '())))
      ('()   ;(shift k (cons k  '()))  ;; Another alternative
             (void))
      (x     (shift k (cons k  (list x))))))

  (define (yield! x)
    (cond ((equal? x 'STOP) (abort   (cons #f '())))
          ((symbol? x)      (shift k (cons k  (list x))))))

  (match-let loop (((cons k vs) (reset (walk yield! tree)
                                       (cons #f '()))))
    (append vs (if k (loop (k #t)) '()))))



(extract '(a (b c (d (e) f (g STOP h)) i) j))

(extract '(a (b c (d (e) f (g h)) i) j))


;; Kenichi Asai's examples

;; How to discard: times
(define (multiply xs)
  (define (loop xs)
    (match xs
      ('()        1)
      ((cons 0 _) (abort 0))  ;; Alternatively: (shift _ 0)
      ((cons a d) (* a (loop d)))))
  (reset (loop xs)))

(multiply '(1 2 3 4 5))
(multiply '(1 2 0 3 4 5))


;; How to extract: make-append
(define (make-append xs)
  (define (loop xs)
    (match xs
      ('()        (shift k k))
      ((cons a d) (cons a (loop d)))))
  (reset (loop xs)))

((make-append '(1 2 3)) '(4 5 6))


;; How to reorder: bring-nth-to-front, A-normalize
(define (bring-nth-to-front n xs)
  (define (loop n xs)
    (match xs
      ('()        '())
      ((cons a d) (if (= n 0)
                    (shift k (cons a (k d)))
                    (cons a (loop (- n 1) d))))))
  (reset (loop n xs)))

(bring-nth-to-front 20 '(0 1 2 3))
(bring-nth-to-front 4 '(0 1 2 3 4 5 6 7))


(define (A-normalize e)
  (define (loop e)
    (match e
      (`(+ ,a ,b) (let ((nfa (loop a))
                        (nfb (loop b))
                        (x   (gensym 'e)))
                    (shift k `(let ((,x (+ ,nfa ,nfb)))
                                ,(k x)))))
      (_          e)))
  (reset (loop e)))

(A-normalize '(+ 1 (+ (+ (+ 2 3) (+ 4 5)) 6)))


;; How to wrap: printf, state
(define (printf fmt)
  (define (loop fmt)
    (match fmt
      ('()        "")
      ((cons a d) (string-append
                    (match a
                      ('number (shift k (lambda (n) (k (number->string n)))))
                      (a       a))
                    (loop d)))))
  (reset (loop fmt)))

(((printf '("hello " number " world " number "!")) 8) 9)


(define (get)      (shift k (lambda (v) ((k v)      v))))
(define (put v)    (shift k (lambda (_) ((k (void)) v))))
(define (return v) (lambda (_) v))

((reset (put 3)
        (return (+ (get) (begin (put 4) (get)) (get))))
 0)
