#lang racket
(require "util.rkt")
(require "base.rkt")

(let ((_
  (map (lambda (el) (print (denote-eval el)) (display "\n")) (list
    (value (uno))                                           ; '()
    (value (pair (uno) (bit (b-0))))                        ; '(() . 0)
    (value (pair (bit (b-1)) (bit (b-0))))                  ; '(1 . 0)
    (value (pair (bit (b-1)) (pair (bit (b-0)) (uno))))     ; '(1 0)
    (action-2 (lam-apply)
      (action-2 (lam-apply)
        (value (lam (value (lam (value (bvar 1))))))
        (value (bit (b-0)))) (value (bit (b-1))))           ; 0
    (action-2 (lam-apply)
      (action-2 (lam-apply)
        (value (lam (value (lam (value (bvar 1))))))
        (value (bit (b-1)))) (value (bit (b-1))))           ; 1
    (action-2 (lam-apply)
      (action-2 (lam-apply)
        (value (lam (value (lam (value (bvar 1))))))
        (value (bit (b-1)))) (value (bit (b-0))))           ; 1
    (action-2 (pair-access)
      (value (bit (b-0))) (value (pair (uno) (bit (b-0))))) ; '()
    (action-2 (pair-access)
      (value (bit (b-1))) (value (pair (uno) (bit (b-0))))) ; 0
    ))))
  (void))

;> (define dt (denote (value (pair (bvar 0) (pair (bit (b-0)) (uno))))))
;> (dt denote-env-empty)
;list-ref: contract violation
  ;expected: pair?
  ;given: '()
  ;argument position: 1st
  ;other arguments...:
   ;0
  ;context...:
   ;/home/greg/projects/experiments/supercompilation/cog/base.rkt:63:0: denote-env-lookup
   ;/home/greg/projects/experiments/supercompilation/cog/base.rkt:52:18
   ;/usr/racket/collects/racket/private/misc.rkt:87:7
;> (dt (denote-env-extend denote-env-empty 'a))
;'(a 0)
;> (dt (denote-env-extend denote-env-empty (bvar 0)))
;(list (bvar 0) 0)
;> (dt (denote-env-extend denote-env-empty (list 'bvar 0)))
;'((bvar 0) 0)
;> (dt (denote-env-extend denote-env-empty 'v0))
;'(v0 0)


(define test-term-0
  (action-2 (lam-apply)
            (value (lam (value (pair (bvar 0) (bvar 1)))))
            (value (bvar 0))))

(pretty-print
  (substitute 0 (bit (b-1)) test-term-0))
;(action-2
; (lam-apply)
; (value (lam (value (pair (bvar 0) (bit (b-1))))))
; (value (bit (b-1))))

(define test-term-1
  (action-2 (lam-apply) (value (lam test-term-0)) (value (bit (b-1)))))

(pretty-print (step test-term-1))
;(right
; (action-2
;  (lam-apply)
;  (value (lam (value (pair (bvar 0) (bit (b-1))))))
;  (value (bit (b-1)))))

(pretty-print (step (right-x (step test-term-1))))
;(right (value (pair (bit (b-1)) (bit (b-1)))))

(define test-term-3
  (action-2
    (lam-apply)
    (value (lam (value (pair (pair (bvar 1)
                                   (bvar 0))
                             (lam (value (pair (pair (bvar 0)
                                                     (bvar 1))
                                               (bvar 2))))))))
    (value (pair (bvar 0)
                 (lam (value (pair (bvar 1)
                                   (bvar 0))))))))

(pretty-print (step test-term-3))
;(right
; (value
;  (pair
;   (pair (bvar 0) (pair (bvar 0) (lam (value (pair (bvar 1) (bvar 0))))))
;   (lam
;    (value
;     (pair
;      (pair (bvar 0) (pair (bvar 1) (lam (value (pair (bvar 2) (bvar 0))))))
;      (bvar 1)))))))


(pretty-print (tuple-encode (list 'a 'b 'c)))
;(pair 'a (pair 'b (pair 'c (uno))))
(pretty-print (tuple-pad 7 0 (tuple-encode (list 'a 'b 'c))))
;(pair 0 (pair 0 (pair 0 (pair 0 (pair 'a (pair 'b (pair 'c (uno))))))))

(pretty-print (tuple-get 3 (tuple-pad 7 0 (tuple-encode (list 'a 'b 'c)))))
;0
(pretty-print (tuple-get 5 (tuple-pad 7 0 (tuple-encode (list 'a 'b 'c)))))
;'b
(pretty-print (tuple-set 5 'd (tuple-pad 7 0 (tuple-encode (list 'a 'b 'c)))))
;(pair 0 (pair 0 (pair 0 (pair 0 (pair 'a (pair 'd (pair 'c (uno))))))))
(pretty-print (tuple-set 2 'd (tuple-pad 7 0 (tuple-encode (list 'a 'b 'c)))))
;(pair 0 (pair 0 (pair 'd (pair 0 (pair 'a (pair 'b (pair 'c (uno))))))))
(pretty-print (tuple-get 4 (tuple-pad 7 0 (tuple-encode (list (tuple-encode (list 'l 'r)) 'b 'c)))))
;(pair 'l (pair 'r (uno)))

(define l0 (tuple-lens-compose (tuple-lens 4) (tuple-lens 1)))
(pretty-print (car (l0 (tuple-pad 7 0 (tuple-encode (list (tuple-encode (list 'l 'r)) 'b 'c))))))
;'r
(pretty-print ((cdr (l0 (tuple-pad 7 0 (tuple-encode (list (tuple-encode (list 'l 'r)) 'b 'c))))) 'rrr))
;(pair
; 0
; (pair
;  0
;  (pair
;   0
;   (pair 0 (pair (pair 'l (pair 'rrr (uno))) (pair 'b (pair 'c (uno))))))))


(interact-with test-term-3)
