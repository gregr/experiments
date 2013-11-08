#lang racket
(require "util.rkt")
(require "base.rkt")

(pretty-print (list-inits '(a b c d)))
;'(() (a) (a b) (a b c) (a b c d))


(pretty-print (:.* 'src))
(pretty-print (:=* 'src 'tgt))


(define tests `(
  ()                          ; '()
  (pair 0 (pair 1 ()))        ; '(0 1)
  (tuple 0 1 0 1)             ; '(0 1 0 1)
  (pair () 0)                 ; '(() . 0)
  (pair 1 0)                  ; '(1 . 0)
  (pair 1 (pair 0 ()))        ; '(1 0)
  ((lam (x y) x) 0 1)         ; 0
  ((lam (x y) x) 1 1)         ; 1
  ((lam (x y) y) 1 0)         ; 0
  (pair-access 0 (pair () 0)) ; '()
  (pair-access 1 (pair () 0)) ; 0
  (if-0 0 1 0)                ; 1
  (if-0 1 1 0)                ; 0
  (pair-l (pair 0 1))         ; 0
  (pair-r (pair 0 1))         ; 1
  ((pair-access 0 (pair (lam (_) ()) (lam (x) 0))) ())  ; this should become an if-0
  ((pair-access 0 (pair (lam (_) ()) (lam (x) x))) ())  ; this should not become an if-0
  ))

(define parsed-tests (right-x (map-parse penv-init tests)))
(define unparsed-tests (map (curry unparse upenv-empty) parsed-tests))
(pretty-print unparsed-tests)

(define (eval-print el)
  (print (denote-eval el)) (display "\n"))
(let ((_ (map eval-print parsed-tests))) (void))


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

(define l0 (:o* lens-identity (tuple-lens 4) (tuple-lens 1)))
(pretty-print (:.* (tuple-pad 7 0 (tuple-encode (list (tuple-encode (list 'l 'r)) 'b 'c))) (tuple-lens 4) (tuple-lens 1)))
;'r
(pretty-print (:. (tuple-pad 7 0 (tuple-encode (list (tuple-encode (list 'l 'r)) 'b 'c))) l0))
;'r
(pretty-print (:= (tuple-pad 7 0 (tuple-encode (list (tuple-encode (list 'l 'r)) 'b 'c))) 'rrr l0))
;(pair
; 0
; (pair
;  0
;  (pair
;   0
;   (pair 0 (pair (pair 'l (pair 'rrr (uno))) (pair 'b (pair 'c (uno))))))))


(pretty-print (bits-encode 15))
;(pair
; (bit (b-1))
; (pair (bit (b-1)) (pair (bit (b-1)) (pair (bit (b-1)) (uno)))))
(pretty-print (bits-decode (bits-encode 15)))
;15
(pretty-print (bits-pad 8 (bits-encode 15)))
;(pair
; (bit (b-0))
; (pair
;  (bit (b-0))
;  (pair
;   (bit (b-0))
;   (pair
;    (bit (b-0))
;    (pair
;     (bit (b-1))
;     (pair (bit (b-1)) (pair (bit (b-1)) (pair (bit (b-1)) (uno)))))))))
(pretty-print (bits-decode (bits-pad 8 (bits-encode 15))))
;15
(pretty-print (bits-encode 16))
;(pair
; (bit (b-1))
; (pair
;  (bit (b-0))
;  (pair (bit (b-0)) (pair (bit (b-0)) (pair (bit (b-0)) (uno))))))
(pretty-print (bits-decode (bits-encode 16)))
;16
(pretty-print (bits-pad 8 (bits-encode 16)))
;(pair
; (bit (b-0))
; (pair
;  (bit (b-0))
;  (pair
;   (bit (b-0))
;   (pair
;    (bit (b-1))
;    (pair
;     (bit (b-0))
;     (pair (bit (b-0)) (pair (bit (b-0)) (pair (bit (b-0)) (uno)))))))))
(pretty-print (bits-decode (bits-pad 8 (bits-encode 16))))
;16

(pretty-print (bits-select (bits-pad 4 (bits-encode 3)) 'default
             (list (cons (bits-pad 4 (bits-encode 7)) 'seven)
                   (cons (bits-pad 4 (bits-encode 4)) 'four))))
;'default
(pretty-print (bits-select (bits-pad 4 (bits-encode 3)) 'default
             (list (cons (bits-pad 4 (bits-encode 7)) 'seven)
                   (cons (bits-pad 4 (bits-encode 3)) 'three)
                   (cons (bits-pad 4 (bits-encode 4)) 'four))))
;'three


(pretty-print (nat-encode 7))
;(pair
; (bit (b-1))
; (pair
;  (bit (b-1))
;  (pair
;   (bit (b-1))
;   (pair
;    (bit (b-1))
;    (pair
;     (bit (b-1))
;     (pair (bit (b-1)) (pair (bit (b-1)) (pair (bit (b-0)) (uno)))))))))
(pretty-print (nat-decode (nat-encode 7)))
;7


(define test-ns 0)
(define test-symbol-keys (map (curry cons test-ns) '(ta tb tc)))
(pretty-print (map symbol-add test-symbol-keys))
(pretty-print (map symbol-encode test-symbol-keys))
(pretty-print (map symbol-decode (map symbol-encode test-symbol-keys)))
;'((0 . ta) (0 . tb) (0 . tc))

(symbol-add* '(one) 2)
(symbol-add* '(one two))
(symbol-add* '(one three))
(pretty-print (map symbol-encode* '((one) (one two) (one three))))
(pretty-print (map (curry symbol-encode** '()) '((one) (one two) (one three))))
(pretty-print
  (map (curry symbol-decode** '()) (map (curry symbol-encode** '()) '((one) (one two) (one three)))))
;'((one) (one two) (one three))


(define test-term-4 (right-x (parse (penv-vars-add penv-init 's0)
  `((lam (x) (pair (pair s0 x) (lam (y) (pair (pair y x) s0))))
    (pair s0 (lam (z) (pair s0 z))))
  )))

(define test-term-5 (right-x (parse (penv-vars-add penv-init 's0)
  `((lam (w) (lam (x)
      ((pair-access 1 (pair (pair s0 (pair x w))
                            (lam (y) (pair (pair y x) s0)))) 0)))
    () (pair s0 (lam (z) (pair s0 z))))
  )))

(interact-with test-term-5)
