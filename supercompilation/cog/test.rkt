#lang racket
(require "util.rkt")
(require "syntax-abstract.rkt")
(require "substitution.rkt")
(require "semantics-denotational.rkt")
(require "semantics-operational.rkt")
(require "data-encoding.rkt")
(require "parsing.rkt")
(require "syntax-0-parsing.rkt")
(require "syntax-0-unparsing.rkt")
(require "interaction.rkt")
(require "syntax-1-bootstrapping.rkt")
(require "syntax-1-parsing.rkt")
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
  ((lam (a b c) (produce ())) (produce 0) (produce 1) (produce (pair 0 1)))
  ((lam (x) (pair x (lam (y) x))) (lam (a) ()))
  ((lam (x) (subst ((tuple 0 1 0) 1) 0 x)) ())  ; this example is nonsense
  ))

(define parsed-tests (right-x (map-parse-0 penv-init-0 tests)))
(define unparsed-tests (map (curry unparse upenv-empty) parsed-tests))
(pretty-print unparsed-tests)

(define (eval-print el)
  (print (denote-eval noisy-consume el)) (display "\n"))
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


(define test-term-4 (right-x (parse-0 (penv-vars-add penv-init-0 's0)
  `((lam (x) (pair (pair s0 x) (lam (y) (pair (pair y x) s0))))
    (pair s0 (lam (z) (pair s0 z))))
  )))

(define test-term-5 (right-x (parse-0 (penv-vars-add penv-init-0 's0)
  `((lam (w) (lam (x)
      ((pair-access 1 (pair (pair s0 (pair x w))
                            (lam (y) (pair (pair y x) s0)))) 0)))
    () (pair s0 (lam (z) (pair s0 z))))
  )))

(define test-term-6 (right-x (parse-0 (penv-vars-add penv-init-0 's0)
  `((fix (repeat count val)
         (if-0 (pair-l count)
               ()
               ((lam (rest) (pair val rest)) (repeat (pair-r count) val))))
    ,(unparse upenv-empty (value (nat-encode 5)))
    s0))))

(interact-with test-term-6)
(interact-with-0 (std `(
  (bits-assoc (tuple 1 0 1)
    (pair (tuple 1 1 0)
          (tuple (pair (pair (tuple 1 1 1 1 0) (tuple 0 1 0 1)) (tuple 1 0))
                 (pair (pair (tuple 1 1 1 1 0) (tuple 0 1 0 0)) (tuple 0 1))))
    (pair (tuple 1 1 1 1 0) (tuple 0 1 0 0))))))
;(interact-with-0 (std `(
  ;(bits-unsized-eq? (tuple 1 1 1 1 1 0)
    ;(tuple 0 0 1 0 1) (tuple 0 0 1 0 1)))))


(define prog (std-1 `(pair 0b (pair 1b (pair bit? ())))))
(define prog2 (std-1 `(pair 0b (pair 1b (pair 0b ())))))
(define prog3 (std-1 '0b))

(denote-eval noisy-consume (std-1 `((lam (x) (pair x ())) (lam (a b) a))))
(denote-eval noisy-consume (std-1 `(produce (sym? 0b))))
(denote-eval noisy-consume (std-1 `(produce (bit? 0b))))
(denote-eval noisy-consume (std-1 `(produce (uno? 0b))))
