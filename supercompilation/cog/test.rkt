#lang racket
(require
  "data-encoding.rkt"
  "interaction.rkt"
  "parsing.rkt"
  "semantics-denotational.rkt"
  "semantics-operational.rkt"
  "syntax-0-parsing.rkt"
  "syntax-0-unparsing.rkt"
  "syntax-1-bootstrapping.rkt"
  "syntax-1-parsing.rkt"
  "syntax-1.rkt"
  "syntax-abstract.rkt"
  "util.rkt"
  )

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
