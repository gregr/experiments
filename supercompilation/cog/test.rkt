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
  "workspace.rkt"
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
  ;((lam (x) (subst ((tuple 0 1 0) 1) 0 x)) ())  ; this example is nonsense
  ))

(define parsed-tests (right-x (map-parse-0 penv-init-0 tests)))
(define unparsed-tests (map (curry unparse upenv-empty) parsed-tests))

(module+ main
  (pretty-print unparsed-tests)
  )

(define (eval-print el)
  (print (denote-eval noisy-consume el)) (display "\n"))
(module+ main
  (let ((_ (map eval-print parsed-tests))) (void))
  )

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

(define test-term-7
  (right-x (parse-0 penv-init-0
    (std `(
           (bits-assoc (tuple 1 0 1)
                       (pair (tuple 1 1 0)
                             (tuple (pair (pair (tuple 1 1 1 1 0) (tuple 0 1 0 1)) (tuple 1 0))
                                    (pair (pair (tuple 1 1 1 1 0) (tuple 0 1 0 0)) (tuple 0 1))))
                       (pair (tuple 1 1 1 1 0) (tuple 0 1 0 0))))))))

(define prog (std-1 `(pair 0b (pair 1b (pair bit? ())))))
(define prog2 (std-1 `(pair 0b (pair 1b (pair 0b ())))))
(define prog3 (std-1 '0b))

(module+ main
  (denote-eval noisy-consume (std-1 `((lam (x) (pair x ())) (lam (a b) a))))
  (denote-eval noisy-consume (std-1 `(produce (sym? 0b))))
  (denote-eval noisy-consume (std-1 `(produce (bit? 0b))))
  (denote-eval noisy-consume (std-1 `(produce (uno? 0b))))

  (displayln "\n")

  (define (test-int term) (interaction (curry unparse upenv-empty) (::0 term) '()))
  (define test-tab-0 (tab (void) (list 0 1 3)))
  (define test-tab-1 (tab (void) (list 1 2 3)))
  (define test-int-0 (test-int (value (pair (uno) (bit (b-0))))))
  (define test-int-1 (test-int (value (pair (uno) (bit (b-1))))))
  (define test-int-2 (test-int test-term-6))
  (define test-int-3 (test-int (value (pair (bit (b-0)) (bit (b-1))))))
  (define test-idb (:~* interaction-db-empty
                        (lambda (hv) (hash-set* hv 0 test-int-0 1 test-int-1 2 test-int-2 3 test-int-3))
                        'uid->interaction))
  (define test-ws-view-0 (workspace-view 0 (list 0 0)))
  (define test-ws-0 (workspace (list test-tab-0 test-tab-1) test-idb))
  (define test-ws-view-1 (workspace-view 1 (list 0 1)))
  (define test-ws-1 (workspace (list test-tab-0 test-tab-1) test-idb))

  (display (present-workspace test-ws-view-0 test-ws-0))
  (display "\n\n")
  (display (present-workspace test-ws-view-1 test-ws-1))
  (display "\n\n")
  (display (present-workspace workspace-view-empty workspace-empty))
  (display "\n\n")
  )

(module+ main
  (interact-with test-term-6)
  (interact-with test-term-7)
  ;(interact-with-0 (std `(
    ;(bits-unsized-eq? (tuple 1 1 1 1 1 0)
      ;(tuple 0 0 1 0 1) (tuple 0 0 1 0 1)))))
  )

(module+ test
  (require rackunit)
  (check-equal?
    (step-complete test-term-6)
    (value
      (pair
        (bvar 0)
        (pair (bvar 0) (pair (bvar 0) (pair (bvar 0) (pair (bvar 0) (uno)))))))
    )
  (check-equal?
    (step-complete test-term-7)
    (value (pair (bit (b-0)) (pair (bit (b-1)) (uno))))
    )
  )
