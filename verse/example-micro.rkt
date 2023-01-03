#lang racket/base
(require "micro.rkt" racket/pretty)

(define detect-loops? #t)

(define env.initial env.empty)

(define (run*       E) (run       #f E))
(define (trace-run* E) (trace-run #f E))

(define (run count E)
  (let loop ((count count) (st (initial-state env.initial E)))
    (define (stop) (state-reify st))
    (cond
      ((eqv? count 0)        (stop))
      ((not st)              #f)
      ((value? (state-E st)) (stop))
      (else                  (let ((st.next (state-step st)))
                               (if (and detect-loops? (equal? st st.next))
                                   (stop)
                                   (loop (and count (- count 1)) st.next)))))))

(define (trace-run count E)
  (let loop ((count count) (st (initial-state env.initial E)))
    (define (stop) (list (state-reify st)))
    (cond
      ((eqv? count 0)        (stop))
      ((not st)              (list #f))
      ((value? (state-E st)) (stop))
      (else                  (let ((st.next (state-step st)))
                               (if (and detect-loops? (equal? st st.next))
                                   (stop)
                                   (cons (state-reify st)
                                         (loop (and count (- count 1)) st.next))))))))

(define (show A B)
  (newline)
  (pretty-write A)
  (pretty-write '==>*)
  (pretty-write B))

(for-each
  ;(lambda (E) (time (show E (trace-run 0 E))))  ; view the initial "start" expression without taking any steps
  ;(lambda (E) (time (show E (trace-run* E))))   ; show the entire "step" history
  (lambda (E) (time (show E (run* E))))          ; show only the final result
  `(
    ;;;;;;;;;;;;;
    ;;; Basic ;;;
    ;;;;;;;;;;;;;

    (value #t)

    (op cons (value a) (value b))

    (all (exist (X)
           (seq (== (ref X) (alt (value 1) (value 2)))
                (op cons (ref X) (ref X)))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; From the VC paper ;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;

    (exist (x y z)
      (seq (seq (== (ref x) (op cons (ref y) (value 3)))
                (== (ref x) (op cons (value 2) (ref z))))
           (ref y)))

    (exist (first)
      (seq (== (ref first) (lam pr (exist (a b)
                                     (seq (== (ref pr) (op cons (ref a) (ref b)))
                                          (ref a)))))
           (exist (x y)
             (seq (seq (== (ref x) (op cons (ref y) (value 5)))
                       (== (app (ref first) (ref x)) (value 2)))
                  (ref y)))))

    ;; Simplification of the previous example:
    (exist (first)
      (seq (== (ref first) (lam pr (exist (a b)
                                     (seq (== (ref pr) (op cons (ref a) (ref b)))
                                          (ref a)))))
           (exist (y)
             (seq (== (app (ref first) (op cons (ref y) (value 5))) (value 2))
                  (ref y)))))

    (exist (x y)
      (seq (seq (op +o (value 3) (ref y) (ref x))
                (== (ref y) (value 7)))
           (ref x)))

    (all (exist (x)
           (seq (== (ref x) (alt (value 7) (value 5)))
                (op cons (value 3) (ref x)))))

    (all (exist (x y)
           (seq (seq (== (ref x) (alt (value 3) (value 4)))
                     (== (ref y) (alt (value 20) (value 30))))
                (op cons (ref x) (ref y)))))

    (all (exist (x)
           (alt (seq (== (ref x) (value 3))
                     (exist (out)
                       (seq (op +o (ref x) (value 1) (ref out))
                            (ref out))))
                (seq (== (ref x) (value 4))
                     (exist (out)
                       (seq (op *o (ref x) (value 2) (ref out))
                            (ref out)))))))

    (all (exist (append)
           (seq (== (ref append)
                    (lam pr
                         (exist (xs ys)
                           (seq (== (ref pr) (op cons (ref xs) (ref ys)))
                                (alt (seq (== (ref xs) (value ()))
                                          (ref ys))
                                     (exist (x xrest)
                                       (seq (== (ref xs) (op cons (ref x) (ref xrest)))
                                            (op cons (ref x)
                                                (app (ref append) (op cons (ref xrest) (ref ys)))))))))))
                (exist (single)
                  (seq (== (ref single) (op cons (value 1) (value ())))
                       (exist (zs)
                         (seq (== (app (ref append) (op cons (ref zs) (ref single)))
                                  (ref single))
                              (ref zs))))))))

    ;; A more miniKanren-like example of running backwards:
    (all (exist (append)
           (seq (== (ref append)
                    (lam pr
                         (exist (xs ys)
                           (seq (== (ref pr) (op cons (ref xs) (ref ys)))
                                (alt (seq (== (ref xs) (value ()))
                                          (ref ys))
                                     (exist (x xrest)
                                       (seq (== (ref xs) (op cons (ref x) (ref xrest)))
                                            (op cons (ref x)
                                                (app (ref append) (op cons (ref xrest) (ref ys)))))))))))
                (exist (out)
                  (seq (== (ref out) (op cons (value 1) (op cons (value 2) (op cons (value 3) (value ())))))
                       (exist (as bs)
                         (seq (== (app (ref append) (op cons (ref as) (ref bs)))
                                  (ref out))
                              (op cons (ref as) (op cons (ref bs) (value ()))))))))))

    ;; Unground list elements are fine:
    (all (exist (append)
           (seq (== (ref append)
                    (lam pr
                         (exist (xs ys)
                           (seq (== (ref pr) (op cons (ref xs) (ref ys)))
                                (alt (seq (== (ref xs) (value ()))
                                          (ref ys))
                                     (exist (x xrest)
                                       (seq (== (ref xs) (op cons (ref x) (ref xrest)))
                                            (op cons (ref x)
                                                (app (ref append) (op cons (ref xrest) (ref ys)))))))))))
                (exist (Q out)
                  (seq (== (ref out) (op cons (value 1) (op cons (ref Q) (op cons (value 3) (value ())))))
                       (exist (as bs)
                         (seq (== (app (ref append) (op cons (ref as) (ref bs)))
                                  (ref out))
                              (op cons (ref as) (op cons (ref bs) (value ()))))))))))

    (all (exist (x y)
           (seq (== (ref y)
                    (app (one (alt (seq (== (ref x) (value 0))
                                        (lam #f (value 3)))
                                   (lam #f (value 4))))
                         (value ())))
                (seq (== (ref x) (value 7))
                     (ref y)))))

    (all (exist (t out)
           (seq (== (ref t) (all (alt (value 10) (alt (value 27) (value 32)))))
                (seq (op vector-refo (ref t) (alt (value 1) (alt (value 0) (value 1))) (ref out))
                     (ref out)))))

    (all (exist (t out)
           (seq (== (ref t) (all (alt (value 10) (alt (value 27) (value 32)))))
                (exist (i)
                  (seq (op vector-refo (ref t) (ref i) (ref out))
                       (ref out))))))

    (one (alt (value 1)
              (exist (loop)
                (seq (== (ref loop)
                         (lam #f (app (ref loop) (value ()))))
                     (app (ref loop) (value ()))))))

    ;; Infinite loop (we can sometimes detect loops and stop):
    ;(all (alt (value 1)
    ;          (exist (loop)
    ;            (seq (== (ref loop)
    ;                     (lam unit (app (ref loop) (ref unit))))
    ;                 (app (ref loop) (value ()))))))

    ,@(let*
        ((vif  (lambda (scope e1 e2 e3) `(app (one (alt (exist ,scope (seq ,e1 (lam #f ,e2)))
                                                        (lam #f ,e3)))
                                              (value ()))))
         (vlib (lambda (e)
                 `(exist (vector-ref vector-length head tail cons map)
                    (seq
                      (seq
                        (seq
                          (seq (== (ref vector-ref)
                                   (lam pr
                                        (exist (vec i out)
                                          (seq (== (ref pr) (op cons (ref vec) (ref i)))
                                               (seq (op vector-refo (ref vec) (ref i) (ref out))
                                                    (ref out))))))
                               (== (ref vector-length)
                                   (lam vec
                                        (exist (out)
                                          (seq (op vector-lengtho (ref vec) (ref out))
                                               (ref out))))))
                          (seq (seq (== (ref head)
                                        (lam xs (app (ref vector-ref) (op cons (ref xs) (value 0)))))
                                    (== (ref tail)
                                        (lam xs (all (exist (i)
                                                       (seq (op < (value 0) (ref i))
                                                            (app (ref vector-ref) (op cons (ref xs) (ref i)))))))))
                               (== (ref cons)
                                   (lam pr (exist (x xs)
                                             (seq (== (ref pr) (op cons (ref x) (ref xs)))
                                                  (all (alt (ref x)
                                                            (exist (i)
                                                              (app (ref vector-ref) (op cons (ref xs) (ref i))))))))))))
                        (== (ref map)
                            (lam pr
                                 (exist (f xs)
                                   (seq (== (ref pr) (op cons (ref f) (ref xs)))

                                        ; NOTE: these are other ways we could define map:
                                        ;one (alt
                                        ;      (seq (== (app (ref vector-length) (ref xs)) (value 0))
                                        ;           (all (== (value #t) (value #f))))
                                        ;      (seq (op < (value 0) (app (ref vector-length) (ref xs)))
                                        ;           (app (ref cons)
                                        ;                (op cons
                                        ;                    (app (ref f) (app (ref head) (ref xs)))
                                        ;                    (app (ref map)
                                        ;                         (op cons
                                        ;                             (ref f)
                                        ;                             (app (ref tail) (ref xs)))))))))

                                        ;one (alt (exist (x)
                                        ;           (seq
                                        ;             (== (ref x) (app (ref head) (ref xs)))
                                        ;             (app (ref cons)
                                        ;                  (op cons
                                        ;                      (app (ref f) (ref x))
                                        ;                      (app (ref map)
                                        ;                           (op cons
                                        ;                               (ref f)
                                        ;                               (app (ref tail) (ref xs))))))))
                                        ;           (all (== (value #t) (value #f)))))

                                        ;app (one (alt (seq
                                        ;                (app (ref head) (ref xs))
                                        ;                (lam #f (app (ref cons)
                                        ;                             (op cons
                                        ;                                 (app (ref f) (app (ref head) (ref xs)))
                                        ;                                 (app (ref map)
                                        ;                                      (op cons
                                        ;                                          (ref f)
                                        ;                                          (app (ref tail) (ref xs))))))))
                                        ;              (lam #f (all (== (value #t) (value #f))))))
                                        ;    (value ()))

                                        ;(app (one (alt
                                        ;            (exist (x)
                                        ;              (seq
                                        ;                (== (ref x) (app (ref head) (ref xs)))
                                        ;                (lam #f (app (ref cons)
                                        ;                             (op cons
                                        ;                                 (app (ref f) (ref x))
                                        ;                                 (app (ref map)
                                        ;                                      (op cons
                                        ;                                          (ref f)
                                        ;                                          (app (ref tail) (ref xs)))))))))
                                        ;            (lam #f (all (== (value #t) (value #f))))))
                                        ;     (value ()))

                                        ,(vif '(x)
                                              '(== (ref x) (app (ref head) (ref xs)))
                                              '(app (ref cons)
                                                    (op cons
                                                        (app (ref f) (ref x))
                                                        (app (ref map) (op cons (ref f) (app (ref tail) (ref xs))))))
                                              '(all (== (value #t) (value #f)))))))))
                      ,e))))
         ;; The paper writes it this way, but this introduces an unnecessary variable:
         ;(vfor (lambda (scope e1 e2)
         ;        (vlib `(exist (v)
         ;                 (seq (== (ref v) (all (exist ,scope (seq ,e1 (lam #f ,e2)))))
         ;                      (app (ref map) (op cons
         ;                                         (lam z (app (ref z) (value ())))
         ;                                         (ref v))))))))
         (vfor (lambda (scope e1 e2)
                 (vlib `(app (ref map) (op cons
                                           (lam z (app (ref z) (value ())))
                                           (all (exist ,scope (seq ,e1 (lam #f ,e2))))))))))
        (list
          (vfor '(x)
                '(seq (== (ref x) (alt (value 2) (alt (value 3) (value 5))))
                      (op < (value 2) (ref x)))
                '(exist (out)
                   (seq (op +o (ref x) (value 1) (ref out))
                        (ref out))))

          (vfor '(x y)
                '(seq (== (ref x) (alt (value 10) (value 20)))
                      (== (ref y) (alt (value 1) (alt (value 2) (value 3)))))
                '(exist (out)
                   (seq (op +o (ref x) (ref y) (ref out))
                        (ref out))))

          `(all ,(vfor '(x)
                       '(== (ref x) (alt (value 10) (value 20)))
                       '(alt (ref x)
                             (exist (out)
                               (seq (op +o (ref x) (value 1) (ref out))
                                    (ref out))))))

          '(all (exist (y)
                  (seq (op +o (value 3) (value 4) (ref y))
                       (app (lam x (exist (out)
                                     (seq (op +o (ref x) (value 1) (ref out))
                                          (ref out))))
                            (ref y)))))

          '(all (exist (x)
                  (seq (op vector-refo
                           (all (alt (value 2) (alt (value 3) (alt (value 2) (alt (value 7) (value 9))))))
                           (ref x) (value 2))
                       (ref x))))

          `(exist (x)
             (== (ref x) ,(vif '()
                               '(seq (== (ref x) (value 0))
                                     (op < (value 1) (ref x)))
                               '(value 33)
                               '(value 55))))

          '(all (exist (x y)
                  (seq (== (ref y) (alt (seq (== (ref x) (value 3))
                                             (exist (out)
                                               (seq (op *o (ref x) (value 2) (ref out))
                                                    (ref out))))
                                        (== (ref x) (value 4))))
                       (all (alt (exist (out)
                                   (seq (op +o (ref x) (value 1) (ref out))
                                        (ref out))) (ref y))))))

          `(all (exist (x)
                  (seq ,(vif '()
                             '(op < (value 0) (ref x))
                             '(value 55)
                             '(value 44))
                       (seq (== (ref x) (value 1))
                            (alt (value 77) (value 99))))))

          ;; Known to get stuck:
          '(all (exist (f)
                  (seq (app (ref f) (value ()))
                       (alt (== (value #t) (value #f))
                            (== (value 3) (alt (value 1) (value 3)))))))

          `(all (exist (f x y)
                  (seq (seq (== (ref f) (lam p (seq (== (ref x) (value 7))
                                                    (ref p))))
                            (== (ref y) ,(vif '()
                                              '(op < (value 0) (ref x))
                                              '(value 7)
                                              '(value 8))))
                       (app (ref f) (ref y)))))
          ))

    (all (exist (out)
           (seq (op +o (value 3) (alt (value 20) (value 30)) (ref out))
                (ref out))))

    (all (alt (value 1) (alt (value 7) (value 2))))

    (exist (i out) (seq (op vector-refo
                            (all (alt (value 1) (alt (value 7) (value 2))))
                            (ref i) (ref out))
                        (ref out)))
    ))
