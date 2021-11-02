#lang racket/base
(provide
  (all-from-out "microk-fo-def.rkt" "mk-fo.rkt")
  matcho
  evalo
  compile-match
  compile-match/value
  )

(require "microk-fo-def.rkt"
         "mk-fo.rkt"
         racket/pretty
         )

#|
;; Example from: http://minikanren.org/workshop/2020/minikanren-2020-paper8.pdf

'(match (x y z)
   ((_  #f #t) 1)
   ((#f #t _ ) 2)
   ((_  _  #f) 3)
   ((_  _  #t) 4))
;; otherwise return error value

;; correct but non-optimal
'(if x
   (if y
     (if z 4 3)
     (if z 1 3))
   (if y
     2
     (if z 1 3)))

;; optimal
'(if y
   (if x
     (if z 4 3)
     2)
   (if z 1 3))
;|#

(define-relation (pairo a)
  (fresh (x y) (== a (cons x y))))

(define-relation (not-pairo a)
  (for-all (x y) (=/= a (cons x y))))

(define-relation (matcho scrutinee clauses result)
  (conde ((== clauses '()) (== result 'FAILURE))
         ((fresh (pattern rhs clauses.remaining)
            (== clauses (cons (list pattern rhs) clauses.remaining))
            (match-many1o scrutinee pattern '() rhs scrutinee clauses.remaining result)))))

(define-relation (match-manyo vp* rhs v.full clauses.remaining result)
  (conde ((== vp* '()) (== result rhs))
         ((fresh (v p vp*.remaining)
            (== vp* (cons (cons v p) vp*.remaining))
            (match-many1o v p vp*.remaining rhs v.full clauses.remaining result)))))

(define-relation (match-many1o v p vp* rhs v.full clauses.remaining result)
  (conde ((==  p '_) (match-manyo vp* rhs v.full clauses.remaining result))
         ((=/= p '_)
          (conde ((==  v p) (not-pairo p) (match-manyo vp* rhs v.full clauses.remaining result))
                 ((fresh (p.fst p.snd v.fst v.snd)
                    (== p (cons p.fst p.snd))
                    (== v (cons v.fst v.snd))
                    (match-many1o v.fst p.fst
                                  (cons (cons v.snd p.snd) vp*)
                                  rhs v.full clauses.remaining result)))
                 ((=/= v p) (not-pairo p) (matcho v clauses.remaining result))
                 ((pairo p) (not-pairo v) (matcho v clauses.remaining result)))
          ;; an alternative approach that should have the same meaning
          #;(conde ((==  v p) (match-manyo vp* rhs v.full clauses.remaining result))
                 ((=/= v p)
                  (conde ((fresh (p.fst p.snd v.fst v.snd)
                            (== p (cons p.fst p.snd))
                            (== v (cons v.fst v.snd))
                            (match-many1o v.fst p.fst
                                          (cons (cons v.snd p.snd) vp*)
                                          rhs v.full clauses.remaining result)))
                         ((pairo p) (not-pairo v) (matcho v clauses.remaining result))
                         ((not-pairo p)           (matcho v clauses.remaining result)))))
          )))

(define-relation (evalo input expr value)
  (conde ((== expr `(quote ,value))
          ;(atomo value)
          )
         ((fresh (accessors v e.then e.else)
            (== expr `(if (pair? (access . ,accessors))
                        ,e.then
                        ,e.else))
            (accesso input accessors v)
            (conde ((pairo     v) (evalo input e.then value))
                   ((not-pairo v) (evalo input e.else value)))))
         ((fresh (v.lhs accessors.rhs v.rhs e.then e.else)
            (== expr `(if (equal? (quote ,v.lhs)
                                  (access . ,accessors.rhs))
                        ,e.then
                        ,e.else))
            (conde ((==  v.lhs v.rhs)
                    (accesso input accessors.rhs v.rhs)
                    ;(atomo v.lhs)
                    (evalo input e.then value))
                   ((=/= v.lhs v.rhs)
                    (accesso input accessors.rhs v.rhs)
                    ;(atomo v.lhs)
                    (evalo input e.else value)))))))

(define-relation (atomo value)
  (not-pairo value)
  #;(conde ((numbero value))
         ((symbolo value))
         ((stringo value))
         ((== value #t))
         ((== value #f))
         ((== value '())))
  )

(define-relation (accesso input accessors value)
  (conde ((== accessors '()) (== value input))
         ((fresh (accessor accessors.remaining input.next unused)
            (== accessors (cons accessor accessors.remaining))
            (conde ((== accessor 'fst) (== input (cons input.next unused)))
                   ((== accessor 'snd) (== input (cons unused input.next))))
            (accesso input.next accessors.remaining value)))))

(define (compile-match clauses)
  (run 1 (program)
    (for-all (v) (fresh (result)
                   (matcho v clauses result)
                   (evalo v program result)))))

(define (compile-match/value n v clauses)
  (run n (program)
    (fresh (result)
      (matcho v clauses result)
      (evalo v program result))))

(define-syntax-rule
  (PBE (in out) ...)
  (begin (pretty-write '(example: (PBE (in out) ...)))
         (pretty-write
           (time (run 1 (program) (evalo 'in program 'out) ...)))))

(define (example-compile-match clauses)
  (pretty-write `(example: (compile-match ,clauses)))
  (pretty-write (time (compile-match clauses))))

(define (example-compile-match/value . args)
  (pretty-write `(example: (compile-match/value . ,args)))
  (pretty-write (time (apply compile-match/value args))))

(pretty-write
  (time
    (run 2 (result)
      (matcho #t
              '((t 1)
                (#f 2)
                (_  3))
              result))))

(pretty-write
  (time
    (run 20 (i e v)
      (evalo i e v))))

#;(PBE
  ((#t . 1)  1)
  ((#t . 2)  1)
  ((#f . #t) 2)
  ((#f . 3)  3)
  ((#f . 4)  3)
  (7 4)
  (8 4)
  )

(example-compile-match/value
  3
  #t
  '((#t 1)
    (#f 2)
    (_  3)))

(example-compile-match/value
  3
  #f
  '((#t 1)
    (#f 2)
    (_  3)))

(example-compile-match/value
  3
  'foo
  '((#t 1)
    (#f 2)
    (_  3)))

(example-compile-match/value
  2
  #t
  '(((#t . _) 11)
    (#f 11)
    (_        22)))

(example-compile-match/value
  2
  '(#t . 7)
  '(((#t . _) 11)
    (_        22)))

(example-compile-match/value
  2
  '(#f . 7)
  '(((#t . _) 11)
    (_        22)))

(pretty-write 'BAD-EXAMPLES:)

(run 2 (result) (evalo 888 '(if (equal? '#t (access fst)) '123 '456) result))

(run 2 (result) (matcho 888
                        '(((#t . _) 123)
                          (_        456))
                        result))

(run 1 ()
  (fresh (result)
    (matcho 888
            '(((#t . _) 123)
              (_        456))
            result)
    (evalo 888 '(if (equal? '#t (access fst)) '123 '456) result)))

(run 1 ()
  (for-all (v)
           (fresh (result)
             (matcho v
                     '(((#t . _) 123)
                       (_        456))
                     result)
             (evalo v '(if (equal? '#t (access fst)) '123 '456) result))))

(pretty-write 'HARDCODED-EXAMPLES:)

(define (compiles== clauses program)
  (pretty-write `(compiles== ,clauses ,program))
  (pretty-write
    (time
      (run 1 ()
        (for-all (v) (fresh (result)
                       (matcho v clauses result)
                       (evalo v program result)))))))

(compiles==
  '(((#t . _) 11)
    (_        22))
  '(if (pair? (access))
     (if (equal? '#t (access fst))
       '11
       '22)
     '22))

(compiles==
  '(((_ . #f) 111)
    (_        222))
  '(if (pair? (access))
     (if (equal? '#f (access snd))
       '111
       '222)
     '222))

#;(compiles==
  '(((_ . #f) 1)
    ((_ . #t) 2)
    (_        3))
  '(if (pair? (access))
     (if (equal? '#f (access snd))
       '1
       (if (equal? '#t (access snd))
         '2
         '3))
     '3))

#;(compiles==
  '(((_  . #f) 1)
    ((#f . #t) 2)
    (_         3))
  '(if (pair? (access))
     (if (equal? '#f (access snd))
       '1
       (if (equal? '#t (access snd))
         (if (equal? '#f (access fst))
           '2
           '3)
         '3))
     '3))

#;(compiles==
  '(((_  #f #t) 1)
    ((#f #t _ ) 2)
    ((_  _  #f) 3)
    ((_  _  #t) 4))
  '(if (pair? (access))
     (if (pair? (access snd))
       (if (pair? (access snd snd))
         (if (equal? '() (access snd snd snd))

           _TODO

           'FAILURE)
         'FAILURE)
       'FAILURE)
     'FAILURE))

(pretty-write 'FULL-EXAMPLES:)

;; ~5 milliseconds
(example-compile-match
  '((_ 1)))

;; ~5 seconds
(example-compile-match
  '((#t 1)
    (#f 2)
    (_  3)))

;; ~9 seconds, but is the answer correct?
(example-compile-match
  '(((#t . _) 11)
    (_        22)))

;; TODO: how long do these examples take?

(example-compile-match
  '(((_ . #f) 111)
    (_        222)))

;; TODO: resolve shrink-away error?
(example-compile-match
  '(((_  . #f) 1)
    ((#f . #t) 2)
    (_         3)))

(example-compile-match
  '(((_  #f #t) 1)
    ((#f #t _ ) 2)
    ((_  _  #f) 3)
    ((_  _  #t) 4)))
