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

;; TODO: enrich the matching language to support destructuring patterns
(define-relation (matcho scrutinee clauses result)
  (conde ((== clauses '()) (== result 'FAILURE))
         ((fresh (pattern rhs clauses.remaining)
            (== clauses (cons (list pattern rhs) clauses.remaining))
            (conde ((==  '_ pattern) (== result rhs))
                   ((=/= '_ pattern)
                    (conde ((==  scrutinee pattern) (== result rhs))
                           ((=/= scrutinee pattern) (matcho scrutinee clauses.remaining result)))))))))

(define-relation (evalo input expr value)
  (conde ((== expr `(quote ,value)))
         ((fresh (v.lhs accessors.rhs v.rhs e.then e.else)
            (== expr `(if (equal? (quote ,v.lhs)
                                  (access . ,accessors.rhs))
                        ,e.then
                        ,e.else))
            (conde ((==  v.lhs v.rhs)
                    (accesso input accessors.rhs v.rhs)
                    (evalo input e.then value))
                   ((=/= v.lhs v.rhs)
                    (accesso input accessors.rhs v.rhs)
                    (evalo input e.else value)))))))

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

#;(pretty-write
  (time
    (run 2 (result)
      (matcho #t
              '((t 1)
                (#f 2)
                (_  3))
              result))))

#;(pretty-write
  (time
    (run 20 (i e v)
      (evalo i e v))))

#;(example-compile-match/value
  10
  ;#t
  ;#f
  'foo
  '((#t 1)
    (#f 2)
    (_  3)))

(pretty-write 'EXAMPLES:)

;; ~5 milliseconds
(example-compile-match
  '((_ 1)))

;; ~5 seconds
(example-compile-match
  '((#t 1)
    (#f 2)
    (_  3)))

;; TODO: enrich the matching language to support destructuring patterns
#;(compile-match
  '(((_  #f #t) 1)
    ((#f #t _ ) 2)
    ((_  _  #f) 3)
    ((_  _  #t) 4)))
