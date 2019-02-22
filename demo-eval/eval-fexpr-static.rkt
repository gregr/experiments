#lang racket

; Lists are syntactic sugar:
; (a b c)    ==> (a . (b . (c . ())))
; (a b . c)  ==> (a . (b . c))

;; Quasi-map (map over improper lists)
;; e.g., (~map2 list '(a b . c) '(1 2 3 4 5))
;;       =
;;       ((a 1) (b 2) (c (3 4 5)))
(define (~map2 f xs ys)
  (cond ((pair? xs) (cons (f (car xs) (car ys))
                          (~map2 f (cdr xs) (cdr ys))))
        ((null? xs) '())
        (else       (list (f xs ys)))))

;; Environments
(define env-empty '())

(define (env-ref env name)
  (define rib (assoc name env))
  (unless rib (error "unbound identifier:" name))
  (cdr rib))

(define (env-ref-syntax? env name)
  (and (symbol? name) (car (env-ref env name))))

(define (env-ref-value env name)
  (cdr (env-ref env name)))

(define (env-remove* env names)
  (remf* (lambda (rib) (member (car rib) names)) env))

;(append
  ;'((a . 1)
    ;(b . 2)
    ;(c . (3 4 5))
    ;)
  ;env ;; removed a b c
  ;)

;((name . (syntax? . value))
 ;...
 ;)

(define (env-extend* env bindings)
  (append bindings (env-remove* env (map car bindings))))

; Here is John Shutt's calculus for reference.  I'll show my modifications after:

; S   ::=   s | () | #t | #f | (S . S)   .

; T ::= x | [combine T T T] | ⟨λx.T⟩ | ⟨εx.T⟩ | [eval T T] | ⟨wrap T⟩ | e | S   .

; [eval d T]   →   d
         ;if d is an empty list, boolean, λ-function, ε-function, or environment
; [eval s e]   →   lookup(s,e)     if symbol s is bound in environment e
; [eval (T1 T2) T3]   →   [combine [eval T1 T3] T2 T3]
; [combine ⟨wrap T1⟩ T2 T3]   →   [combine T1 [eval T2 T3] T3]   .
; [combine ⟨λx.T1⟩ T2 T3]   →   T1[x ← T2]
; [combine ⟨εx.T1⟩ T2 T3]   →   [combine T1[x ← T3] T2 T3]   .


; In my system, S is the same, but T and its rewrites no longer need his extra interpretation machinery for combine.  Specifically, apply replaces and simplifies combine, there's no need for wrap, and there's no need for the environment binder (epsilon).  In other words, it's just like what he calls M, plus eval.  All the fexpr magic now comes from how eval interprets bindings in the environment (which is why I call this approach "static"):

; T ::= x | ⟨λx.T⟩ | ⟨apply T T⟩ | ⟨if T T T⟩ ... etc. | [eval T T] | S   .

; ⟨apply ⟨λx.T1⟩ T2⟩  →   T1[x ← T2]
; plus typical rules for extended lambda calculus terms, ⟨if T T T⟩ ... etc.

; [eval d T]   →   d
         ;if d is an empty list, boolean, number, etc.
; ;; This is the interesting application rule for fexpr magic.
; ;; Notice T is not evaluated, and that the calling environment is also passed in:
; [eval (s T) e]   →  ⟨apply ⟨apply [eval s e] e⟩ T⟩
         ;if s is bound in e and syntax?(s,e) = #t
; ;; This is the uninteresting application rule you're familiar with:
; [eval (T1 T2) e]   →   ⟨apply [eval T1 e] [eval T2 e]⟩
; [eval s e]   →   lookup(s,e)
         ;if symbol s is bound in environment e


; These calculi are independent of any particular operative (John's name for a fexpr) definitions, which would be bound in the environment.  As an example of a typical LAMBDA (assumed to be bound to the appropriate operative) he mentions in prose: "For example, [eval (LAMBDA X FOO) e0] would evaluate to a function with static environment e0, of the form ⟨wrap ⟨λx.[eval FOO ⟪...⟫]⟩⟩ with the contents of e0 embedded somewhere in the ⟪...⟫".  He's roughly saying this:

; [eval (LAMBDA X FOO) e0]  →*  ⟨wrap ⟨λx.[eval FOO ⟪(X x) . e0⟫]⟩⟩

; In my system, you would define a different LAMBDA, which binds names that respond #f to `syntax?`, and possibly something called LAMBDA-SYNTAX for binding names that respond #t to `syntax?`.  Given standard bindings in e0, something like:

; [eval (LAMBDA X FOO) e0]  →*   ⟨λx.[eval FOO ⟪(X #f x) . e0⟫]⟩

; [eval (LAMBDA-SYNTAX X FOO) e0]  →*   ⟨λx.[eval FOO ⟪(X #t x) . e0⟫]⟩

; It's just the usual lexically scoped evaluation of the body.  The only difference is #f vs. #t in the environment extension.

; As a little more context to help you check your understanding, for these transitions to work, the standard env e0 must have bindings for LAMBDA and LAMBDA-SYNTAX of this form:
; ⟪(LAMBDA #t <an appropriate λ term>)
   ;. ((LAMBDA-SYNTAX #t <an appropriate λ term>)
     ;. rest-of-e0)⟫


; T ::= x | ⟨λx.T⟩ | ⟨apply T T⟩ | ⟨if T T T⟩ ... etc. | [eval T T] | S   .

; ⟨apply ⟨λx.T1⟩ T2⟩  →   T1[x ← T2]
; plus typical rules for extended lambda calculus terms, ⟨if T T T⟩ ... etc.

;; Evaluation
(define (eval env form)
  (cond
    ; ;; This is the interesting application rule for fexpr magic.
    ; ;; Notice T is not evaluated, and that the calling environment is also passed in:
    ; [eval (s T) e]   →  ⟨apply ⟨apply [eval s e] e⟩ T⟩
    ;   if s is bound in e and syntax?(s,e) = #t
    ; ;; This is the uninteresting application rule you're familiar with:
    ; [eval (T1 T2) e]   →   ⟨apply [eval T1 e] [eval T2 e]⟩
    ((pair? form)
     (define rator (car form))
     (define rands (cdr form))
     (define proc (eval env rator))
     (if (and (symbol? rator) (env-ref-syntax? env rator))
       ;; syntax
       (apply proc env rands)
       ;; not syntax
       (apply proc (eval* env rands))))
    ;; variables
    ; [eval s e]   →   lookup(s,e)
    ;  if symbol s is bound in environment e
    ((symbol? form) (env-ref-value env form))
    ;; literals
    ; [eval d T]   →   d
    ;  if d is an empty list, boolean, number, etc.
    (#t form)))

(define (eval* env forms)
  (map (lambda (form) (eval env form)) forms))

(define (@quote env datum) datum)

(define (@if env c t f)
  (if (eval env c)
    (eval env t)
    (eval env f)))

(define (@lambda env param body)
  (lambda args
    (eval (env-extend*
            env (~map2 (lambda (p a)
                         (cons p (cons #f a))) param args))
          body)))

(define env:initial
  `((@               #f . ,(lambda (proc . args) (apply proc args)))
    (eval            #f . ,eval)
    (eval*           #f . ,eval*)
    (apply           #f . ,apply)
    (pair?           #f . ,pair?)
    (symbol?         #f . ,symbol?)
    (number?         #f . ,number?)
    (null?           #f . ,null?)
    (procedure?      #f . ,procedure?)
    (equal?          #f . ,equal?)
    (list            #f . ,list)
    (cons            #f . ,cons)
    (car             #f . ,car)
    (cdr             #f . ,cdr)
    (cadr            #f . ,cadr)
    (cdar            #f . ,cdar)
    (cddr            #f . ,cddr)
    (caar            #f . ,caar)
    (cadar           #f . ,cadar)
    (+               #f . ,+)
    (-               #f . ,-)
    (*               #f . ,*)
    (/               #f . ,/)
    (=               #f . ,=)
    (>               #f . ,>)
    (<               #f . ,<)
    (>=              #f . ,>=)
    (<=              #f . ,<=)
    (map             #f . ,map)
    (~map2           #f . ,~map2)
    (displayln       #f . ,displayln)
    (env-ref         #f . ,env-ref)
    (env-ref-syntax? #f . ,env-ref-syntax?)
    (env-ref-value   #f . ,env-ref-value)
    (env-remove*     #f . ,env-remove*)
    (env-extend*     #f . ,env-extend*)

    (quote           #t . , @quote)
    (if              #t . , @if)
    (lambda          #t . , @lambda)

    ;; $ applies a procedure as if it is a syntax operator.
    ($               #t . ,(lambda (env rator . rands)
                             (apply (eval env rator) env rands)))))

;; Bootstrap env:base, which will contain bindings for:
;;   lambda-syntax, let-syntax, let, cond
(define bootstrap-env:base
  '((lambda (@lambda-syntax)
      ;; Use @lambda-syntax to bind itself as the syntax: lambda-syntax
      (($ @lambda-syntax (lambda-syntax)
          ((lambda-syntax (let-syntax)
             (let-syntax ((let (lambda (env bindings body)
                                 (apply (@ lambda env (map car bindings) body)
                                        (eval* env (map cadr bindings))))))
               (let ((fix (lambda (f)
                            ((lambda (d) (d d))
                             (lambda (x) (f (lambda arg
                                              (apply (x x) arg))))))))

                 (let-syntax
                   ((cond (fix (lambda (@cond)
                                 (lambda (env . clauses)
                                   (if (null? clauses) #t
                                     (if (eval env (caar clauses))
                                       (eval env (cadar clauses))
                                       (apply @cond env (cdr clauses)))))))))

                   ;; env:base
                   ($ (lambda (env) env))))))

           ;; let-syntax
           ;;     (let-syntax ((x a) (y b) ...) body)
           ;; ==> ((lambda-syntax (x y ...) body) a b ...)
           (lambda (env bindings body)
             (apply (@lambda-syntax env (map car bindings) body)
                    (eval* env (map cadr bindings))))))

       ;; lambda-syntax
       @lambda-syntax))

    ;; @lambda-syntax
    (lambda (env param body)
      (lambda args
        (eval (env-extend*
                env (~map2 (lambda (p a)
                             (cons p (cons #t a))) param args))
              body)))))

(define env:base (eval env:initial bootstrap-env:base))

(for-each
  (lambda (form)
    (newline)
    (pretty-write form)
    (displayln '=>)
    (flush-output)
    (pretty-write (eval env:base form))
    (flush-output))

  '(
    #t

    (+ 2 8)

    (cons 'a '(b c))

    (if #t 'yes 'no)

    (if #f 'yes 'no)

    ((lambda (x) x) 3)

    ((lambda (lambda) lambda) (+ 1 2))

    ($ (lambda (env args) args) (+ 1 2))

    ((@ lambda '() '() 5))

    ((@ lambda '() '(x) 'x) 6)

    ((lambda (x y) (cons x y)) 11 12)

    ((@ lambda
        ($ (lambda (e) e))
        '(x y)
        '(cons x y))
     11 12)

    (cond (#t 'first)
          (#f 'second)
          (#t 'third))

    (cond (#f 'first)
          (#f 'second)
          (#t 'third))

    (let ((sign (lambda (n) (cond ((< n 0) 'negative)
                                  ((= n 0) 'zero)
                                  (#t      'positive)))))
      (list (list -3 (sign -3))
            (list 10 (sign 10))
            (list  0 (sign  0))))

    (let ((fix (lambda (f)
                 ((lambda (d) (d d))
                  (lambda (x) (f (lambda arg (apply (x x) arg))))))))
      (let ((append
              (fix (lambda (append)
                     (lambda (xs ys)
                       (if (null? xs)
                         ys
                         (cons (car xs) (append (cdr xs) ys))))))))
        (list (append '() '())
              (append '(foo) '(bar))
              (append '(1 2) '(3 4)))))
    ))
