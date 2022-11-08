#lang racket/base
(provide (struct-out closure) E.tiny? E.tiny?!)
(require racket/match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tiny Scheme Grammars ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Atom: a null, boolean, number, or symbol.
; A ::= () | #f | #t | <number> | <symbol>

;; Value: an atom, singleton vector, pair, or procedure (represented as a closure).
; V ::= A | #(V) | (V . V) | #s(closure (<symbol> ...) E ((<symbol> . V) ...))

;; Lambda expression:
; LAM ::= (lambda (<symbol> ...) E)

;; Expression:
; E ::=
;     ;; variable
;     <symbol>
;     ;; constructors
;     | (quote A)
;     | (vector E)
;     | (cons E E)
;     | LAM
;     ;; quasi-constructor
;     | (+ E E)
;     ;; accessors
;     | (vector-ref E E)
;     | (car E)
;     | (cdr E)
;     ;; predicates
;     | (atom=? E E)
;     | (null? E)
;     | (boolean? E)
;     | (vector? E)
;     | (pair? E)
;     | (number? E)
;     | (symbol? E)
;     | (procedure? E)
;     ;; case analysis
;     | (if E E E)
;     ;; procedure call
;     | (call E E ...)
;     ;; recursive procedure binding
;     | (letrec ((<symbol> LAM) ...) E)

(struct closure (param* body env) #:prefab)

(define (ucons x x*) (if (memv x x*) x* (cons x x*)))

(define (uappend x* y*)
  (match x*
    ('()         y*)
    ((cons x x*) (ucons x (uappend x* y*)))))

(define (andmap ? x*)
  (match x*
    ('()         #t)
    ((list x)    (? x))
    ((cons x x*) (and (? x) (andmap ? x*)))))

(define (atom? x) (or (null? x) (boolean? x) (number? x) (symbol? x)))

(define (parameter*? x)
  (let loop ((x x) (name* '()))
    (match x
      ('()        #t)
      ((cons y x) (and (symbol? y)
                       (not (memv y name*))
                       (loop x (cons y name*))))
      (_          #f))))

(define (LAM.tiny? bound* stx)
  (match stx
    (`(lambda ,(? parameter*? param*) ,body)
      (E.tiny? (uappend param* bound*) body))
    (_ #f)))

(define (E.tiny? bound* stx)
  (let* ((loop?  (lambda (stx)  (E.tiny? bound* stx)))
         (lam?   (lambda (stx)  (LAM.tiny? bound* stx)))
         (bound? (lambda (name) (memv name bound*))))
    (match stx
      ((? symbol?)                            (bound? stx))
      (`(quote ,(? atom?))                    #t)
      (`(vector ,(? loop?))                   #t)
      (`(cons ,(? loop?) ,(? loop?))          #t)
      ((? lam?)                               #t)
      (`(+ ,(? loop?) ,(? loop?))             #t)
      (`(vector-ref ,(? loop?) ,(? loop?))    #t)
      (`(car ,(? loop?))                      #t)
      (`(cdr ,(? loop?))                      #t)
      (`(atom=? ,(? loop?) ,(? loop?))        #t)
      (`(null? ,(? loop?))                    #t)
      (`(boolean? ,(? loop?))                 #t)
      (`(vector? ,(? loop?))                  #t)
      (`(pair? ,(? loop?))                    #t)
      (`(number? ,(? loop?))                  #t)
      (`(symbol? ,(? loop?))                  #t)
      (`(procedure? ,(? loop?))               #t)
      (`(if ,(? loop?) ,(? loop?) ,(? loop?)) #t)
      (`(call ,(? loop?) . ,arg*)             (and (list? arg*) (andmap loop? arg*)))
      (`(letrec ,bpair* ,body)                (let loop ((bpair* bpair*) (param* '()) (rhs* '()))
                                                (match bpair*
                                                  ('() (and (lam? `(lambda ,param* ,body))
                                                            (let ((bound* (uappend param* bound*)))
                                                              (andmap (lambda (rhs) (LAM.tiny? bound* rhs)) rhs*))))
                                                  ((cons (list (? symbol? param) rhs) bpair*)
                                                   (loop bpair* (cons param param*) (cons rhs rhs*)))
                                                  (_ #f))))
      (_                                      #f))))

(define (parameter*?! p)
  (let loop ((x p) (name* '()))
    (match x
      ('()        #t)
      ((cons y x) (unless (symbol? y) (error "parameter must be a symbol" y p))
                  (when (memv y name*) (error "duplicate parameter" y p))
                  (loop x (cons y name*)))
      (_          (error "parameters must be a list" p)))))

(define (LAM.tiny?! bound* stx)
  (match stx
    (`(lambda ,param* ,body) (parameter*?! param*)
                             (E.tiny?! (uappend param* bound*) body))
    (_ (error "invalid lambda" stx))))

(define (E.tiny?! bound* stx)
  (let* ((loop?! (lambda (stx)  (E.tiny?! bound* stx)))
         (lam?!  (lambda (stx)  (LAM.tiny?! bound* stx)))
         (bound? (lambda (name) (memv name bound*))))
    (match stx
      ((? symbol?)             (unless (bound? stx) (error "unbound variable" stx)))
      (`(quote ,x)             (unless (atom? x) (error "invalid constant" x stx)))
      (`(vector ,x)            (loop?! x))
      (`(cons ,a ,b)           (loop?! a) (loop?! b))
      (`(lambda . ,_)          (lam?! stx))
      (`(+ ,a ,b)              (loop?! a) (loop?! b))
      (`(vector-ref ,x ,i)     (loop?! x) (loop?! i))
      (`(car ,x)               (loop?! x))
      (`(cdr ,x)               (loop?! x))
      (`(atom=? ,a ,b)         (loop?! a) (loop?! b))
      (`(null? ,x)             (loop?! x))
      (`(boolean? ,x)          (loop?! x))
      (`(vector? ,x)           (loop?! x))
      (`(pair? ,x)             (loop?! x))
      (`(number? ,x)           (loop?! x))
      (`(symbol? ,x)           (loop?! x))
      (`(procedure? ,x)        (loop?! x))
      (`(if ,c ,t ,f)          (loop?! c) (loop?! t) (loop?! f))
      (`(call ,p . ,arg*)      (loop?! p)
                               (unless (list? arg*) (error "arguments are not a list" arg*))
                               (for-each loop?! arg*))
      (`(letrec ,bpair* ,body) (let loop ((bpair* bpair*) (param* '()) (rhs* '()))
                                 (match bpair*
                                   ('()
                                    (lam?! `(lambda ,param* ,body))
                                    (let ((bound* (uappend param* bound*)))
                                      (for-each (lambda (rhs) (LAM.tiny?! bound* rhs)) rhs*)))
                                   ((cons (list param rhs) bpair*)
                                    (unless (symbol? param) (error "invalid letrec parameter" param))
                                    (loop bpair* (cons param param*) (cons rhs rhs*)))
                                   (_ (error "invalid binding-pair list" bpair*)))))
      (_                       (error "unrecognized expression" stx)))))
