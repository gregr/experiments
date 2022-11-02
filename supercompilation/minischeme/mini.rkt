#lang racket/base
(provide P.mini->E.tiny E.mini->E.tiny)
(require "tiny.rkt" racket/match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mini Scheme Grammars ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Literal: a boolean or a number.
; L ::= #f | #t | <number>

;; Atom: a literal, a null, or a symbol.
; A ::= L | () | <symbol>

;; Value: an atom, singleton vector, pair, or procedure.
; V ::= A | #(V) | (V . V) | #s(closure (<symbol> ...) E ((<symbol> . V) ...))

;; S-expression: an atom, singleton vector, or pair, without any procedures.
; S ::= A | #(S) | (S . S)

;; Lambda expression:
; LAM ::= (lambda (<symbol> ...) E)

;; Expression:
; E ::=
;     ;; variable
;     <symbol>
;     ;; constructors
;     | L
;     | (quote S)
;     | (vector E)
;     | (cons E E)
;     | (list E ...)
;     | (quasiquote QE)
;     | LAM
;     ;; quasi-constructor
;     | (+ E E)
;     ;; accessors
;     | (vector-ref E E)
;     | (car E)
;     | (cdr E)
;     ;; predicates
;     | (eqv? E E)
;     | (null? E)
;     | (vector? E)
;     | (pair? E)
;     | (number? E)
;     | (symbol? E)
;     | (procedure? E)
;     ;; short-circuiting logical operators
;     | (and E ...)
;     | (or E ...)
;     ;; case analysis
;     | (if E E E)
;     | (cond (E E) ... (else E))
;     ;; binding
;     | (let <symbol> ((<symbol> E)   ...) E)
;     | (let          ((<symbol> E)   ...) E)
;     | (let*         ((<symbol> E)   ...) E)
;     | (letrec       ((<symbol> LAM) ...) E)
;     ;; case analysis and binding
;     | (match E (MP E) ...)
;     ;; procedure call
;     | (E E ...)

;; Quasiquote expression:
; QE ::= (quasiquote QE)
;      | (unquote E)
;      | (QE . QE)        ; where the left QE is neither quasiquote or unquote
;      | #(QE)
;      | A                ; where the atom is not equal to quasiquote or unquote

;; Match pattern:
; MP ::= _
;      | <symbol>     ; where the symbol is not equal to _
;      | L
;      | (quote S)
;      | (cons MP MP)
;      | (vector MP)
;      | (list MP ...)
;      | (list* MP ... MP)
;      | (? E          MP ...)
;      | (? null?      MP ...)
;      | (? vector?    MP ...)
;      | (? pair?      MP ...)
;      | (? number?    MP ...)
;      | (? symbol?    MP ...)
;      | (? procedure? MP ...)
;      | (not MP)
;      | (and MP ...)
;      | (or  MP ...)
;      | (quasiquote QP)

;; Quasiquote pattern:
; QP ::= (quasiquote QP)
;      | (unquote MP)
;      | (QP . QP)        ; where the left QP is neither quasiquote or unquote
;      | #(QP)
;      | S                ; where the s-expression is not equal to quasiquote or unquote

;; Top-level definition: a definition of a recursive procedure or a constant.
; D ::= (define (name <symbol> ...) E)
;     | (define name LAM)
;     | (define name L)
;     | (define name (quote S))

;; Program: a list of definitions followed by an expression.
; P ::= D ... E

(define (error x) (`(error: ,x)))

(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))

(define (memv x x*)
  (match x*
    ('()         #f)
    ((cons y y*) (if (eqv? x y)
                     x*
                     (memv x y*)))))

(define (ucons x x*) (if (memv x x*) x* (cons x x*)))

(define (uappend x* y*)
  (match x*
    ('()         y*)
    ((cons x x*) (ucons x (uappend x* y*)))))

(define (parameter*? param*)
  (match param*
    ('()                 #t)
    ((cons param param*) (and (symbol? param)
                              (parameter*? param*)
                              (not (memv param param*))))))

(define (foldl f acc x*)
  (match x*
    ('()         acc)
    ((cons x x*) (foldl f (f x acc) x*))))

(define (foldr f acc x*)
  (match x*
    ('()         acc)
    ((cons x x*) (f x (foldr f acc x*)))))

(define (andmap ? x*)
  (match x*
    ('()         #t)
    ((list x)    (? x))
    ((cons x x*) (and (? x) (andmap ? x*)))))

(define (ormap ? x*)
  (match x*
    ('()         #f)
    ((list x)    (? x))
    ((cons x x*) (or (? x) (ormap ? x*)))))

;; TODO: support first-class primitives.
(define (reverse x*) (foldl cons '() x*))

(define (literal? x) (or (eqv? x #t) (eqv? x #f) (number? x)))
(define (atom?    x) (or (literal? x) (null? x) (symbol? x)))
(define (binding? x) (and (pair? x) (symbol? (car x)) (pair? (cdr x)) (null? (cddr x))))

(define (P.mini->E.tiny P.mini)
  (let ((bound* (foldl (lambda (D?E name*)
                         (let ((name (match D?E
                                       (`(define (,name . ,_) ,_) name)
                                       (`(define ,name        ,_) name)
                                       (_                         #f))))
                           (if name
                               (cond ((not (symbol? name)) (error (list '(definition name must be a symbol) name)))
                                     ((eqv? name 'define)  (error '(cannot define define)))
                                     ((memv name name*)    (error (list '(same name defined multiple times) name)))
                                     (else                 (cons name name*)))
                               name*)))
                       '() P.mini)))
    (match (foldl (lambda (D?E c*p*e)
                    (match c*p*e
                      ((list c* p* e?)
                       (if e?
                           (error (list '(program list must end with exactly one expression) D?E))
                           (match D?E
                             (`(define (,name . ,param*) ,body)
                               (list c* (cons `(,name ,(LAM->E.tiny bound* `(lambda ,param* ,body))) p*) #f))
                             (`(define ,name ,(and `(lambda . ,_) LAM))
                               (list c* (cons `(,name ,(LAM->E.tiny bound* LAM))                     p*) #f))
                             (`(define ,name ,(? literal? L))
                               (list (cons `(,name (quote ,L))             c*) p* #f))
                             (`(define ,name (quote ,S))
                               (list (cons `(,name (quote ,(S->E.tiny S))) c*) p* #f))
                             (`(define ,name ,body)
                               (error (list '(definition must be of a constant or a procedure) D?E)))
                             (E (list c* p* (E.mini->E.tiny bound* E))))))))
                  (list '() '() #f)
                  P.mini)
      ((list c* p* e) (if e
                          (let ((c* (reverse c*)))
                            `(call (lambda ,(map car c*)
                                     (letrec ,(reverse p*) ,e))
                                   . ,(map cadr c*)))
                          (error '(program list must end with an expression)))))))

(define (S->E.tiny S)
  (match S
    ((? atom?)      `(quote S))
    (`#(,S)         `(vector ,(S->E.tiny S)))
    (`(,S.a . ,S.b) `(cons   ,(S->E.tiny S.a)
                             ,(S->E.tiny S.b)))
    (_              (error (list '(invalid S-expression) S)))))

(define (E.mini->E.tiny bound* E)
  (let* ((loop         (lambda (E)    (E.mini->E.tiny bound* E)))
         (bound?       (lambda (name) (memv name bound*)))
         (not-keyword? (lambda (x)    (or (not (symbol? x)) (bound? x)))))
    (match E
      ((? literal?)                          `(quote ,E))
      ((? symbol?)                           (if (bound? E)
                                                 E
                                                 (error (list '(unbound variable) E))))
      ((cons (? not-keyword?) _)             (if (list? E)
                                                 `(call . ,(map loop E))
                                                 (error (list '(invalid call expression) E))))
      (`(quote ,S)                           (S->E.tiny S))
      (`(vector ,E)                          `(vector ,(loop E)))
      (`(cons ,E.a ,E.b)                     `(cons   ,(loop E.a)
                                                      ,(loop E.b)))
      ('(list)                               '(quote ()))
      (`(list ,E.a . ,E*)                    `(cons ,(loop E.a)
                                                    ,(loop `(list . ,E*))))
      (`(quasiquote ,QE)                     (QE->E.tiny bound* QE 0))
      (`(lambda . ,_)                        (LAM->E.tiny bound* E))
      (`(+ ,E.a ,E.b)                        `(+ ,(loop E.a) ,(loop E.b)))
      (`(vector-ref ,E.v ,E.i)               `(vector-ref ,(loop E.v) ,(loop E.i)))
      (`(car ,E)                             `(car ,(loop E)))
      (`(cdr ,E)                             `(cdr ,(loop E)))
      (`(eqv? ,E.a ,E.b)                     `(eqv? ,(loop E.a) ,(loop E.b)))
      (`(null?      ,E)                      `(null?      ,(loop E)))
      (`(vector?    ,E)                      `(vector?    ,(loop E)))
      (`(pair?      ,E)                      `(pair?      ,(loop E)))
      (`(number?    ,E)                      `(number?    ,(loop E)))
      (`(symbol?    ,E)                      `(symbol?    ,(loop E)))
      (`(procedure? ,E)                      `(procedure? ,(loop E)))
      (`(and)                                '(quote #t))
      (`(and ,E)                             (loop E))
      (`(and ,E . ,E*)                       `(if ,(loop E)
                                                  ,(loop `(and . ,E*))
                                                  '#f))
      (`(or)                                 '(quote #f))
      (`(or ,E)                              (loop E))
      (`(or ,E . ,E*)                        `((lambda (temp rest) (if temp temp (call rest)))
                                               ,(loop E)
                                               (lambda () ,(loop `(or . ,E*)))))
      (`(if ,E.c ,E.t ,E.f)                  `(if ,(loop E.c) ,(loop E.t) ,(loop E.f)))
      (`(cond (else ,E))                     (loop E))
      (`(cond (,E.test ,E) . ,clause*)       `(if ,(loop E.test)
                                                  ,(loop E)
                                                  ,(loop `(cond . ,clause*))))
      (`(let ,(? symbol? name) ,binding* ,E) (let ((param* (map car binding*)))
                                               (cond ((not (andmap binding? binding*))
                                                      (error (list '(invalid named-let bindings) binding*)))
                                                     ((not (parameter*? param*))
                                                      (error (list '(invalid named-let parameters) param*)))
                                                     (else
                                                       (let ((bound* (ucons name (uappend param* bound*))))
                                                         `(call (letrec ((,name (lambda ,param*
                                                                                  ,(E.mini->E.tiny bound* E))))
                                                                  ,name)
                                                                . ,(map loop (map cadr binding*))))))))
      (`(let ,binding* ,E)                   (let ((param* (map car binding*)))
                                               (cond ((not (andmap binding? binding*))
                                                      (error (list '(invalid let bindings) binding*)))
                                                     ((not (parameter*? param*))
                                                      (error (list '(invalid let parameters) param*)))
                                                     (else
                                                       `(call (lambda ,param*
                                                                ,(E.mini->E.tiny (uappend param* bound*) E))
                                                              . ,(map loop (map cadr binding*)))))))
      (`(let* ,binding* ,body)               (if (not (andmap binding? binding*))
                                                 (error (list '(invalid let* bindings) binding*))
                                                 (E.mini-let*->E.tiny bound* binding* body)))
      (`(letrec ,binding* ,body)             (let ((param* (map car binding*)))
                                               (cond ((not (andmap binding? binding*))
                                                      (error (list '(invalid letrec bindings) binding*)))
                                                     ((not (parameter*? param*))
                                                      (error (list '(invalid letrec parameters) param*)))
                                                     (else
                                                       `(letrec ,(map (lambda (b)
                                                                        (list (car b) (LAM->E.tiny bound* (cadr b))))
                                                                      binding*)
                                                          ,(E.mini->E.tiny (uappend param* bound*) E))))))
      (`(match ,E . ,clause*)                (E.mini-match->E.tiny bound* E clause*))
      (_                                     (error (list '(invalid expression) E))))))

(define (LAM->E.tiny bound* E)
  (match E
    (`(lambda ,(? parameter*? param*) ,E.body)
      `(lambda ,param* ,(E.mini->E.tiny (uappend param* bound*) E.body)))
    (_ (error (list '(invalid lambda expression) E)))))

(define (E.mini-let*->E.tiny bound* binding* E.body)
  (match binding*
    ('()                                      (E.mini->E.tiny bound* E.body))
    ((cons (list (? symbol? x) E.x) binding*) `(call (lambda (,x)
                                                       ,(E.mini-let*->E.tiny (ucons x bound*) binding* E.body))
                                                     ,(E.mini->E.tiny bound* E.x)))))

(define (QE->E.tiny bound* QE level)
  (match QE
    ('quasiquote           (error '(misplaced quasiquote)))
    ('unquote              (error '(misplaced unquote)))
    ((list 'quasiquote QE) `(cons ,'(quote quasiquote) (cons ,(QE->E.tiny bound* QE (+ level 1)) '())))
    ((list 'unquote X)     (if (eqv? level 0)
                               (E.mini->E.tiny bound* X)
                               `(cons ,'(quote unquote) (cons ,(QE->E.tiny bound* X (+ level -1)) '()))))
    (`(,QE.a . ,QE.b)      `(cons ,(QE->E.tiny bound* QE.a level) ,(QE->E.tiny bound* QE.b level)))
    (`#(,QE)               `(vector ,(QE->E.tiny bound* QE level)))
    ((? atom?)             `(quote ,QE))))

(define (E.mini-match->E.tiny bound* E.x clause*)
  `(call (lambda (x ?+rhs*)
           (call (call (lambda (t) (call t t))
                       (lambda (self)
                         (lambda (?+rhs*)
                           (if (null? ?+rhs*)
                               (call (cons 'error: (cons 'no (cons 'matching (cons 'clause (cons 'for: (cons x '())))))))
                               (call (lambda (sub)
                                       (if sub
                                           (call (cdr (car ?+rhs*)) sub)
                                           (call (call self self) (cdr ?+rhs*))))
                                     (call (car (car ?+rhs*)) x))))))
                 ?+rhs*))
         ,(E.mini->E.tiny bound* E.x)
         ,(foldr (lambda (clause ?+rhs*)
                   (match clause
                     ((list MP E.rhs)
                      (let* ((PP    (MP->PP MP))
                             (b*.PP (PP-bound* PP '())))
                        `(cons (cons ,(PP->? bound* PP)
                                     (call (lambda (rhs)
                                             (lambda (sub)
                                               (call rhs . ,(map (lambda (b)
                                                                   `(call (call (lambda (t) (call t t))
                                                                                (lambda (self)
                                                                                  (lambda (sub)
                                                                                    (if (eqv? ',b (car (car sub)))
                                                                                        (cdr (car sub))
                                                                                        (call (call self self) (cdr sub))))))
                                                                          sub))
                                                                 b*.PP))))
                                           (lambda ,b*.PP ,(E.mini->E.tiny (uappend b*.PP bound*) E.rhs))))
                               ,?+rhs*)))
                     (_ (error (list '(invalid match clause) clause)))))
                 '(quote ())
                 clause*)))

(define (PP->? bound* PP)
  (let* ((bound?       (lambda (name) (memv name bound*)))
         (not-keyword? (lambda (x)    (or (not (symbol? x)) (bound? x)))))
    (match PP
      ('_                             '(lambda (x sub) sub))
      ((? symbol?)                    `(lambda (x sub)
                                         (call (call (lambda (template) (call template template))
                                                     (lambda (self)
                                                       (lambda (sub)
                                                         (if (null? sub)
                                                             (cons (cons ',PP x) '())
                                                             (if (eqv? ',PP (car (car sub)))
                                                                 (if (eqv? x (cdr (car sub)))
                                                                     sub
                                                                     '#f)
                                                                 (call (lambda (sub.rest)
                                                                         (if sub.rest
                                                                             (cons (car sub) sub.rest)
                                                                             '#f))
                                                                       (call (call self self) (cdr sub))))))))
                                               sub)))
      (`(quote ,A)                    `(lambda (x sub) (if (eqv? x (quote ,A)) sub '#f)))
      (`(cons ,PP.a ,PP.b)            `(lambda (x sub)
                                         (if (pair? x)
                                             (call (lambda (sub)
                                                     (if sub
                                                         (call ,(PP->? bound* PP.b) (cdr x) sub)
                                                         '#f))
                                                   (call ,(PP->? bound* PP.a) (car x) sub))
                                             '#f)))
      (`(vector ,PP)                  `(lambda (x sub)
                                         (if (vector? x)
                                             (call ,(PP->? bound* PP) (vector-ref x 0) sub)
                                             '#f)))
      (`(? ,(and (? not-keyword?) E)) `((lambda (?)
                                          (lambda (x sub) (if (call ? x) sub '#f)))
                                        ,(E.mini->E.tiny bound* E)))
      ('(? null?)                     '(lambda (x sub) (if (null?      x) sub '#f)))
      ('(? vector?)                   '(lambda (x sub) (if (vector?    x) sub '#f)))
      ('(? pair?)                     '(lambda (x sub) (if (pair?      x) sub '#f)))
      ('(? number?)                   '(lambda (x sub) (if (number?    x) sub '#f)))
      ('(? symbol?)                   '(lambda (x sub) (if (symbol?    x) sub '#f)))
      ('(? procedure?)                '(lambda (x sub) (if (procedure? x) sub '#f)))
      (`(not ,PP)                     `(lambda (x sub)
                                         (if (call ,(PP->? bound* PP) x sub) '#f sub)))
      (`(and ,PP.a ,PP.b)             `(lambda (x sub)
                                         (call (lambda (sub)
                                                 (if sub
                                                     (call ,(PP->? bound* PP.b) x sub)
                                                     '#f))
                                               (call ,(PP->? bound* PP.a) x sub))))
      ;; TODO: we could extract the intersection PP-bound* from either result sub,
      ;; but doing so is complex.  Instead, we'll just return the original sub.
      (`(or ,PP.a ,PP.b)              `(lambda (x sub)
                                         (if (call ,(PP->? bound* PP.a) x sub)
                                             sub
                                             (if (call ,(PP->? bound* PP.b) x sub)
                                                 sub
                                                 '#f)))))))

(define (PP-bound* PP bound*)
  (match PP
    ('_                  bound*)
    ((? symbol?)         (ucons PP bound*))
    (`(quote ,_)         bound*)
    (`(cons ,PP.a ,PP.b) (PP-bound* PP.b (PP-bound* PP.a bound*)))
    (`(vector ,PP)       (PP-bound* PP bound*))
    (`(? ,_)             bound*)
    (`(not ,_)           bound*)
    (`(and ,PP.a ,PP.b)  (PP-bound* PP.b (PP-bound* PP.a bound*)))
    ;; TODO: we could use this to allow the intersection, but enforcing the
    ;; intersection during matching is complex.
    ;(`(or ,PP.a ,PP.b)   (let ((bound*.a (PP-bound* PP.a '()))
    ;                           (bound*.b (PP-bound* PP.b '())))
    ;                       (foldl (lambda (b.a bound*)
    ;                                (if (memv b.a bound*.b)
    ;                                    (ucons b.a bound*)
    ;                                    bound*))
    ;                              bound*
    ;                              bound*.a)))
    (`(or ,_ ,_) bound*)))

(define (MP->PP MP)
  (match MP
    ((? symbol?)            MP)
    ((? literal?)           `(quote ,MP))
    (`(quote (? atom?))     MP)
    (`(quote #(,S))         `(vector ,(MP->PP `(quote ,S))))
    (`(quote (,S.a . ,S.b)) `(cons   ,(MP->PP `(quote ,S.a))
                                     ,(MP->PP `(quote ,S.b))))
    (`(vector ,MP)          `(vector ,(MP->PP MP)))
    (`(cons ,MP.a ,MP.b)    `(cons ,(MP->PP MP.a)
                                   ,(MP->PP MP.b)))
    ('(list)                '(quote ()))
    (`(list ,MP.a . ,MP*)   `(cons ,(MP->PP MP.a)
                                   ,(MP->PP `(list . ,MP*))))
    (`(list* ,MP)           (MP->PP MP))
    (`(list* ,MP.a . ,MP*)  `(cons ,(MP->PP MP.a)
                                   ,(MP->PP `(list* . ,MP*))))
    (`(? ,E)                MP)
    (`(? ,E . ,MP)          `(and (? ,E) ,(MP->PP `(and . ,MP))))
    (`(not ,MP)             `(not ,(MP->PP MP)))
    ('(and)                 '_)
    (`(and ,MP)             (MP->PP MP))
    (`(and ,MP . ,MP*)      `(and ,(MP->PP MP) ,(MP->PP `(and . ,MP*))))
    ('(or)                  '(not _))
    (`(or ,MP)              (MP->PP MP))
    (`(or ,MP . ,MP*)       `(or ,(MP->PP MP) ,(MP->PP `(or . ,MP*))))
    (`(quasiquote ,QP)      (QP->PP QP 0))
    (_                      (error (list '(invalid match pattern) MP)))))

(define (QP->PP QP level)
  (match QP
    ('quasiquote           (error '(misplaced quasiquote)))
    ('unquote              (error '(misplaced unquote)))
    ((list 'quasiquote QP) `(cons ,'(quote quasiquote) (cons ,(QP->PP QP (+ level 1)) '())))
    ((list 'unquote X)     (if (eqv? level 0)
                               (MP->PP X)
                               `(cons ,'(quote unquote) (cons ,(QP->PP X (+ level -1)) '()))))
    (`(,QP.a . ,QP.b)      `(cons ,(QP->PP QP.a level) ,(QP->PP QP.b level)))
    (`#(,QP)               `(vector ,(QP->PP QP level)))
    ((? atom?)             `(quote ,QP))))
