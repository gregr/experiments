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

;; Primitive:
; PRIM ::= vector
;        | cons
;        | +
;        | vector-ref
;        | car
;        | cdr
;        | atom=?
;        | null?
;        | boolean?
;        | vector?
;        | pair?
;        | number?
;        | symbol?
;        | procedure?

;; Lambda expression:
; LAM ::= (lambda (<symbol> ...) E)

;; Expression:
; E ::= PRIM
;     ;; variable
;     | <symbol>
;     ;; constructors
;     | L
;     | (quote S)
;     | (quasiquote QE)
;     | (list E ...)
;     | LAM
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
;      | (? E MP ...)
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

(define (not x) (if x #f #t))

(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))

(define (list? x) (or (null? x) (and (pair? x) (list? (cdr x)))))

(define (memv x x*)
  (match x*
    ('()         #f)
    ((cons y y*) (if (atom=? x y)
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

(define (reverse x*) (foldl cons '() x*))

(define (map f x*)
  (let loop ((x* x*))
    (match x*
      ('()         '())
      ((cons x x*) (cons (f x) (loop x*))))))

(define (literal? x) (or (boolean? x) (number? x)))
(define (atom?    x) (or (literal? x) (null? x) (symbol? x)))
(define (binding? x) (and (pair? x) (symbol? (car x)) (pair? (cdr x)) (null? (cddr x))))

(define (E.tiny-lambda? E.tiny)
  (match E.tiny
    (`(lambda . ,_) #t)
    (_              #f)))

(define (E.tiny-constant? E.tiny)
  (match E.tiny
    (`(quote ,_)       #t)
    (`(vector ,E)      (E.tiny-constant? E))
    (`(cons ,E.a ,E.b) (and (E.tiny-constant? E.a) (E.tiny-constant? E.b)))
    (_                 #f)))

(define defstate.empty '(() ()))

(define (defstate-name* dst) (car dst))

(define (defstate-expression dst)
  (match dst
    (`(,_ ((#f ,^E.tiny ,_) . ,_)) ^E.tiny)
    (_                             #f)))

(define (defstate-definition* dst)
  (match dst
    (`(,_ ((#f . ,_) . ,rdef*)) (reverse rdef*))
    (_                          (reverse (cadr dst)))))

(define (defstate-add-expression dst stx ^E.tiny) (defstate-define dst stx #f ^E.tiny))

(define (defstate-define dst stx name ^E.tiny)
  (if (and name (not (symbol? name)))
      (error (list '(definition name must be a symbol) name stx))
      (match dst
        (`(,_ ((#f . ,_) . ,_)) (error (list '(program list must end with exactly one expression) stx)))
        (`(,name* ,def*) (let ((def* (cons (list name ^E.tiny stx) def*)))
                           (cond ((not name)        (list name* def*))
                                 ((memv name name*) (error (list '(same name defined multiple times) name stx)))
                                 (else              (list (cons name name*) def*))))))))

(define (defstate->E.tiny dst)
  (let ((bound* (defstate-name*       dst))
        (^E     (defstate-expression  dst)))
    (if (not ^E)
        (error '(program list must end with an expression))
        (match (foldl (lambda (def c*p*)
                        (match def
                          ((list name ^E stx)
                           (let ((E (^E bound*)))
                             (match c*p*
                               ((list c* p*)
                                (cond ((E.tiny-lambda?   E) (list c* (cons (list name E) p*)))
                                      ((E.tiny-constant? E) (list (cons (list name E) c*) p*))
                                      (else (error (list '(definition must be of a constant or a procedure) stx))))))))))
                      (list '() '())
                      (defstate-definition* dst))
          ((list c* p*)
           (let ((c* (reverse c*)))
             `(call (lambda ,(map car c*)
                      (letrec ,(reverse p*) ,(^E bound*)))
                    . ,(map cadr c*))))))))

(define (P.mini->E.tiny P.mini)
  (defstate->E.tiny
    (foldl (lambda (stx dst)
             (match stx
               (`(define (,name . ,param*) ,body)
                 (defstate-define dst stx name (lambda (bound*) (LAM->E.tiny bound* `(lambda ,param* ,body)))))
               (`(define ,name ,E)
                 (defstate-define dst stx name (lambda (bound*) (E.mini->E.tiny bound* E))))
               (E
                 (defstate-add-expression dst stx (lambda (bound*) (E.mini->E.tiny bound* E))))))
           defstate.empty
           P.mini)))

(define (S->E.tiny S)
  (match S
    ((? atom?)      `(quote ,S))
    (`#(,S)         `(vector ,(S->E.tiny S)))
    (`(,S.a . ,S.b) `(cons   ,(S->E.tiny S.a)
                             ,(S->E.tiny S.b)))
    (_              (error (list '(invalid S-expression) S)))))

(define (prim? x)
  (memv x '(vector cons + vector-ref car cdr atom=? null? boolean? vector? pair? number? symbol? procedure?)))

(define (E.mini->E.tiny bound* E)
  (let* ((loop         (lambda (E)    (E.mini->E.tiny bound* E)))
         (bound?       (lambda (name) (memv name bound*)))
         (not-keyword? (lambda (x)    (or (not (symbol? x)) (prim? x) (bound? x)))))
    (match E
      ((? literal?)                          `(quote ,E))
      ((? symbol?)                           (if (bound? E)
                                                 E
                                                 (match E
                                                   ('vector     '(lambda (x)   (vector x)))
                                                   ('cons       '(lambda (a b) (cons a b)))
                                                   ('+          '(lambda (a b) (+ a b)))
                                                   ('vector-ref '(lambda (v i) (vector-ref v i)))
                                                   ('car        '(lambda (x)   (car x)))
                                                   ('cdr        '(lambda (x)   (cdr x)))
                                                   ('atom=?     '(lambda (a b) (atom=? a b)))
                                                   ('null?      '(lambda (x)   (null? x)))
                                                   ('boolean?   '(lambda (x)   (boolean? x)))
                                                   ('vector?    '(lambda (x)   (vector? x)))
                                                   ('pair?      '(lambda (x)   (pair? x)))
                                                   ('number?    '(lambda (x)   (number? x)))
                                                   ('symbol?    '(lambda (x)   (symbol? x)))
                                                   ('procedure? '(lambda (x)   (procedure? x)))
                                                   (_           (error (list '(unbound variable) E))))))
      ((cons (? not-keyword?) _)             (if (list? E)
                                                 `(call . ,(map loop E))
                                                 (error (list '(invalid call expression) E))))
      (`(quote ,S)                           (S->E.tiny S))
      ('(list)                               '(quote ()))
      (`(list ,E.a . ,E*)                    `(cons ,(loop E.a)
                                                    ,(loop `(list . ,E*))))
      (`(,'quasiquote ,QE)                   (QE->E.tiny bound* QE 0))
      (`(lambda . ,_)                        (LAM->E.tiny bound* E))
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
    ((list 'unquote X)     (if (atom=? level 0)
                               (E.mini->E.tiny bound* X)
                               `(cons ,'(quote unquote) (cons ,(QE->E.tiny bound* X (+ level -1)) '()))))
    (`(,QE.a . ,QE.b)      `(cons ,(QE->E.tiny bound* QE.a level) ,(QE->E.tiny bound* QE.b level)))
    (`#(,QE)               `(vector ,(QE->E.tiny bound* QE level)))
    ((? atom?)             `(quote ,QE))))

(define (E.mini-match->E.tiny bound* E.x clause*)
  `(call (lambda (x ?+rhs*)
           (letrec
             ((self (lambda (?+rhs*)
                      (if (null? ?+rhs*)
                          (call (cons 'error: (cons 'no (cons 'matching (cons 'clause (cons 'for: (cons x '())))))))
                          (call (lambda (sub)
                                  (if sub
                                      (call (cdr (car ?+rhs*)) sub)
                                      (call self (cdr ?+rhs*))))
                                (call (car (car ?+rhs*)) x))))))
             (call self ?+rhs*)))
         ,(E.mini->E.tiny bound* E.x)
         ,(foldr (lambda (clause ?+rhs*)
                   (match clause
                     ((list MP E.rhs)
                      (let* ((PP    (MP->PP MP))
                             (b*.PP (PP-bound* PP '()))
                             (arg*  (map (lambda (b)
                                           `(letrec
                                              ((self (lambda (sub)
                                                       (if (atom=? ',b (car (car sub)))
                                                           (cdr (car sub))
                                                           (call self (cdr sub))))))
                                              (call self sub)))
                                         b*.PP)))
                        `(cons (cons ,(PP->? bound* PP)
                                     (call (lambda (rhs) (lambda (sub) (call rhs . ,arg*)))
                                           ,(LAM->E.tiny bound* `(lambda ,b*.PP ,E.rhs))))
                               ,?+rhs*)))
                     (_ (error (list '(invalid match clause) clause)))))
                 '(quote ())
                 clause*)))

(define (PP->? bound* PP)
  (let* ((bound?       (lambda (name) (memv name bound*)))
         (not-keyword? (lambda (x)    (or (not (symbol? x)) (prim? x) (bound? x)))))
    (match PP
      ('_                             '(lambda (x sub) sub))
      ((? symbol?)                    `(lambda (x sub)
                                         (letrec
                                           ((self (lambda (sub)
                                                    (if (null? sub)
                                                        (cons (cons ',PP x) '())
                                                        (if (atom=? ',PP (car (car sub)))
                                                            (if (atom=? x (cdr (car sub)))
                                                                sub
                                                                '#f)
                                                            (call (lambda (sub.rest)
                                                                    (if sub.rest
                                                                        (cons (car sub) sub.rest)
                                                                        '#f))
                                                                  (call self (cdr sub))))))))
                                           (call self sub))))
      (`(quote ,A)                    `(lambda (x sub) (if (atom=? x (quote ,A)) sub '#f)))
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
    (`(quote ,(? atom?))    MP)
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
    (`(,'quasiquote ,QP)    (QP->PP QP 0))
    (_                      (error (list '(invalid match pattern) MP)))))

(define (QP->PP QP level)
  (match QP
    ('quasiquote           (error '(misplaced quasiquote)))
    ('unquote              (error '(misplaced unquote)))
    ((list 'quasiquote QP) `(cons ,'(quote quasiquote) (cons ,(QP->PP QP (+ level 1)) '())))
    ((list 'unquote X)     (if (atom=? level 0)
                               (MP->PP X)
                               `(cons ,'(quote unquote) (cons ,(QP->PP X (+ level -1)) '()))))
    (`(,QP.a . ,QP.b)      `(cons ,(QP->PP QP.a level) ,(QP->PP QP.b level)))
    (`#(,QP)               `(vector ,(QP->PP QP level)))
    ((? atom?)             `(quote ,QP))))
