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
;        | null?
;        | boolean?
;        | vector?
;        | pair?
;        | number?
;        | symbol?
;        | procedure?
;        | =
;        | symbol=?

;; Lambda expression:
; LAM ::= (lambda (<symbol> ...) DL)

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
;     | (cond (E DL) ... (else DL))
;     ;; binding
;     | (let <symbol> ((<symbol> E)   ...) DL)
;     | (let          ((<symbol> E)   ...) DL)
;     | (let*         ((<symbol> E)   ...) DL)
;     | (letrec       ((<symbol> LAM) ...) DL)
;     ;; case analysis and binding
;     | (match E (MP DL) ...)
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
; D ::= (define (name <symbol> ...) . DL)
;     | (define name LAM)
;     | (define name L)
;     | (define name (quote S))

;; Definition list: a list of definitions followed by an expression.
; DL ::= D ... E

;;;;;;;;;;;;;;;;;
;;; Utilities ;;;
;;;;;;;;;;;;;;;;;

(define (error x) (`(error: ,x)))

(define (not x) (if x #f #t))

(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))

(define (list? x) (or (null? x) (and (pair? x) (list? (cdr x)))))

(define (atom? x) (or (null? x) (boolean? x) (number? x) (symbol? x)))

(define (atom=? a b)
  (cond ((null?    a) (null? b))
        ((boolean? a) (and (boolean? b) (if a b (not b))))
        ((number?  a) (and (number? b) (= a b)))
        ((symbol?  b) (and (symbol? b) (symbol=? a b)))
        (else         #f)))

(define (memv x x*)
  (match x*
    ('()         #f)
    ((cons y y*) (if (atom=? x y)
                     x*
                     (memv x y*)))))

(define (assoc/? ? kv*)
  (let loop ((kv* kv*))
    (match kv*
      ('()                            #f)
      ((cons (and (cons k v) kv) kv*) (if (? k) kv (loop kv*))))))

(define (assoc-symbol key kv*) (assoc/? (lambda (k) (symbol=? k key)) kv*))

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

(define (ucons   x  x*) (if (memv x x*) x* (cons x x*)))
(define (uappend x* y*) (foldr ucons y*  x*))
(define (append  x* y)  (foldr cons  y   x*))
(define (reverse x*)    (foldl cons  '() x*))

(define (map f x*)
  (let loop ((x* x*))
    (match x*
      ('()         '())
      ((cons x x*) (cons (f x) (loop x*))))))

(define (map2 f x* y*)
  (let loop ((x* x*) (y* y*))
    (match (list x* y*)
      ((list '()         '())         '())
      ((list (cons x x*) (cons y y*)) (cons (f x y) (loop x* y*))))))

;;;;;;;;;;;;;;;;;;
;;; MiniScheme ;;;
;;;;;;;;;;;;;;;;;;

(define (literal? x) (or (boolean? x) (number? x)))
(define (binding? x) (and (pair? x) (symbol? (car x)) (pair? (cdr x)) (null? (cddr x))))

(define (parameter*? param*)
  (match param*
    ('()                 #t)
    ((cons param param*) (and (symbol? param)
                              (parameter*? param*)
                              (not (memv param param*))))))

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

(define (env-bind   env name vocab=>x) (cons (cons name vocab=>x) env))
(define (env-bound? env name)          (not (not (assoc-symbol name env))))

(define (env-ref env vocab name)
  (let ((name+vocab=>x (assoc-symbol name env)))
    (and name+vocab=>x
         (let ((vocab+x (assoc-symbol vocab (cdr name+vocab=>x))))
           (and vocab+x (cdr vocab+x))))))

(define env.empty '())

(define (env-extend:primitive env)
  (foldl (lambda (name+code env)
           (match name+code
             ((list name code) (env-bind env name (list (cons 'expression (lambda (env stx) code)))))))
         env
         '((vector     (lambda (x)   (vector x)))
           (cons       (lambda (a b) (cons a b)))
           (+          (lambda (a b) (+ a b)))
           (vector-ref (lambda (v i) (vector-ref v i)))
           (car        (lambda (x)   (car x)))
           (cdr        (lambda (x)   (cdr x)))
           (null?      (lambda (x)   (null? x)))
           (boolean?   (lambda (x)   (boolean? x)))
           (vector?    (lambda (x)   (vector? x)))
           (pair?      (lambda (x)   (pair? x)))
           (number?    (lambda (x)   (number? x)))
           (symbol?    (lambda (x)   (symbol? x)))
           (procedure? (lambda (x)   (procedure? x)))
           (=          (lambda (a b) (=        a b)))
           (symbol=?   (lambda (a b) (symbol=? a b))))))

(define (env-extend:base-definition env)
  (env-bind env 'define (list (cons 'definition:operator parse-define))))

(define (env-extend:base-expression env)
  (foldl (lambda (name+parse env)
           (match name+parse
             ((cons name parse) (env-bind env name (list (cons 'expression:operator parse))))))
         env
         (list (cons 'quote      parse-quote)
               (cons 'quasiquote parse-quasiquote)
               (cons 'list       parse-list)
               (cons 'lambda     parse-lambda)
               (cons 'and        parse-and)
               (cons 'or         parse-or)
               (cons 'if         parse-if)
               (cons 'cond       parse-cond)
               (cons 'let        parse-let)
               (cons 'let*       parse-let*)
               (cons 'letrec     parse-letrec)
               (cons 'match      parse-match))))

(define (env-extend:base env)
  (env-extend:base-expression (env-extend:base-definition (env-extend:primitive env))))

(define (env-bind-variable  env x)  (env-bind env x (list (cons 'expression parse-variable))))
(define (env-bind-variable* env x*) (foldl (lambda (x env) (env-bind-variable env x)) env x*))

(define (parse-mini-scm-program stx*) (parse-definition*-expression (env-extend:base env.empty) stx*))

;;;;;;;;;;;;;;;;;;;
;;; Definitions ;;;
;;;;;;;;;;;;;;;;;;;

(define (env->defstate env) `(,env () ()))

(define (defstate-env   dst) (car dst))
(define (defstate-name* dst) (cadr dst))

(define (defstate-expression dst)
  (match dst
    (`(,_ ,_ ((#f ,^E.tiny ,_) . ,_)) ^E.tiny)
    (_                                #f)))

(define (defstate-definition* dst)
  (match dst
    (`(,_ ,_ ((#f . ,_) . ,rdef*)) (reverse rdef*))
    (_                             (reverse (cadr dst)))))

(define (defstate-add-expression dst stx ^E.tiny) (defstate-define dst stx #f ^E.tiny))

(define (defstate-define dst stx name ^E.tiny)
  (if (and name (not (symbol? name)))
      (error (list '(definition name must be a symbol) name stx))
      (match dst
        (`(,_ ,_ ((#f . ,_) . ,_))
          (error (list '(definition list must end with exactly one expression) stx)))
        (`(,env ,name* ,def*)
          (let ((def* (cons (list name ^E.tiny stx) def*)))
            (cond ((not name)        (list env name* def*))
                  ((memv name name*) (error (list '(same name defined multiple times) name stx)))
                  (else              (list (env-bind-variable env name) (cons name name*) def*))))))))

(define (parse-definition dst stx)
  (define (default dst stx)
    (defstate-add-expression dst stx (lambda (env) (parse-expression env stx))))
  ((match stx
     ((cons (? symbol? operator) _) (or (env-ref (defstate-env dst) 'definition:operator operator)
                                        default))
     (_                             default))
   dst stx))

(define (parse-define dst stx)
  (match stx
    (`(,_ (,name . ,param*) . ,body)
      (defstate-define dst stx name (lambda (env) (parse-lambda env `(lambda ,param* . ,body)))))
    (`(,_ ,name ,E)
      (defstate-define dst stx name (lambda (env) (parse-expression env E))))
    (_ (error (list '(invalid define) stx)))))

(define (parse-definition*-expression env stx)
  (if (list? stx)
      (let* ((dst (foldl (lambda (stx dst) (parse-definition dst stx)) (env->defstate env) stx))
             (env (defstate-env        dst))
             (^E  (defstate-expression dst)))
        (if ^E
            (match (foldl (lambda (def c*p*)
                            (match def
                              ((list name ^E stx)
                               (let ((E (^E env)))
                                 (match c*p*
                                   ((list c* p*)
                                    (cond ((E.tiny-lambda?   E) (list c* (cons (list name E) p*)))
                                          ((E.tiny-constant? E) (list (cons (list name E) c*) p*))
                                          (else (error (list '(definition can only bind a constant or a procedure)
                                                             stx))))))))))
                          (list '() '())
                          (defstate-definition* dst))
              ((list c* p*)
               (let ((c* (reverse c*)))
                 `(call (lambda ,(map car c*)
                          (letrec ,(reverse p*) ,(^E env)))
                        . ,(map cadr c*)))))
            (error (list '(definition list must end with an expression) stx))))
      (error (list '(definition*-expression sequence must be a list) stx))))

;;;;;;;;;;;;;;;;;;;
;;; Expressions ;;;
;;;;;;;;;;;;;;;;;;;

(define (parse-expression env stx)
  (match stx
    ((? literal?)      (parse-quote env `(quote ,stx)))
    ((? symbol?)       (let ((parse (env-ref env 'expression stx)))
                         (if parse
                             (parse env stx)
                             (error (list '(unbound variable) stx)))))
    ((cons operator _) ((or (and (symbol? operator) (env-ref env 'expression:operator operator))
                            parse-call)
                        env stx))
    (_                 (error (list '(invalid expression) stx)))))

(define (parse-expression* env stx*) (map (lambda (e) (parse-expression env e)) stx*))

(define (parse-variable env stx)
  (if (symbol? stx)
      stx
      (error (list '(variable must be a symbol) stx))))

(define (parse-call env stx)
  (if (and (pair? stx) (list? stx))
      `(call . ,(parse-expression* env stx))
      (error (list '(invalid call expression) stx))))

(define ($quote S)
  (let loop ((S S))
    (match S
      ((? atom?)      `(quote ,S))
      (`#(,S)         `(vector ,($quote S)))
      (`(,S.a . ,S.b) `(cons ,($quote S.a) ,($quote S.b)))
      (_              (error (list '(invalid S-expression) S))))))

(define (parse-quote env stx)
  (match stx
    ((list _ S) ($quote S))
    (_          (error (list '(invalid quote expression) stx)))))

(define (parse-quasiquote env stx)
  (match stx
    ((list _ QE)
     (let loop ((QE QE) (level 0))
       (match QE
         ('quasiquote           (error (list '(misplaced quasiquote) stx)))
         ('unquote              (error (list '(misplaced unquote) stx)))
         ((list 'quasiquote QE) `(cons ,($quote 'quasiquote) (cons ,(loop QE (+ level 1)) '())))
         ((list 'unquote X)     (if (= level 0)
                                    (parse-expression env X)
                                    `(cons ,($quote 'unquote) (cons ,(loop X (+ level -1)) '()))))
         (`(,QE.a . ,QE.b)      `(cons ,(loop QE.a level) ,(loop QE.b level)))
         (`#(,QE)               `(vector ,(loop QE level)))
         ((? atom?)             ($quote QE)))))
    (_ (error (list '(invalid quasiquote expression) stx)))))

(define (parse-list env stx)
  (match stx
    ((cons _ (? list? arg*))
     (foldr (lambda (arg rest) `(cons ,arg ,rest))
            ($quote '())
            (parse-expression* env arg*)))
    (_ (error (list '(invalid list expression) stx)))))

(define ($lambda env param* ^body) `(lambda ,param* ,(^body (env-bind-variable* env param*))))

(define (parse-lambda env stx)
  (match stx
    (`(,_ ,param* . ,body)
      (if (and (list? param*) (parameter*? param*))
          ($lambda env param* (lambda (env) (parse-definition*-expression env body)))
          (error (list '(invalid lambda parameters) param* stx))))
    (_ (error (list '(invalid lambda expression) stx)))))

(define (parse-and env stx)
  (match stx
    (`(,_)              ($quote #t))
    (`(,_ ,arg . ,arg*) (let loop ((arg arg) (arg* arg*))
                          (let ((E (parse-expression env arg)))
                            (match arg*
                              ('()             E)
                              ((cons arg arg*) `(if ,E ,(loop arg arg*) '#f))))))
    (_                  (error (list '(invalid and expression) stx)))))

(define (parse-or env stx)
  (match stx
    (`(,_)              ($quote #f))
    (`(,_ ,arg . ,arg*) (let loop ((arg arg) (arg* arg*))
                          (let ((E (parse-expression env arg)))
                            (match arg*
                              ('()             E)
                              ((cons arg arg*) `(call (lambda (temp rest) (if temp temp (call rest)))
                                                      ,E (lambda () ,(loop arg arg*))))))))
    (_                  (error (list '(invalid or expression) stx)))))

(define (parse-if env stx)
  (define (loop x) (parse-expression env x))
  (match stx
    (`(,_ ,c ,t ,f) `(if ,(loop c) ,(loop t) ,(loop f)))
    (_              (error (list '(invalid if expression) stx)))))

(define (parse-cond env stx)
  (define (keyword? x) (and (symbol? x) (not (env-bound? env x))))
  (match stx
    (`(,_ (,(? keyword? 'else) . ,stx*)) (parse-definition*-expression env stx*))
    (`(,_ (,test . ,stx*) . ,clause*)    `(if ,(parse-expression env test)
                                              ,(parse-definition*-expression env stx*)
                                              ,(parse-cond env `(cond . ,clause*))))
    (_                                   (error (list '(invalid cond expression) stx)))))

(define (parse-let env stx)
  (match stx
    (`(,_ ,(? symbol? name) ,(? list? b*) . ,body)
      (let ((param* (map car b*)))
        (cond ((not (andmap binding? b*))
               (error (list '(invalid named-let bindings) b* stx)))
              ((not (parameter*? param*))
               (error (list '(invalid named-let parameters) param* stx)))
              (else `(call ,($letrec env
                                     (list name)
                                     (list (lambda (env) (parse-lambda env `(lambda ,param* . ,body))))
                                     (lambda (env) (parse-variable env name)))
                           . ,(parse-expression* env (map cadr b*)))))))
    (`(,_ ,(? list? b*) . ,body)
      (let ((param* (map car b*)))
        (cond ((not (andmap binding? b*))
               (error (list '(invalid let bindings) b*)))
              ((not (parameter*? param*))
               (error (list '(invalid let parameters) param*)))
              (else `(call ,(parse-lambda env `(lambda ,param* . ,body))
                           . ,(map (lambda (e) (parse-expression env e)) (map cadr b*)))))))
    (_ (error (list '(invalid let expression) stx)))))

(define (parse-let* env stx)
  (match stx
    (`(,_ ,(? list? b*) . ,body)
      (if (not (andmap binding? b*))
          (error (list '(invalid let* bindings) b*))
          (let loop ((env env) (b* b*))
            (match b*
              ('()                                (parse-definition*-expression env body))
              ((cons (list (? symbol? x) E.x) b*) `(call ,($lambda env (list x) (lambda (env) (loop env b*)))
                                                         ,(parse-expression env E.x)))))))
    (_ (error (list '(invalid let* expression) stx)))))

(define ($letrec env param* ^rhs* ^body)
  (let ((env (env-bind-variable* env param*)))
    `(letrec ,(map2 (lambda (param ^rhs)
                      (let ((rhs (^rhs env)))
                        (if (E.tiny-lambda? rhs)
                            (list param rhs)
                            (error (list '(letrec can only bind procedures) param rhs)))))
                    param* ^rhs*)
       ,(^body env))))

(define (parse-letrec env stx)
  (match stx
    (`(,_ ,(? list? b*) . ,body)
      (let ((param* (map car b*)))
        (cond ((not (andmap binding? b*))
               (error (list '(invalid letrec bindings) b*)))
              ((not (parameter*? param*))
               (error (list '(invalid letrec parameters) param*)))
              (else ($letrec env param* (map (lambda (rhs) (lambda (env) (parse-expression env rhs)))
                                             (map cadr b*))
                             (lambda (env) (parse-definition*-expression env body)))))))
    (_ (error (list '(invalid letrec expression) stx)))))

(define (parse-match env stx)
  (match stx
    (`(,_ ,x . ,clause*)
      `(call (lambda (x ?+rhs*)
               (letrec
                 ((self (lambda (?+rhs*)
                          (if (null? ?+rhs*)
                              (call (cons 'error: (cons 'no (cons 'matching (cons 'clause (cons 'for: (cons x '())))))))
                              (call (lambda (sub)
                                      (if sub
                                          (call (cdr (car ?+rhs*)) sub)
                                          (call self (cdr ?+rhs*))))
                                    (call (car (car ?+rhs*)) x '()))))))
                 (call self ?+rhs*)))
             ,(parse-expression env x)
             ,(foldr (lambda (clause ?+rhs*)
                       (match clause
                         ((cons MP stx*.rhs)
                          (let* ((PP    (MP->PP MP))
                                 (b*.PP (PP-bound* PP '()))
                                 (arg*  (map (lambda (b)
                                               `(letrec
                                                  ((self (lambda (sub)
                                                           (if (symbol=? ',b (car (car sub)))
                                                               (cdr (car sub))
                                                               (call self (cdr sub))))))
                                                  (call self sub)))
                                             b*.PP)))
                            `(cons (cons ,(PP->? env PP)
                                         (call (lambda (rhs) (lambda (sub) (call rhs . ,arg*)))
                                               ,(parse-lambda env `(lambda ,b*.PP . ,stx*.rhs))))
                                   ,?+rhs*)))
                         (_ (error (list '(invalid match clause) clause)))))
                     '(quote ())
                     clause*)))
    (_ (error (list '(invalid match expression) stx)))))

(define (PP->? env PP)
  (define equal?.tiny
    `(letrec ((equal? (lambda (a b)
                        (if (null? a) (null? b)
                            (if (boolean? a) (if (boolean? b) (if a b (if b '#f '#t)) '#f)
                                (if (number? a) (if (number? b) (= a b) '#f)
                                    (if (symbol? a) (if (symbol? b) (symbol=? a b) '#f)
                                        (if (pair? a)
                                            (if (pair? b)
                                                (if (call equal? (car a) (car b))
                                                    (call equal? (cdr a) (cdr b))
                                                    '#f)
                                                '#f)
                                            (if (vector? a)
                                                (if (vector? b)
                                                    (call equal? (vector-ref a '0) (vector-ref b '0))
                                                    '#f)
                                                '#f)))))))))
       equal?))
  (let loop ((PP PP))
    (match PP
      ('_                  '(lambda (x sub) sub))
      ((? symbol?)         `(lambda (x sub)
                              (letrec
                                ((self (lambda (sub)
                                         (if (null? sub)
                                             (cons (cons ',PP x) '())
                                             (if (symbol=? ',PP (car (car sub)))
                                                 (if (call ,equal?.tiny x (cdr (car sub)))
                                                     sub
                                                     '#f)
                                                 (call (lambda (sub.rest)
                                                         (if sub.rest
                                                             (cons (car sub) sub.rest)
                                                             '#f))
                                                       (call self (cdr sub))))))))
                                (call self sub))))
      (`(quote ,A)         `(lambda (x sub) (if (call ,equal?.tiny x (quote ,A)) sub '#f)))
      (`(cons ,PP.a ,PP.b) `(lambda (x sub)
                              (if (pair? x)
                                  (call (lambda (sub)
                                          (if sub
                                              (call ,(loop PP.b) (cdr x) sub)
                                              '#f))
                                        (call ,(loop PP.a) (car x) sub))
                                  '#f)))
      (`(vector ,PP)       `(lambda (x sub)
                              (if (vector? x)
                                  (call ,(loop PP) (vector-ref x '0) sub)
                                  '#f)))
      (`(? ,E)             `(call (lambda (?)
                                    (lambda (x sub) (if (call ? x) sub '#f)))
                                  ,(parse-expression env E)))
      (`(not ,PP)          `(lambda (x sub)
                              (if (call ,(loop PP) x sub) '#f sub)))
      (`(and ,PP.a ,PP.b)  `(lambda (x sub)
                              (call (lambda (sub)
                                      (if sub
                                          (call ,(loop PP.b) x sub)
                                          '#f))
                                    (call ,(loop PP.a) x sub))))
      ;; TODO: we could extract the intersection PP-bound* from either result sub,
      ;; but doing so is complex.  Instead, we'll just return the original sub.
      (`(or ,PP.a ,PP.b)   `(lambda (x sub)
                              (if (call ,(loop PP.a) x sub)
                                  sub
                                  (if (call ,(loop PP.b) x sub)
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
    ((list 'unquote X)     (if (= level 0)
                               (MP->PP X)
                               `(cons ,'(quote unquote) (cons ,(QP->PP X (+ level -1)) '()))))
    (`(,QP.a . ,QP.b)      `(cons ,(QP->PP QP.a level) ,(QP->PP QP.b level)))
    (`#(,QP)               `(vector ,(QP->PP QP level)))
    ((? atom?)             `(quote ,QP))))
