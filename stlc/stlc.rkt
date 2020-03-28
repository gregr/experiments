#lang racket

(define-syntax let/list
  (syntax-rules ()
    ((_ loop (list-expr bindings ...) ((phd . ptl) cons-case ...) nil-case ...)
     (let loop ((list-name list-expr) bindings ...)
       (match list-name
         (`(,phd . ,ptl) cons-case ...)
         ('() nil-case ...))))))

(define env-empty '())
(define (env-lookup-default env name default)
  (match env
    (`((,n ,val) . ,env)
      (if (eq? n name)
        val
        (env-lookup-default env name default)))
    ('() (default))))
(define (env-lookup env name) (env-lookup-default env name (lambda () name)))
(define (env-lookup-fail env name)
  (env-lookup-default
    env name (lambda () (error (format "unbound variable: ~a" name)))))
(define (env-extend env name val) `((,name ,val) . ,env))

(define (eval-term env term)
  (match term
    (`(,term : ,type) (eval-term env term))
    (`(lambda ,name ,body) `(closure ,name ,body ,env))
    (`(,fn ,arg)
      (match (eval-term env fn)
        (`(closure ,name ,body ,cenv)
          (eval-term (env-extend cenv name (eval-term env arg)) body))
        (vfn `(,vfn ,(eval-term env arg)))))
    (_ (env-lookup env term))))

(define (normalize-term env term)
  (match (eval-term env term)
    (`(closure ,name ,body ,cenv)
      (let ((name1 (gensym name)))
        `(lambda ,name1 ,(normalize-term (env-extend cenv name name1) body))))
    (`(,fn ,arg) `(,(normalize-term env fn) ,(normalize-term env arg)))
    (term term)))

(define (denote-term term)
  (match term
    (`(closure ,name ,body ,cenv)
      (let ((clo ((denote-term `(lambda ,name ,body))
                  (map (lambda (binding)
                         (list (car binding) (list #f (cadr binding))))
                       cenv))))
        (lambda (env) clo)))
    (`(,term : ,type) (denote-term term))
    (`(lambda ,name ,body)
      (let ((dbody (denote-term body)))
        (lambda (env)
          (lambda (arg) (dbody (env-extend env name (list #t arg)))))))
    (`(,fn ,arg)
      (let ((dfn (denote-term fn)) (darg (denote-term arg)))
        (lambda (env) ((dfn env) (darg env)))))
    (_ (lambda (env)
         (match (env-lookup-default env term (lambda () (list #t term)))
           (`(#t ,val) val)
           (`(#f ,term) ((denote-term term) env)))))))

(define (eval term) (eval-term env-empty term))
(define (normalize term) (normalize-term env-empty term))
(define (denote term) ((denote-term term) env-empty))

(define (parse-type type-stx)
  (match type-stx
    (`(,ty0) (parse-type ty0))
    (`(,ty0 ->) (error (format "incomplete -> type: ~a" type-stx)))
    (`(,ty0 -> . ,rest) `(,(parse-type ty0) -> ,(parse-type rest)))
    (_ type-stx)))

(define (parse penv stx)
  (define (free-name? name) (not (env-lookup-default penv name (lambda () #f))))
  (define (special? name) (procedure? (env-lookup penv name)))
  (match stx
    (`(,term ,(? free-name? ':) . ,type)
      `(,(parse penv term) : ,(parse-type type)))
    (`(,(? free-name? 'lambda) ,params ,body)
      (match params
        (`(,name)          (parse penv `(lambda ,name ,body)))
        (`(,name . ,names) (parse penv `(lambda ,name (lambda ,names ,body))))
        (name              `(lambda ,name
                              ,(parse (env-extend penv name name) body)))))
    (`(,(? symbol? (? special? fn)) . ,args)
      (parse penv (apply (env-lookup penv fn) args)))
    (`(,fn . ,args)
      (let/list loop (args (pfn (parse penv fn)))
        ((arg . args) (loop args `(,pfn ,(parse penv arg))))
        pfn))
    (_ (env-lookup penv stx))))

(define-syntax env-extend-params
  (syntax-rules ()
    ((_ env param params ...)
     (env-extend-params (env-extend env 'param param) params ...))
    ((_ env) env)))
(define-syntax lc-module-k
  (syntax-rules (=)
    ((_ penv (bindings ...) (clauses ...) (data ...) (name params ...) = rhs rest ...)
     (lc-module-k penv
                  (bindings ...
                   (name (lambda (params ...)
                           (parse (env-extend-params penv params ...) 'rhs)))
                   (penv (env-extend penv 'name name)))
                  (clauses ... ('name name))
                  (data ... (list 'name name))
                  rest ...))
    ((_ penv (bindings ...) (clauses ...) (data ...) name = rhs rest ...)
     (lc-module-k penv
                  (bindings ...
                   (name (parse penv 'rhs))
                   (penv (env-extend penv 'name name)))
                  (clauses ... ('name name))
                  (data ... (list 'name name))
                  rest ...))
    ((_ penv bindings (clauses ...) (data ...))
     (let* bindings (lambda args
                      (match args
                        ('() (list data ...))
                        (`(,name) (match name clauses ...))))))))
(define-syntax lc-module
  (syntax-rules ()
    ((_ body ...) (let ((penv env-empty))
                    (lc-module-k penv () () () body ...)))))

(define example
  (lc-module
    true  = (lambda (t f) t)
    false = (lambda (t f) f)

    (if condition true-alt false-alt) =
    ((condition (lambda _ true-alt) (lambda _ false-alt)) (lambda _ _))

    n0   = (lambda (f x) x)
    succ = (lambda (n f x) (f (n f x)))
    (exp a b) = (b a)

    n1   = (succ n0)
    n2   = (succ n1)
    n3   = (succ n2)
    n8   = (exp n2 n3)
    n256 = (exp n2 n8)

    add = (lambda (a b f x) (a f (b f x)))
    mul = (lambda (a b f) (a (b f)))

    nil   = (lambda (c n) n)
    cons  = (lambda (h t c n) (c h (t c n)))
    foldr = (lambda (c n xs) (xs c n))

    r0 = ((cons n0 nil) : (((a -> a) -> a -> a) -> (b -> b)) -> (b -> b))
    ;r0 = (cons n0 nil)
    r1 = (cons n1 r0)
    r2 = (cons n2 r1)

    pair  = (lambda (l r f) (f l r))
    left  = (lambda p (p true))
    right = (lambda p (p false))

    ;range-descending = (lambda n
                         ;(n (lambda xs
                              ;(xs (lambda (h t) (cons (succ h) xs))
                                  ;(cons n0 nil)))
                            ;nil))

    range-descending = (lambda n
                         (right (n (lambda (current)
                                     (pair (succ (left current))
                                           (cons (left current) (right current))))
                                   (pair n0 nil))))

    sum = (foldr add n0)
    n11 = (succ (succ (succ n8)))
    n55 = (sum (range-descending n11))

    n4  = (succ n3)
    r4  = (range-descending n4)
    n6  = (sum r4)
    r6  = (range-descending n6)
    n15 = (sum r6)
    r11 = (range-descending n11)

    ra4 = (cons n0 (cons n1 (cons n2 (cons n3 (cons n4 nil)))))
    n10 = (sum ra4)

    filter = (lambda p?
               (foldr (lambda (h t)
                        (if (p? h) (cons h t) t))
                      nil))
    positive? = (lambda n (n (lambda x true) false))
    positives = (filter positive?)
    n1_and_n3 = (positives (cons n0 (cons n1 (cons n0 (cons n3 (cons n0 nil))))))
    ))

(define (closure->boolean clo) ((clo #t) #f))
(define (closure->nat clo) ((clo add1) 0))
(define (closure->pair left right clo)
  (clo (lambda (h) (lambda (t) (cons (left h) (right t))))))
(define (closure->list element clo)
  (map element ((clo (lambda (h) (lambda (t) (cons h t)))) '())))

(define (denote-boolean term) (closure->boolean (denote term)))
(define (denote-nat term) (closure->nat (denote term)))
(define (denote-pair left right term)
  (closure->pair left right (denote term)))
(define (denote-list element term) (closure->list element (denote-term)))

(define var-index 0)
(define (var name)
  (let ((vr (vector var-index)))
    (set! var-index (+ 1 var-index))
    vr))
(define var? vector?)

(define st-empty '())
(define (st-assign st vr val) (cons (list vr val) st))
(define (st-lookup st v0)
  (match st
    (`((,v1 ,val) . ,st) (if (eq? v0 v1) val (st-lookup st v0)))
    (_ v0)))
(define (walk st tm)
  (if (var? tm)
    (let ((tm1 (st-lookup st tm)))
      (if (eq? tm tm1) tm1 (walk st tm1)))
    tm))
(define (walk* st tm)
  (match (walk st tm)
    (`(,in -> ,out) `(,(walk* st in) -> ,(walk* st out)))
    (val val)))

;(define (does-not-occur? st vr tm) st) ;; Use this to see some recursive types
(define (does-not-occur? st vr tm)
  (match tm
    (`(,t0 -> ,t1)
      (and (does-not-occur? st vr (walk st t0))
           (does-not-occur? st vr (walk st t1))))
    (_ (and (not (eq? vr tm)) st))))
(define (unify st t0 t1)
  (let ((t0 (walk st t0))
        (t1 (walk st t1)))
    (cond
      ((eq? t0 t1) st)
      ((var? t0) (and (does-not-occur? st t0 t1) (st-assign st t0 t1)))
      ((var? t1) (and (does-not-occur? st t1 t0) (st-assign st t1 t0)))
      (else
        (match (list t0 t1)
          (`((,a0 -> ,b0) (,a1 -> ,b1))
            (let ((st (unify st a0 a1)))
              (and st (unify st b0 b1))))
          (_ #f))))))

(define (succeed st) st)
(define (fail st) #f)
(define (== t0 t1) (lambda (st) (unify st t0 t1)))

(define-syntax conj*
  (syntax-rules ()
    ((_) succeed)
    ((_ g0) g0)
    ((_ g0 gs ...)
     (lambda (st) (let ((st (g0 st))) (and st ((conj* gs ...) st)))))))
(define-syntax let/vars
  (syntax-rules ()
    ((_ (lvar ...) body ...) (let ((lvar (var 'lvar)) ...) body ...))))
(define-syntax fresh
  (syntax-rules ()
    ((_ lvars body ...) (let/vars lvars (conj* body ...)))))

(define (type-term env term result)
  (define (has-type env term type)
    (match term
      (`(lambda ,name ,body)
        (fresh (in out)
          (== `(,in -> ,out) type)
          (has-type (env-extend env name in) body out)))
      (_ (type-term env term type))))
  (match term
    (`(,term : ,type)
      (conj* (== result type) (has-type env term type)))
    (`(lambda ,name ,body) (has-type env term result))
    (`(,fn ,arg)
      (fresh (ftype in)
        (== `(,in -> ,result) ftype)
        (type-term env fn ftype)
        (has-type env arg in)))
    (_ (== result (env-lookup env term)))))

(define (type term)
  (set! var-index 0)
  (let/vars (result)
    (let ((st ((type-term env-empty term result) st-empty)))
      (and st (walk* st result)))))
