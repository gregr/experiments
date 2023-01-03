#lang racket/base
(provide (all-defined-out))
(require (prefix-in micro: "micro.rkt") racket/list (except-in racket/match ==))

;;; miniVerse
;;; based on: https://simon.peytonjones.org/assets/pdfs/verse-conf.pdf

(define detect-loops? #t)

(define (run*       E) (run       #f E))
(define (trace-run* E) (trace-run #f E))

(define (run count E)
  (let loop ((count count) (st (base-initial-state E)))
    (define (stop) (micro:state-reify st))
    (cond
      ((eqv? count 0)                    (stop))
      ((not st)                          #f)
      ((micro:value? (micro:state-E st)) (stop))
      (else                              (let ((st.next (micro:state-step st)))
                                           (if (and detect-loops? (equal? st st.next))
                                               (stop)
                                               (loop (and count (- count 1)) st.next)))))))

(define (trace-run count E)
  (let loop ((count count) (st (base-initial-state E)))
    (define (stop) (list (micro:state-reify st)))
    (cond
      ((eqv? count 0)                    (stop))
      ((not st)                          (list #f))
      ((micro:value? (micro:state-E st)) (stop))
      (else                              (let ((st.next (micro:state-step st)))
                                           (if (and detect-loops? (equal? st st.next))
                                               (stop)
                                               (cons (micro:state-reify st)
                                                     (loop (and count (- count 1)) st.next))))))))

(define env.empty (hash))

(define (env-ref env vocab name)
  (let ((vocab=>val (hash-ref env name #f)))
    (and vocab=>val (hash-ref vocab=>val vocab #f))))

(define (env-extend env vocab name* val*)
  (foldl (lambda (name val env) (hash-set env name (hash vocab val))) env name* val*))

(define (env-extend-scope env param* addr*)
  (env-extend env 'expression param* (map parse-variable-ref/address addr*)))

;; NOTE: we may need more uniqueness if we do fancier kinds of parsing.
(define (env-param*->addr* env param*) param*)

(define (ast:quote v)          `(value ,v))
(define (ast:ref   addr)       `(ref ,addr))
(define (ast:==    a b)        `(== ,a ,b))
(define (ast:lam   param body) `(lam ,param ,body))
(define (ast:call  rator rand) `(app ,rator ,rand))
(define (ast:op    name rand*) `(op ,name . ,rand*))
(define (ast:one   body)       `(one ,body))
(define (ast:all   body)       `(all ,body))
(define (ast:alt   a b)        `(alt ,a ,b))
(define (ast:seq   a b)        `(seq ,a ,b))
(define (ast:exist name* body) `(exist ,name* ,body))

(define ($quote v)             (ast:quote v))
(define ($ref   addr)          (ast:ref addr))
(define ($==    a b)           (ast:== a b))
(define ($call  rator . rand*) (ast:call rator (apply $vector rand*)))
(define ($op    name . rand*)  (ast:op name rand*))
(define ($one   e . e*)        (ast:one (apply $begin e e*)))
(define ($all   e . e*)        (ast:all (apply $begin e e*)))
(define ($begin e . e*)        (let loop ((e e) (e* e*))
                                 (cond
                                   ((null? e*) e)
                                   (else       (ast:seq e (loop (car e*) (cdr e*)))))))
(define ($alt . e*)            (if (null? e*)
                                   $fail
                                   (let loop ((e (car e*)) (e* (cdr e*)))
                                     (cond
                                       ((null? e*) e)
                                       (else       (ast:alt e (loop (car e*) (cdr e*))))))))
(define ($vector . e*)         (let loop ((i 0) (e* e*))
                                 (cond
                                   ((null? e*) (ast:all (apply $alt (map $ref (range i)))))
                                   (else       (ast:call (ast:lam i (loop (+ i 1) (cdr e*)))
                                                         (car e*))))))
(define ($vleno v out)         ($op 'vector-lengtho v out))
(define ($vrefo v i out)       ($op 'vector-refo    v i out))
(define ($cons a b)            ($op 'cons a b))

(define $fail    ($== ($quote #t) ($quote #f)))
(define $succeed ($== ($quote #t) ($quote #t)))

(define ($exist env param* ^body)
  (let* ((addr* (env-param*->addr* env param*))
         (env   (env-extend-scope env param* addr*)))
    (ast:exist addr* (apply ^body env addr*))))

(define ($lambda env param* ^body)
  (ast:lam #f ($begin
                ($vleno ($ref #f) ($quote (length param*)))
                ($exist env param*
                        (lambda (env . addr*)
                          (apply $begin
                                 (append (map (lambda (i addr)
                                                ($vrefo ($ref #f) ($quote i) ($ref addr)))
                                              (range (length addr*)) addr*)
                                         (list (apply ^body env addr*)))))))))

(define (literal? stx) (or (boolean? stx) (number? stx) (string? stx)))

(define (parse-expression* env stx*)
  (unless (list? stx*) (error "not a list of expressions" stx*))
  (map (lambda (stx) (parse-expression env stx)) stx*))

(define (parse-expression env stx)
  (cond
    ((pair? stx)    (let* ((rator (car stx))
                           (op    (env-ref env 'expression:operator rator)))
                      (if (procedure? op)
                          (op env stx)
                          (apply $call
                                 (parse-expression env rator)
                                 (parse-expression* env (cdr stx))))))
    ((symbol? stx)  (let* ((op (env-ref env 'expression stx)))
                      (cond
                        ((procedure? op) (op env stx))
                        ((not        op) (error "unbound identifier" stx))
                        (else            (error "invalid use of identifier" stx)))))
    ((literal? stx) (ast:quote stx))
    (else           (error "not an expression" stx))))

(define ((parse-variable-ref/address addr) env e) (ast:ref addr))

(define (parse-quote env v) (ast:quote v))

(define (parse-== env a b) ($== (parse-expression env a) (parse-expression env b)))

(define (parse-begin env . e*) (apply $begin (parse-expression* env e*)))
(define (parse-alt   env . e*) (apply $alt   (parse-expression* env e*)))
(define (parse-one   env . e*) (apply $one   (parse-expression* env e*)))
(define (parse-all   env . e*) (apply $all   (parse-expression* env e*)))

(define (parse-exist env param* . body*)
  ($exist env param* (lambda (env . _) (apply parse-begin env body*))))

(define (parse-lambda env param* . body*)
  ($lambda env param* (lambda (env . _) (apply parse-begin env body*))))

(define (parse-if/exist env param* e.c e.t e.f)
  (ast:call ($one ($alt ($exist env param*
                                (lambda (env . _)
                                  ($begin (parse-expression env e.c)
                                          (ast:lam #f (parse-expression env e.t)))))
                        (ast:lam #f (parse-expression env e.f))))
            ($quote '())))

(define (parse-for/exist env param* e.gen e.body)
  (ast:call
    (ast:call
      (ast:exist
        '(VCONS VHEAD VTAIL VMAP)
        ($begin
          ($== ($ref 'VCONS)
               (ast:lam
                 'x (ast:lam
                      'xs ($all ($alt ($ref 'x)
                                      (ast:exist
                                        '(i out)
                                        ($begin ($vrefo ($ref 'xs) ($ref 'i) ($ref 'out))
                                                ($ref 'out))))))))
          ($== ($ref 'VHEAD)
               (ast:lam
                 'xs (ast:exist
                       '(out) ($begin ($vrefo ($ref 'xs) ($quote 0) ($ref 'out))
                                      ($ref 'out)))))
          ($== ($ref 'VTAIL)
               (ast:lam
                 'xs ($all (ast:exist
                             '(i out) ($begin ($op '< ($quote 0) ($ref 'i))
                                              ($vrefo ($ref 'xs) ($ref 'i) ($ref 'out))
                                              ($ref 'out))))))
          ($==
            ($ref 'VMAP)
            (ast:lam
              'f
              (ast:lam
                'xs
                (ast:call
                  ($one
                    ($alt (ast:exist
                            '(x)
                            ($begin
                              ($== ($ref 'x) (ast:call ($ref 'VHEAD) ($ref 'xs)))
                              (ast:lam
                                #f
                                (ast:call
                                  (ast:call ($ref 'VCONS) (ast:call ($ref 'f) ($ref 'x)))
                                  (ast:call (ast:call ($ref 'VMAP) ($ref 'f))
                                            (ast:call ($ref 'VTAIL) ($ref 'xs)))))))
                          (ast:lam #f ($all $fail))))
                  ($quote '())))))))
      (ast:lam #f (ast:call ($ref #f) ($quote '()))))
    ($all ($exist env param* (lambda (env . _)
                               ($begin (parse-expression env e.gen)
                                       (ast:lam #f (parse-expression env e.body))))))))

(define (parse-if  env . e*) (apply parse-if/exist  env '() e*))
(define (parse-for env . e*) (apply parse-for/exist env '() e*))

(define (parse-top env def*)
  (if (null? def*)
      $succeed
      (let loop ((def* def*) (name* '()) (rhs* '()))
        (match def*
          ('() (let* ((name* (reverse name*))
                      (addr* (env-param*->addr* env.boot.0 name*))
                      (env   (env-extend-scope env name* addr*))
                      (rhs*  (parse-expression* env (reverse rhs*))))
                 (values env addr* (apply $begin (map (lambda (addr rhs) ($== ($ref addr) rhs))
                                                      addr* rhs*)))))
          (`((define ,name ,rhs) . ,def*)
            (loop def* (cons name name*) (cons rhs rhs*)))))))

(define ((expression-operator-parser parser argc.min argc.max) env expr)
  (unless (list? expr) (error "not a list" expr))
  (let* ((e* expr) (argc (- (length e*) 1)))
    (unless (<= argc.min argc)           (error "too few operator arguments"  parser expr))
    (unless (<= argc (or argc.max argc)) (error "too many operator arguments" parser expr))
    (apply parser env (cdr e*))))

(define env.boot.0
  (let* ((spec*
           `((quote     1 1  ,parse-quote)
             (==        2 2  ,parse-==)
             (exist     2 #f ,parse-exist)
             (lambda    2 #f ,parse-lambda)
             (begin     1 #f ,parse-begin)
             (alt       0 #f ,parse-alt)
             (one       1 #f ,parse-one)
             (all       1 #f ,parse-all)
             (if/exist  4 4  ,parse-if/exist)
             (for/exist 3 3  ,parse-for/exist)
             (if        3 3  ,parse-if)
             (for       2 2  ,parse-for)))
         (name*   (map car    spec*))
         (min*    (map cadr   spec*))
         (max*    (map caddr  spec*))
         (parser* (map cadddr spec*))
         (parser* (map expression-operator-parser parser* min* max*)))
    (env-extend env.empty 'expression:operator name* parser*)))

(define prim-def*
  (let ()
    (define (prim1 name)
      `(,name (lam argv (seq (op vector-lengtho (ref argv) (value 1))
                             (exist (a)
                               (seq (op vector-refo (ref argv) (value 0) (ref a))
                                    (op ,name (ref a))))))))

    (define (prim2 name)
      `(,name (lam argv (seq (op vector-lengtho (ref argv) (value 2))
                             (exist (a b)
                               (seq (op vector-refo (ref argv) (value 0) (ref a))
                                    (seq (op vector-refo (ref argv) (value 1) (ref b))
                                         (op ,name (ref a) (ref b)))))))))
    (append '((vector        (lam argv (ref argv)))
              (vector-ref    (lam argv (seq (op vector-lengtho (ref argv) (value 2))
                                            (exist (vec idx out)
                                              (seq (op vector-refo (ref argv) (value 0) (ref vec))
                                                   (seq (op vector-refo (ref argv) (value 1) (ref idx))
                                                        (seq (op vector-refo (ref vec) (ref idx) (ref out))
                                                             (ref out))))))))
              (vector-length (lam argv (seq (op vector-lengtho (ref argv) (value 1))
                                            (exist (vec len)
                                              (seq (op vector-refo (ref argv) (value 0) (ref vec))
                                                   (seq (op vector-lengtho (ref vec) (ref len))
                                                        (ref len)))))))

              (list (lam argv
                         (exist (loop)
                           (seq (== (ref loop)
                                    (lam i (app (one (alt (seq (op vector-lengtho
                                                                   (ref argv) (ref i))
                                                               (lam #f (value ())))
                                                          (lam #f
                                                               (op cons
                                                                   (exist (out)
                                                                     (seq (op vector-refo
                                                                              (ref argv) (ref i) (ref out))
                                                                          (ref out)))
                                                                   (app (ref loop)
                                                                        (exist (out)
                                                                          (seq (op +o (ref i) (value 1) (ref out))
                                                                               (ref out))))))))
                                                (value ()))))
                                (app (ref loop) (value 0))))))
              (+ (lam argv (seq (op vector-lengtho (ref argv) (value 2))
                                (exist (a b out)
                                  (seq (op vector-refo (ref argv) (value 0) (ref a))
                                       (seq (op vector-refo (ref argv) (value 1) (ref b))
                                            (seq (op +o (ref a) (ref b) (ref out))
                                                 (ref out))))))))
              (- (lam argv (seq (op vector-lengtho (ref argv) (value 2))
                                (exist (a b out)
                                  (seq (op vector-refo (ref argv) (value 0) (ref a))
                                       (seq (op vector-refo (ref argv) (value 1) (ref b))
                                            (seq (op +o (ref out) (ref b) (ref a))
                                                 (ref out))))))))
              (* (lam argv (seq (op vector-lengtho (ref argv) (value 2))
                                (exist (a b out)
                                  (seq (op vector-refo (ref argv) (value 0) (ref a))
                                       (seq (op vector-refo (ref argv) (value 1) (ref b))
                                            (seq (op *o (ref a) (ref b) (ref out))
                                                 (ref out))))))))
              (/ (lam argv (seq (op vector-lengtho (ref argv) (value 2))
                                (exist (a b out)
                                  (seq (op vector-refo (ref argv) (value 0) (ref a))
                                       (seq (op vector-refo (ref argv) (value 1) (ref b))
                                            (seq (seq (op number? (ref b))
                                                      (app (one (alt (seq (== (ref b) (value 0))
                                                                          (lam #f (== (value #t) (value #f))))
                                                                     (lam #f (== (value #t) (value #t)))))
                                                           (value '())))
                                                 (seq (app (one (alt (seq (== (ref a) (value 0))
                                                                          (lam #f (== (ref out) (value 0))))
                                                                     (lam #f (== (value #t) (value #t)))))
                                                           (value '()))
                                                      (seq (op *o (ref out) (ref b) (ref a))
                                                           (ref out)))))))))))
            (map prim1 '(number? symbol? string? vector? procedure?))
            (map prim2 '(cons < <=)))))

(define-values (env.boot.1 addr*.boot.1 code.boot.1)
  (let* ((name*      (map car  prim-def*))
         (rhs*       (map cadr prim-def*))
         (addr*      (env-param*->addr* env.boot.0 name*))
         (env.boot.1 (env-extend-scope env.boot.0 name* addr*)))
    (values env.boot.1 addr* (apply $begin (map (lambda (addr rhs) ($== ($ref addr) rhs))
                                                addr* rhs*)))))

(define base-def*
  '((define null?    (lambda (x) (== x '())))
    (define boolean? (lambda (x) (one (alt (== x #t) (== x #f)))))
    (define pair?    (lambda (x) (exist (a b) (== x (cons a b)))))
    (define car      (lambda (x) (exist (a b) (== x (cons a b)) a)))
    (define cdr      (lambda (x) (exist (a b) (== x (cons a b)) b)))
    (define >        (lambda (a b) (< b a)))
    (define >=       (lambda (a b) (<= b a)))

    (define vhead (lambda (xs) (vector-ref xs 0)))

    (define vtail (lambda (xs)
                    (all (exist (i) (> i 0) (vector-ref xs i)))))

    (define vcons (lambda (x xs)
                    (all (alt x (exist (i) (vector-ref xs i))))))

    (define vmap
      (lambda (f xs)
        (if/exist (x) (== x (vhead xs))
                  (vcons (f x) (vmap f (vtail xs)))
                  (== #t #f))))

    ;; TODO: include more common vector functions (from Figure 2)
    ))

(define-values (env.base addr*.base code.base)
  (let-values (((env.base addr*.base.0 code.base.0) (parse-top env.boot.1 base-def*)))
    (values env.base (append addr*.boot.1 addr*.base.0) ($begin code.boot.1 code.base.0))))

(define (base-initial-state E)
  (micro:initial-state micro:env.empty (ast:exist addr*.base ($begin code.base (parse-expression env.base E)))))

;; Optionally include fewer dependencies in initial state:
;(define (base-initial-state E)
;  (micro:initial-state micro:env.empty (ast:exist addr*.boot.1 ($begin code.boot.1 (parse-expression env.boot.1 E)))))

;; Optionally include no dependencies in initial state:
;(define (base-initial-state E)
;  (micro:initial-state micro:env.empty (parse-expression env.boot.0 E)))
