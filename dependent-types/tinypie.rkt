;;; This is a variation of David Christiansen's Normalization by Evaluation:
;;;   https://davidchristiansen.dk/tutorials/nbe/
;;; This version includes let bindings and a tower of universes.

;; TODO: implement some form of inference to make it less tedious
;; to use derived operators like replace, symm, and cong.

#lang racket/base
(provide (all-defined-out))
(require racket/match racket/pretty)

(define (pretty-error message . e*)
  (displayln "================================================================\nERROR:")
  (display message)
  (displayln ":")
  (for-each pretty-write e*)
  (displayln "================================================================")
  (apply error message e*))

(define (map* f x**)
  (let loop ((x** x**))
    (if (null? x**)
        '()
        (cons (apply f (car x**)) (loop (cdr x**))))))

(define (foldl* f acc x**)
  (let loop ((x** x**) (acc acc))
    (if (null? x**)
        acc
        (loop (cdr x**) (apply f acc (car x**))))))

(define (nat? x) (and (integer? x) (exact? x) (<= 0 x)))

(define (E:the    t e)      `#(E:the    ,t ,e))
(define (E:var    x)        `#(E:var    ,x))
(define (E:U      n)        `#(E:U      ,n))
(define (E:Pi     x A B)    `#(E:Pi     ,x ,A ,B))
(define (E:Sigma  x A B)    `#(E:Sigma  ,x ,A ,B))
(define (E:lam    x body)   `#(E:lam    ,x ,body))
(define (E:cons   a b)      `#(E:cons   ,a ,b))
(define (E:symbol s)        `#(E:symbol ,s))
(define (E:->     A B)      (E:Pi #f A B))
(define (E:let x rhs body)  `#(E:let ,x ,rhs ,body))
(define (E:construct id a*) `#(E:construct ,id ,a*))
(define E.Trivial           (E:construct 'Trivial '()))
(define (E:= X from to)     (E:construct '= (list X from to)))
(define E.sole              (E:construct 'sole '()))
(define (E:boolean b)       (error "booleans are not implemented" b))
(define E.zero              (E:construct 'zero '()))
(define (E:add1    x)       (E:construct 'add1 (list x)))
(define (E:eliminate id a*) `#(E:eliminate ,id ,a*))
(define (E:apply     f a)   (E:eliminate 'apply (list f a)))

(define (annotated   t x)   `#(annotated ,t ,x))
(define (neutral     t x)   `#(neutral   ,t ,x))
(define (V:the       t x)   `#(V:the     ,t ,x))
(define (V:var       x)     `#(V:var     ,x))
(define (V:U         n)     `#(V:U       ,n))
(define (V:Pi        A B)   `#(V:Pi      ,A ,B))
(define (V:Sigma     A B)   `#(V:Sigma   ,A ,B))
(define (V:lam       c)     `#(V:lam     ,c))
(define (V:cons      a b)   `#(V:cons    ,a ,b))
(define (V:symbol    s)     `#(V:symbol  ,s))
(define (V:->        A B)   (V:Pi A (closure/proc #f (lambda (_) B))))
(define (V:construct id a*) `#(V:construct ,id ,a*))
(define V.Symbol            (V:construct 'Symbol  '()))
(define V.Absurd            (V:construct 'Absurd  '()))
(define V.Trivial           (V:construct 'Trivial '()))
(define V.Nat               (V:construct 'Nat     '()))
(define (V:= X from to)     (V:construct '= (list X from to)))
(define V.zero              (V:construct 'zero '()))
(define (V:add1 x)          (V:construct 'add1 (list x)))
(define V.same              (V:construct 'same '()))
(define (V:eliminate id a*) `#(V:eliminate ,id ,a*))
(define (V:apply     f a)   (V:eliminate 'apply (list f a)))
(define (V:car       x)     (V:eliminate 'car   (list x)))
(define (V:cdr       x)     (V:eliminate 'cdr   (list x)))
(define (V:ind-Absurd target motive)
  (V:eliminate 'ind-Absurd (list target motive)))
(define (V:ind-Nat target motive base step)
  (V:eliminate 'ind-Nat (list target motive base step)))
(define (V:ind-= target motive base)
  (V:eliminate 'ind-= (list target motive base)))
(define U.top (V:U #f))

(define env.empty '())
(define (env-extend env x v) (cons (cons x v) env))
(define (env-ref    env x)   (let ((xv (assoc x env))) (and xv (cdr xv))))
(define (env-fresh  env x)
  (let ((x (or x '_)))
    (if (env-ref env x)
        (let ((str.prefix (symbol->string x)))
          (let loop ((counter 0))
            (let ((candidate (string->symbol (string-append str.prefix "." (number->string counter)))))
              (if (env-ref env candidate)
                  (loop (+ counter 1))
                  candidate))))
        x)))

;;;;;;;;;;;;;;;;;;;
;;; Computation ;;;
;;;;;;;;;;;;;;;;;;;

(define (closure/proc x proc)   (cons x proc))
(define (closure/E env x body)  (closure/proc x (lambda (v) (evaluate (env-extend env x v) body))))
(define (closure-parameter c)   (car c))
(define (closure-apply     c v) ((cdr c) v))

(define (elim id . v*)
  (let loop ((id id) (v* v*))
    (match (cons id v*)
      (`(apply #(V:lam ,c)                 ,a) (closure-apply c a))
      (`(apply #(neutral #(V:Pi ,A ,B) ,N) ,a) (neutral (closure-apply B a)
                                                        (V:apply N (V:the A a))))
      (`(car #(V:cons ,a ,b))                  a)
      (`(cdr #(V:cons ,a ,b))                  b)
      (`(car #(neutral #(V:Sigma ,A ,_) ,N))   (neutral A                                (V:car N)))
      (`(cdr #(neutral #(V:Sigma ,A ,B) ,N))   (neutral (closure-apply B (loop 'car v*)) (V:cdr N)))
      (`(ind-Absurd #(neutral ,_ ,N) ,motive)
        (neutral motive (V:ind-Absurd N (V:the U.top motive))))
      (`(ind-Nat #(V:construct zero ()) ,motive ,base ,_)
        base)
      (`(ind-Nat #(V:construct add1 (,n)) ,motive ,base ,step)
        (elim 'apply
              (elim 'apply step n)
              (elim 'ind-Nat n motive base step)))
      (`(ind-Nat ,(and target `#(neutral ,_ ,N)) ,motive ,base ,step)
        (neutral (elim 'apply motive target)
                 (V:ind-Nat N
                            (V:the (V:-> V.Nat U.top) motive)
                            (V:the (elim 'apply motive V.zero) base)
                            (V:the (V:Pi V.Nat
                                         (closure/proc
                                           'n.prev
                                           (lambda (n.prev)
                                             (V:-> (elim 'apply motive n.prev)
                                                   (elim 'apply motive (V:add1 n.prev))))))
                                   step))))
      (`(ind-= #(V:construct same ()) ,motive ,base) base)
      (`(ind-= ,(and target `#(neutral #(V:construct = (,X ,from ,to)) ,N)) ,motive ,base)
        (neutral (elim 'apply (elim 'apply motive to) target)
                 (V:ind-= N
                          (V:the (V:Pi X (closure/proc 'x (lambda (x)
                                                            (V:-> (V:= X from x) U.top))))
                                 motive)
                          (V:the (elim 'apply (elim 'apply motive from) V.same) base))))
      (_ (error "evaluate unimplemented for eliminator" (cons id v*))))))

(define (evaluate env E)
  (let loop ((E E))
    (match E
      (`#(E:the       ,_ ,E)       (loop E))
      (`#(E:U         ,n)          (V:U n))
      (`#(E:Pi        ,x ,A ,B)    (V:Pi    (loop A) (closure/E env x B)))
      (`#(E:Sigma     ,x ,A ,B)    (V:Sigma (loop A) (closure/E env x B)))
      (`#(E:lam       ,x ,body)    (V:lam   (closure/E env x body)))
      (`#(E:cons      ,a ,b)       (V:cons  (loop a) (loop b)))
      (`#(E:symbol    ,s)          (V:symbol s))
      (`#(E:let       ,x ,e ,body) (evaluate (env-extend env x (loop e)) body))
      (`#(E:var       ,x)          (match (env-ref env x)
                                     (`#(annotated ,_ ,v) v)
                                     (#f (error "unbound variable" x))
                                     (v v)))
      (`#(E:construct ,id ,e*)     (V:construct id (map loop e*)))
      (`#(E:eliminate ,id ,e*)     (apply elim id (map loop e*)))
      (_                           (error "cannot evaluate" E)))))

(define (normalize env T V)
  (let loop ((T T) (V V))
    (match* (T V)
      (('#(V:construct Symbol ()) `#(V:symbol ,s))           (E:symbol s))
      ((_                         `#(V:construct ,id  ()))   (E:construct id '()))
      (('#(V:construct Nat ())    `#(V:construct add1 (,x))) (E:add1 (loop T x)))
      ((`#(V:Pi ,A ,B) _)
       (let* ((param (or (match V
                           (`#(V:lam ,c) (closure-parameter c))
                           (_            #f))
                         (closure-parameter B)))
              (param (env-fresh env param))
              (arg   (neutral A (V:var param))))
         (E:lam param (normalize (env-extend env param arg)
                                 (closure-apply B arg)
                                 (elim 'apply V arg)))))
      ((`#(V:Sigma ,A ,B) _)
       (let ((a (elim 'car V)))
         (E:cons (loop A                   a)
                 (loop (closure-apply B a) (elim 'cdr V)))))
      ((`#(V:U ,n) `#(V:Pi ,A ,B))
       (let* ((param (env-fresh env (closure-parameter B)))
              (arg   (neutral A (V:var param))))
         (E:Pi param (loop T A) (normalize (env-extend env param arg)
                                           T
                                           (closure-apply B arg)))))
      ((`#(V:U ,n) `#(V:Sigma ,A ,B))
       (let* ((param (env-fresh env (closure-parameter B)))
              (arg   (neutral A (V:var param))))
         (E:Sigma param (loop T A) (normalize (env-extend env param arg)
                                              T
                                              (closure-apply B arg)))))
      ((`#(V:U ,n) `#(V:U ,m)) (E:U m))
      ((`#(V:U ,n) `#(V:construct = (,X ,from ,to)))
       (E:= (loop T X) (loop X from) (loop X to)))
      ((_ `#(neutral ,_ ,N))
       (define (norm x) (match x (`#(V:the ,X ,x) (loop X x))))
       (let loop ((N N))
         (match N
           (`#(V:var       ,x)      (E:var x))
           (`#(V:eliminate ,id ,v*) (E:eliminate id (cons (loop (car v*))
                                                          (map norm (cdr v*))))))))
      ((_ _) (error "cannot normalize" T V)))))

;;;;;;;;;;;;;;;;;;;;;
;;; Type analysis ;;;
;;;;;;;;;;;;;;;;;;;;;

(define (maybe-pretty-type env T)
  (with-handlers ((exn:fail? (lambda (e) T)))
    (E-pretty (normalize env U.top T))))

(define (synthesize-type env E)
  (define (universe-index env E.type)
    (let ((V (synthesize-type env E.type)))
      (match V
        (`#(V:U ,n) n)
        (_          (pretty-error "not a universe"
                                  `(the ,(maybe-pretty-type env V) ,(E-pretty E.type)))))))
  (let loop ((E E))
    (match E
      (`#(E:the       ,T ,E)    (check-type! env U.top T)
                                (let ((T (evaluate env T)))
                                  (check-type! env T E)
                                  T))
      (`#(E:var       ,x)       (match (env-ref env x)
                                  (`#(neutral   ,T ,_) T)
                                  (`#(annotated ,T ,_) T)
                                  (X                   (error "malformed context entry" x X))))
      (`#(E:U         ,n)       (V:U (+ 1 n)))
      (`#(E:Pi        ,x ,A ,B) (let* ((u.A (universe-index env A))
                                       (u.B (universe-index
                                              (env-extend env x (neutral (evaluate env A)
                                                                         (V:var (env-fresh env x))))
                                              B)))
                                  (V:U (max u.A u.B))))
      (`#(E:Sigma     ,x ,A ,B) (let* ((u.A (universe-index env A))
                                       (u.B (universe-index
                                              (env-extend env x (neutral (evaluate env A)
                                                                         (V:var (env-fresh env x))))
                                              B)))
                                  (V:U (max u.A u.B))))
      (`#(E:let       ,x ,e ,b) (synthesize-type
                                  (env-extend env x (annotated (loop e) (evaluate env e)))
                                  b))
      (`#(E:eliminate ,id ,e*)
        (define (pretty-fail e T . x*)
          (let ((e `(the ,(maybe-pretty-type env T) ,(E-pretty e))))
            (apply pretty-error (append x* (list e 'in: (E-pretty E))))))
        (match (cons id e*)
          (`(apply ,f ,a) (let ((A->B (loop f)))
                            (match A->B
                              (`#(V:Pi ,A ,B)
                                (check-type! env A a)
                                (closure-apply B (evaluate env a)))
                              (_ (pretty-fail f A->B "not a function")))))
          (`(car ,e)      (let ((A&B (loop e)))
                            (match A&B
                              (`#(V:Sigma ,A ,B) A)
                              (_ (pretty-fail e A&B "not a pair")))))
          (`(cdr ,e)      (let ((A&B (loop e)))
                            (match A&B
                              (`#(V:Sigma ,A ,B) (closure-apply B (elim 'car (evaluate env e))))
                              (_ (pretty-fail e A&B "not a pair")))))
          (`(ind-Absurd ,base ,motive)
            (check-type! env V.Absurd base)
            (check-type! env U.top motive)
            motive)
          (`(ind-Nat ,target ,motive ,base ,step)
            (check-type! env V.Nat target)
            (check-type! env (V:-> V.Nat U.top) motive)
            (let ((target (evaluate env target))
                  (motive (evaluate env motive)))
              (check-type! env (elim 'apply motive V.zero) base)
              (check-type! env (V:Pi V.Nat (closure/proc
                                             'n.prev
                                             (lambda (n.prev)
                                               (V:-> (elim 'apply motive n.prev)
                                                     (elim 'apply motive (V:add1 n.prev))))))
                           step)
              (elim 'apply motive target)))
          (`(ind-= ,target ,motive ,base)
            (let ((T (loop target)))
              (match T
                (`#(V:construct = (,X ,from ,to))
                  (check-type! env (V:Pi X (closure/proc 'x (lambda (x)
                                                              (V:-> (V:= X from x) U.top))))
                               motive)
                  (let ((target (evaluate env target))
                        (motive (evaluate env motive)))
                    (check-type! env (elim 'apply (elim 'apply motive from) V.same) base)
                    (elim 'apply (elim 'apply motive to) target)))
                (_ (pretty-fail target T "not an equality proof")))))
          (else (error "synthesize-type unimplemented for eliminator" (cons id e*)))))
      (`#(E:symbol ,s)                        V.Symbol)
      ('#(E:construct sole    ())             V.Trivial)
      ('#(E:construct zero    ())             V.Nat)
      (`#(E:construct add1    (,e))           (check-type! env V.Nat e) V.Nat)
      ('#(E:construct Symbol  ())             (V:U 0))
      ('#(E:construct Absurd  ())             (V:U 0))
      ('#(E:construct Trivial ())             (V:U 0))
      ('#(E:construct Nat     ())             (V:U 0))
      (`#(E:construct =       (,X ,from ,to)) (let ((u (universe-index env X))
                                                    (X (evaluate env X)))
                                                (check-type! env X from)
                                                (check-type! env X to)
                                                (V:U u)))
      (_ (pretty-error "expression that cannot synthesize a type" (E-pretty E))))))

(define (check-type! env T E)
  (let loop! ((T T) (E E))
    (define (pretty-fail . x*)
      (apply pretty-error (append x* (list (maybe-pretty-type env T)
                                           'expression-being-checked:
                                           (E-pretty E)))))
    (match E
      (`#(E:lam ,x ,body)
        (match T
          (`#(V:Pi ,A ,B) (let ((a (neutral A (V:var (env-fresh env x)))))
                            (check-type! (env-extend env x a) (closure-apply B a) body)))
          (_              (pretty-fail "not a function type"))))
      (`#(E:cons ,a ,b)
        (match T
          (`#(V:Sigma ,A ,B) (loop! A a)
                             (loop! (closure-apply B (evaluate env a)) b))
          (_                 (pretty-fail "not a pair type"))))
      ('#(E:construct same ())
       (match T
         (`#(V:construct = (,X ,from ,to))
           (unless (alpha-equivalent?/U<= eq? (normalize env X from) (normalize env X to))
             (pretty-write (E-pretty (normalize env U.top T)))
             (pretty-fail "not the same expression in equality type")))
         (_ (pretty-fail "not an equality type"))))
      ('#(E:construct TODO ()) (void))
      (_ (let ((T.synth (synthesize-type env E)))
           (unless (alpha-equivalent?/U<=
                     (lambda (m n) (or (not n) (and m (<= m n))))
                     (normalize env U.top T.synth)
                     (normalize env U.top T))
             (pretty-error "wrong type"
                           (maybe-pretty-type env T)
                           'expression-being-checked:
                           `(the ,(maybe-pretty-type env T.synth) ,(E-pretty E)))))))))

(define (alpha-equivalent?/U<= U<=? A B)
  (let loop ((count 0) (env.A env.empty) (A A) (env.B env.empty) (B B))
    (match* (A B)
      ((`#(E:U ,m)           `#(E:U ,n))           (U<=? m n))
      ((`#(E:var ,x)         `#(E:var ,y))         (let ((ix (env-ref env.A x))
                                                         (iy (env-ref env.B y)))
                                                     (if (and ix iy)
                                                         (= ix iy)
                                                         (and (not ix) (not iy) (eq? x y)))))
      ((`#(E:Pi    ,x ,A ,B) `#(E:Pi    ,y ,C ,D)) (let ((count.next (+ count 1)))
                                                     (and (loop count.next env.A A env.B C)
                                                          (loop count.next
                                                                (env-extend env.A x count) B
                                                                (env-extend env.B y count) D))))
      ((`#(E:Sigma ,x ,A ,B) `#(E:Sigma ,y ,C ,D)) (let ((count.next (+ count 1)))
                                                     (and (loop count.next env.A A env.B C)
                                                          (loop count.next
                                                                (env-extend env.A x count) B
                                                                (env-extend env.B y count) D))))
      ((`#(E:lam ,x ,a)      `#(E:lam ,y ,b))      (let ((count.next (+ count 1)))
                                                     (loop count.next
                                                           (env-extend env.A x count) a
                                                           (env-extend env.B y count) b)))
      ((`#(E:cons ,a ,b)     `#(E:cons ,c ,d))     (and (loop count env.A a env.B c)
                                                        (loop count env.A b env.B d)))
      ((`#(E:symbol ,a)      `#(E:symbol ,b))      (eq? a b))
      ((`#(E:construct ,id.A ,a*) `#(E:construct ,id.B ,b*))
       (and (eq? id.A id.B) (andmap (lambda (a b) (loop count env.A a env.B b)) a* b*)))
      ((`#(E:eliminate ,id.A ,a*) `#(E:eliminate ,id.B ,b*))
       (and (eq? id.A id.B) (andmap (lambda (a b) (loop count env.A a env.B b)) a* b*)))
      ((_ _) #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Printing and parsing ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (E-pretty E)
  (let loop ((E E))
    (match E
      (`#(E:U         0)             'U)
      (`#(E:U         ,n)            `(U ,n))
      (`#(E:Pi        #f ,A ,B)      (let loop.-> ((B B) (A* (list (loop A))))
                                       (match B
                                         (`#(E:Pi #f ,A ,B) (loop.-> B (cons (loop A) A*)))
                                         (_                 `(-> ,@(reverse A*) ,(loop B))))))
      (`#(E:Pi        ,x ,A ,B)      (let loop.pi ((B B) (xA* (list (list x (loop A)))))
                                       (match B
                                         (`#(E:Pi ,x ,A ,B) (loop.pi B (cons (list x (loop A)) xA*)))
                                         (_                 `(Pi ,(reverse xA*) ,(loop B))))))
      (`#(E:Sigma     #f ,A ,B)      `#(Pair ,(loop A) ,(loop B)))
      (`#(E:Sigma     ,x ,A ,B)      (let loop.sigma ((B B) (xA* (list (list x (loop A)))))
                                       (match B
                                         (`#(E:Sigma ,x ,A ,B) (loop.sigma B (cons (list x (loop A)) xA*)))
                                         (_                    `(Sigma ,(reverse xA*) ,(loop B))))))
      (`#(E:lam       ,x ,body)      (let loop.lam ((body body) (x* (list x)))
                                       (match body
                                         (`#(E:lam ,x ,body) (loop.lam body (cons x x*)))
                                         (_                  `(lambda ,(reverse x*) ,(loop body))))))
      (`#(E:cons      ,a ,b)         `(cons ,(loop a) ,(loop b)))
      (`#(E:symbol    ,s)            `(quote ,s))
      (`#(E:the       ,t ,e)         `(the ,(loop t) ,(loop e)))
      (`#(E:let       ,x ,e ,body)   (let loop.let ((body body) (xe* (list (list x (loop e)))))
                                       (match body
                                         (`#(E:let ,x ,e ,body) (loop.let body (cons (list x (loop e)) xe*)))
                                         (_                     (list (if (null? (cdr xe*)) 'let 'let*)
                                                                      (reverse xe*) (loop body))))))
      (`#(E:var       ,x)            x)
      ('#(E:construct #t   ())       #t)
      ('#(E:construct #f   ())       #f)
      ('#(E:construct zero ())       0)
      (`#(E:construct add1 (,n))     (let ((n (loop n))) (if (nat? n) (+ 1 n) `(add1 ,n))))
      (`#(E:construct ,id ())        id)
      (`#(E:construct ,id ,e*)       `(,id . ,(map loop e*)))
      (`#(E:eliminate apply (,f ,a)) (let loop.app ((f f) (a* (list (loop a))))
                                       (match f
                                         (`#(E:eliminate apply (,f ,a)) (loop.app f (cons (loop a) a*)))
                                         (_                             `(,(loop f) . ,a*)))))
      (`#(E:eliminate ,id ,e*)       `(,id . ,(map loop e*))))))

(define (parse-apply env stx)
  (let loop ((E.f (parse env (car stx))) (operand* (cdr stx)))
    (if (pair? operand*)
        (loop (E:apply E.f (parse env (car operand*)))
              (cdr operand*))
        (if (null? operand*)
            E.f
            (pretty-error "not a list" stx)))))

(define (parse env stx)
  (define (parse-operation operator env stx)
    (let ((operation (env-ref env operator)))
      (if operation
          (operation env stx)
          (pretty-error "unbound variable" operator))))
  (let loop ((stx stx))
    (cond
      ((symbol?  stx) (parse-operation stx env stx))
      ((pair?    stx) (let ((operator (car stx)))
                        (if (symbol? operator)
                            (parse-operation operator env stx)
                            (parse-apply env stx))))
      ((nat?     stx) (parse-nat stx))
      ((boolean? stx) (E:boolean stx))
      (else           (pretty-error "not syntax" stx)))))

(define (parse* env stx*) (map (lambda (stx) (parse env stx)) stx*))

(define ((parse-variable-ref/id id) env stx)
  (if (symbol? stx)
      (E:var id)
      (parse-apply env stx)))

(define (parse-nat n)
  (let loop ((n n))
    (if (eq? 0 n)
        E.zero
        (E:add1 (loop (- n 1))))))

(define ((expression-operator-parser parser argc.min argc.max) env stx*)
  (unless (list? stx*) (pretty-error "not a list" stx*))
  (let ((argc (- (length stx*) 1)))
    (unless (<= argc.min argc)           (pretty-error "too few arguments"  stx*))
    (unless (<= argc (or argc.max argc)) (pretty-error "too many arguments" stx*))
    (apply parser env (cdr stx*))))

(define (parse-binding-pair* stx)
  (unless (list? stx) (pretty-error "not a list" stx))
  (unless (pair? stx) (pretty-error "empty binding list"))
  (for-each (lambda (xt) (unless (and (list? xt) (= (length xt) 2))
                           (pretty-error "not a binding pair")))
            stx))

(define ((parse-nested-binder E:binder) env xe* body)
  (parse-binding-pair* xe*)
  (let loop ((env env) (xe* xe*))
    (if (null? xe*)
        (parse env body)
        (let* ((xe (car xe*)) (x (car xe)))
          (E:binder (car xe) (parse env (cadr xe))
                    (loop (env-extend env x (parse-variable-ref/id x))
                          (cdr xe*)))))))

(define parse-Pi    (parse-nested-binder E:Pi))
(define parse-Sigma (parse-nested-binder E:Sigma))
(define parse-let*  (parse-nested-binder E:let))

(define (parse-let env xe* body)
  (parse-binding-pair* xe*)
  (unless (null? (cdr xe*)) (pretty-error "more than one let binding" xe*))
  (let* ((xe (car xe*)) (x (car xe)))
    (E:let x (parse env (cadr xe))
           (parse (env-extend env x (parse-variable-ref/id x)) body))))

(define (parse--> env e . e*)
  (let ((e* (reverse (cons e e*))))
    (let loop ((e (parse env (car e*))) (e* (cdr e*)))
      (if (null? e*)
          e
          (loop (E:Pi #f (parse env (car e*)) e) (cdr e*))))))

(define (parse-Tuple env . e*)
  (let loop ((e* e*))
    (if (null? e*)
        E.Trivial
        (E:Sigma #f (parse env (car e*)) (loop (cdr e*))))))

(define (parse-Pair env a b) (E:Sigma #f (parse env a) (parse env b)))

(define (parse-the env t body) (E:the (parse env t) (parse env body)))

(define (parse-lambda env x* body)
  (unless (list? x*) (pretty-error "not a list" x*))
  (unless (pair? x*) (pretty-error "empty parameter list"))
  (let loop ((env env) (x* x*))
    (if (null? x*)
        (parse env body)
        (let ((x (car x*)))
          (E:lam x (loop (env-extend env x (parse-variable-ref/id x)) (cdr x*)))))))

(define (parse-quote env x)
  (let loop ((x x))
    (cond
      ((symbol?  x) (E:symbol x))
      ((nat?     x) (parse-nat x))
      ((boolean? x) (E:boolean x))
      ((null?    x) E.sole)
      ((pair?    x) (E:cons (loop (car x) (cdr x))))
      (else         (pretty-error "not a literal" x)))))

(define (parse-cons env a b) (E:cons (parse env a) (parse env b)))

(define (parse-U env stx)
  (cond ((symbol? stx) (E:U 0))
        (else (unless (and (pair? stx)
                           (pair? (cdr stx))
                           (null? (cddr stx)))
                (pretty-error "wrong number of arguments" stx))
              (let ((index (cadr stx)))
                (unless (nat? index) (pretty-error "not a universe index" index))
                (E:U index)))))

(define env.initial
  (foldl*
    env-extend
    env.empty
    (append
      `((U ,parse-U))
      (map* (lambda (name parser argc.min argc.max)
              (list name (expression-operator-parser parser argc.min argc.max)))
            `((Pi     ,parse-Pi     2 2)
              (Sigma  ,parse-Sigma  2 2)
              (->     ,parse-->     2 #f)
              (Pair   ,parse-Pair   2 2)
              (Tuple  ,parse-Tuple  0 #f)
              (the    ,parse-the    2 2)
              (lambda ,parse-lambda 2 2)
              (let    ,parse-let    2 2)
              (let*   ,parse-let*   2 2)
              (quote  ,parse-quote  1 1)
              (cons   ,parse-cons   2 2)))
      (map* (lambda (name argc)
              (list name (expression-operator-parser
                           (lambda (env . stx*) (E:construct name (parse* env stx*)))
                           argc argc)))
            '((add1   1)
              (=      3)
              ;(List   1)
              ;(Vec    2)
              ;(Either 2)
              ;(::     2)
              ;(vec::  2)
              ;(left   1)
              ;(right  1)
              ))
      (map* (lambda (name argc)
              (list name (expression-operator-parser
                           (lambda (env . stx*) (E:eliminate name (parse* env stx*)))
                           argc argc)))
            '((car         1)
              (cdr         1)
              (ind-Absurd  2)
              (ind-Nat     4)
              (ind-=       3)  ; should we provide replace instead or implement it in terms of ind-= ?
              ;(ind-List    4)
              ;(ind-Vec     5)
              ;(head        1)
              ;(tail        1)
              ;(ind-Either  4)
              ))
      (map (lambda (name)
             (list name (lambda (env stx)
                          (unless (symbol? stx) (pretty-error "bad syntax" name stx))
                          (E:construct name '()))))
           '(TODO Symbol Absurd Trivial Nat sole zero same
                  ;nil vecnil
                  )))))

;;;;;;;;;;;;;;;;
;;; Examples ;;;
;;;;;;;;;;;;;;;;

(define (run-verbose stx)
  (displayln "\n================================================================")
  (displayln "input syntax:")
  (pretty-write stx)
  (displayln "================================================================")
  (displayln "parsing expression...")
  (let ((E (time (parse env.initial stx))))
    ;(displayln "================================================================")
    ;(displayln "parsed expression:")
    ;(pretty-write (E-pretty E))
    ;(displayln "================================================================")
    (displayln "synthesizing type...")
    (let ((T.value (time (synthesize-type env.initial E))))
      (displayln "normalizing type...")
      (let ((T (time (normalize env.initial U.top T.value))))
        (displayln "================================================================")
        (displayln "normalized type:")
        (pretty-write (E-pretty T))
        (displayln "================================================================")
        (displayln "evaluating expression...")
        (let ((V (time (evaluate env.initial E))))
          (displayln "normalizing value...")
          (let ((E (time (normalize env.initial T.value V))))
            (displayln "================================================================")
            (displayln "final result:")
            (pretty-write (E-pretty (E:the T E)))
            (displayln "================================================================")))))))

(for-each run-verbose
          '(
            (the (-> Nat Nat (-> Nat Nat Nat) Nat)
                 (lambda (a a f) (f a a)))
            (the (-> Nat (-> Nat (-> (-> Nat (-> Nat Nat)) Nat)))
                 (lambda (a)
                   (lambda (a)
                     (lambda (f) ((f a) a)))))
            ((the (-> Nat Nat Nat Nat) (lambda (a b c) b)) 1 2 3)
            ((the (-> Nat Nat Nat Nat) (lambda (a b c) (add1 b))) 1 2 3)
            0
            4
            'foo
            (the (= Nat 2 2) same)

            (let ((+ (the (-> Nat Nat Nat)
                          (lambda (a b)
                            (ind-Nat
                              a
                              (lambda (_) Nat)
                              b
                              (lambda (_ n) (add1 n)))))))
              (+ 3 2))

            (let* ((iter-Nat (the (Pi ((X U)) (-> Nat X (-> X X) X))
                                  (lambda (X n base step)
                                    (ind-Nat
                                      n
                                      (lambda (_) X)
                                      base
                                      (lambda (_) step)))))
                   (+ (the (-> Nat Nat Nat)
                           (lambda (a b)
                             (iter-Nat
                               Nat
                               a
                               b
                               (lambda (n) (add1 n)))))))
              (+ 4 3))

            (the (Pi ((a Nat) (b Nat))
                     (-> (= Nat a b) (= Nat b a)))
                 (lambda (a b proof.a=b)
                   (ind-= proof.a=b
                          (lambda (x _) (= Nat x a))
                          same)))

            (let ((symm (the (Pi ((X U) (from X) (to X))
                                 (-> (= X from to) (= X to from)))
                             (lambda (X from to target)
                               (ind-= target
                                      (lambda (x _) (= X x from))
                                      same)))))
              (the (Pi ((a Nat) (b Nat))
                       (-> (= Nat a b) (= Nat b a)))
                   (lambda (a b proof.a=b)
                     (symm Nat a b proof.a=b))))

            (let ((replace (the (Pi ((X U) (from X) (to X))
                                    (-> (= X from to)
                                        (Pi ((motive (-> X U)))
                                            (-> (motive from) (motive to)))))
                                (lambda (X from to target motive base)
                                  (ind-= target
                                         (lambda (x _) (motive x))
                                         base)))))
              (the (Pi ((a Nat) (b Nat))
                       (-> (= Nat a b) (= Nat b a)))
                   (lambda (a b proof.a=b)
                     (replace Nat a b
                              proof.a=b
                              (lambda (x) (= Nat x a))
                              same))))

            (let* ((replace (the (Pi ((X U) (from X) (to X))
                                     (-> (= X from to)
                                         (Pi ((motive (-> X U)))
                                             (-> (motive from) (motive to)))))
                                 (lambda (X from to target motive base)
                                   (ind-= target
                                          (lambda (x _) (motive x))
                                          base)))))
              (the (Pi ((a Nat) (b Nat))
                       (-> (= Nat a b) (= Nat (add1 a) (add1 b))))
                   (lambda (a b proof.a=b)
                     (replace Nat a b
                              proof.a=b
                              (lambda (x) (= Nat (add1 a) (add1 x)))
                              same))))

            (let* ((replace (the (Pi ((X U) (from X) (to X))
                                     (-> (= X from to)
                                         (Pi ((motive (-> X U)))
                                             (-> (motive from) (motive to)))))
                                 (lambda (X from to target motive base)
                                   (ind-= target
                                          (lambda (x _) (motive x))
                                          base))))
                   (cong (the (Pi ((X U) (Y U) (from X) (to X))
                                  (-> (= X from to)
                                      (Pi ((f (-> X Y)))
                                          (= Y (f from) (f to)))))
                              (lambda (X Y from to target f)
                                (replace X from to
                                         target
                                         (lambda (x) (= Y (f from) (f x)))
                                         same)))))
              (the (Pi ((a Nat) (b Nat))
                       (-> (= Nat a b) (= Nat (add1 a) (add1 b))))
                   (lambda (a b proof.a=b)
                     (cong Nat Nat a b proof.a=b (lambda (n) (add1 n))))))

            ;; Arithmetic with proofs
            (let* ((iter-Nat (the (Pi ((X U)) (-> Nat X (-> X X) X))
                                  (lambda (X n base step)
                                    (ind-Nat
                                      n
                                      (lambda (_) X)
                                      base
                                      (lambda (_) step)))))
                   (+ (the (-> Nat Nat Nat)
                           (lambda (a b) (iter-Nat Nat a b (lambda (n) (add1 n))))))
                   (* (the (-> Nat Nat Nat)
                           (lambda (a b) (iter-Nat Nat a 0 (+ b)))))
                   (replace (the (Pi ((X U) (from X) (to X))
                                     (-> (= X from to)
                                         (Pi ((motive (-> X U)))
                                             (-> (motive from) (motive to)))))
                                 (lambda (X from to target motive base)
                                   (ind-= target
                                          (lambda (x _) (motive x))
                                          base))))
                   (cong (the (Pi ((X U) (Y U) (from X) (to X))
                                  (-> (= X from to)
                                      (Pi ((f (-> X Y)))
                                          (= Y (f from) (f to)))))
                              (lambda (X Y from to target f)
                                (replace X from to
                                         target
                                         (lambda (x) (= Y (f from) (f x)))
                                         same))))
                   (symm (the (Pi ((X U) (from X) (to X))
                                  (-> (= X from to) (= X to from)))
                              (lambda (X from to target)
                                (ind-= target
                                       (lambda (x _) (= X x from))
                                       same))))
                   (proof.+-associative
                     (the (Pi ((a Nat) (b Nat) (c Nat))
                              (= Nat (+ (+ a b) c) (+ a (+ b c))))
                          (lambda (a b c)
                            (ind-Nat
                              a
                              (lambda (a) (= Nat
                                             (+ (+ a b) c)
                                             (+ a (+ b c))))
                              same
                              (lambda (m proof.m)
                                (cong Nat Nat (+ (+ m b) c) (+ m (+ b c)) proof.m (+ 1)))))))
                   (proof.+-commutative
                     (the (Pi ((a Nat) (b Nat))
                              (= Nat (+ b a) (+ a b)))
                          (lambda (a b)
                            (ind-Nat
                              a
                              (lambda (a) (= Nat (+ b a) (+ a b)))
                              (ind-Nat
                                b
                                (lambda (b) (= Nat (+ b 0) b))
                                same
                                (lambda (n proof.n) (cong Nat Nat (+ n 0) n proof.n (+ 1))))
                              (lambda (m proof.m)
                                (replace
                                  Nat (+ 1 (+ b m)) (+ b (+ 1 m))
                                  (ind-Nat
                                    b
                                    (lambda (b) (= Nat (+ 1 (+ b m)) (+ b (+ 1 m))))
                                    same
                                    (lambda (n proof.n)
                                      (cong Nat Nat (+ 1 (+ n m)) (+ n (+ 1 m))
                                            proof.n (+ 1))))
                                  (lambda (x) (= Nat x (+ 1 (+ m b))))
                                  (cong Nat Nat (+ b m) (+ m b) proof.m (+ 1))))))))
                   (proof.*-distributive
                     (the (Pi ((a Nat) (b Nat) (c Nat))
                              (= Nat (* a (+ b c)) (+ (* a b) (* a c))))
                          (lambda (a b c)
                            (ind-Nat
                              a
                              (lambda (a)
                                (= Nat
                                   (* a (+ b c))
                                   (+ (* a b) (* a c))))
                              same
                              (lambda (m proof.m)
                                (ind-=
                                  (proof.+-associative (+ b (* m b)) c (* m c))
                                  (lambda (x _) (= Nat
                                                   (+ (+ b c)
                                                      (* m (+ b c)))
                                                   x))
                                  (ind-=
                                    (symm Nat (+ (+ b (* m b)) c) (+ b (+ (* m b) c))
                                          (proof.+-associative b (* m b) c))
                                    (lambda (x _) (= Nat
                                                     (+ (+ b c)
                                                        (* m (+ b c)))
                                                     (+ x (* m c))))
                                    (ind-=
                                      (proof.+-commutative (* m b) c)
                                      (lambda (x _) (= Nat
                                                       (+ (+ b c)
                                                          (* m (+ b c)))
                                                       (+ (+ b x)
                                                          (* m c))))
                                      (ind-=
                                        (proof.+-associative b c (* m b))
                                        (lambda (x _) (= Nat
                                                         (+ (+ b c)
                                                            (* m (+ b c)))
                                                         (+ x (* m c))))
                                        (ind-=
                                          (symm Nat
                                                (+ (+ (+ b c) (* m b)) (* m c))
                                                (+ (+ b c) (+ (* m b) (* m c)))
                                                (proof.+-associative (+ b c) (* m b) (* m c)))
                                          (lambda (x _) (= Nat
                                                           (+ (+ b c)
                                                              (* m (+ b c)))
                                                           x))
                                          (ind-=
                                            proof.m
                                            (lambda (x _) (= Nat
                                                             (+ (+ b c)
                                                                (* m (+ b c)))
                                                             (+ (+ b c)
                                                                x)))
                                            same)))))))))))
                   (proof.*-commutative
                     (the (Pi ((a Nat) (b Nat))
                              (= Nat (* b a) (* a b)))
                          (lambda (a b)
                            (ind-Nat
                              a
                              (lambda (a) (= Nat (* b a) (* a b)))
                              (ind-Nat
                                b
                                (lambda (b) (= Nat (* b 0) 0))
                                same
                                (lambda (n proof.n) proof.n))
                              (lambda (m proof.m)
                                (ind-=
                                  (symm Nat (* b (+ 1 m)) (+ (* b 1) (* b m))
                                        (proof.*-distributive b 1 m))
                                  (lambda (x _) (= Nat
                                                   x
                                                   (+ b (* m b))))
                                  (ind-=
                                    proof.m
                                    (lambda (x _) (= Nat
                                                     (+ (* b 1) (* b m))
                                                     (+ b x)))
                                    (ind-=
                                      (ind-Nat
                                        b
                                        (lambda (b) (= Nat b (* b 1)))
                                        same
                                        (lambda (n proof.n) (cong Nat Nat n (* n 1) proof.n (+ 1))))
                                      (lambda (x _)
                                        (= Nat
                                           (+ x (* b m))
                                           (+ b (* b m))))
                                      same))))))))
                   (proof.*-associative
                     (the (Pi ((a Nat) (b Nat) (c Nat))
                              (= Nat (* (* a b) c) (* a (* b c))))
                          (lambda (a b c)
                            (ind-Nat
                              a
                              (lambda (a) (= Nat
                                             (* (* a b) c)
                                             (* a (* b c))))
                              same
                              (lambda (m proof.m)
                                (ind-=
                                  (proof.*-commutative (+ b (* m b)) c)
                                  (lambda (x _)
                                    (= Nat
                                       x
                                       (+ (* b c) (* m (* b c)))))
                                  (ind-=
                                    (symm Nat (* c (+ b (* m b))) (+ (* c b) (* c (* m b)))
                                          (proof.*-distributive c b (* m b)))
                                    (lambda (x _) (= Nat
                                                     x
                                                     (+ (* b c) (* m (* b c)))))
                                    (ind-=
                                      (proof.*-commutative b c)
                                      (lambda (x _) (= Nat
                                                       (+ (* c b) (* c (* m b)))
                                                       (+ x (* m (* b c)))))
                                      (ind-=
                                        (proof.*-commutative c (* m b))
                                        (lambda (x _) (= Nat
                                                         (+ (* c b) x)
                                                         (+ (* c b) (* m (* b c)))))
                                        (ind-=
                                          proof.m
                                          (lambda (x _) (= Nat
                                                           (+ (* c b) (* (* m b) c))
                                                           (+ (* c b) x)))
                                          same)))))))))))
              sole)

;            ;; this should succeed
;            (the (Pi ((y Nat)) (= Nat y y))
;                 (lambda (y)
;                   (let ((z y))
;                     ((the (Pi ((x Nat)) (= Nat z y))
;                           (lambda (x) same))
;                      2))))
;
;            ;; this correctly fails
;            ;(the (Pi ((y Nat)) (= Nat y 2))
;            ;     (lambda (y)
;            ;       (let ((z y))
;            ;         ((the (Pi ((y Nat)) (= Nat z y))
;            ;               (lambda (y) same))
;            ;          2))))
;
;            ;; this succeeds due to TODO
;            (the (Pi ((y Nat)) (= Nat y 2))
;                 (lambda (y)
;                   (let ((z y))
;                     ((the (Pi ((y Nat)) (= Nat z y))
;                           (lambda (y) TODO))
;                      2))))
;
;            ;; This correctly fails
;            ;(the (Pi ((y Nat)) (= Nat y 2))
;            ;     (lambda (y)
;            ;       ((the ((the (-> Nat U)
;            ;                   (lambda (z) (Pi ((y Nat)) (= Nat z y))))
;            ;              y)
;            ;             (lambda (y) same))
;            ;        2)))
;
;            ;; This succeeds due to TODO
;            (the (Pi ((y Nat)) (= Nat y 2))
;                 (lambda (y)
;                   ((the ((the (-> Nat U)
;                               (lambda (z) (Pi ((y Nat)) (= Nat z y))))
;                          y)
;                         (lambda (y) TODO))
;                    2)))
;
;            ;; This correctly fails
;            ;(the (Pi ((y Nat))
;            ;         ((the (-> Nat (U 1))
;            ;               (lambda (x)
;            ;                 (Pi ((y Nat))
;            ;                     (= U
;            ;                        ((the (-> Nat U)
;            ;                              (lambda (z) (= Nat x z)))
;            ;                         y)
;            ;                        (= Nat y y)))))
;            ;          y))
;            ;     (lambda (a b) same))
;
;            ;; This correctly fails
;            ;(the (Pi ((y Nat))
;            ;        ((the (-> Nat U)
;            ;              (lambda (x)
;            ;                (Pi ((y Nat))
;            ;                    (= (= Nat y y)
;            ;                        (the (= Nat x y)
;            ;                            same)
;            ;                        (the (= Nat y y)
;            ;                            same)))))
;            ;          y))
;            ;    (lambda (a b) same))
;
;            ;; This correctly fails, but succeeds if Pi-bound variables are not renamed
;            ;(the (Pi ((y Nat))
;            ;         (let ((x y))
;            ;           (Pi ((y Nat))
;            ;               (= (= Nat y y)
;            ;                  (the (= Nat x y)
;            ;                       same)
;            ;                  (the (= Nat y y)
;            ;                       same)))))
;            ;     (lambda (a b) same))
;
;            ;; This correctly fails
;            ;(the (Pi ((y Nat))
;            ;         (let ((x y))
;            ;           (Pi ((y Nat))
;            ;               (= U (= Nat x y) (= Nat y y)))))
;            ;     (lambda (a b) same))

            ))
