#lang racket

(require "util.rkt")

; TODO
; eager and lazy CBV operational semantics
;   CBV describes observable semantics while lazy/eager describes operational strategy
; eager CBV
;   constructors with args need to save one-hole contexts
;   all terms in focus are evaluated to completion with result values being catalogued
; lazy CBV
;   constructors with args do not save any one-hole contexts
;     they can punt on evaluating their args
;     punted terms are paired with current environment and catalogued as eval obligations
;   rewinding to evaluate an obligation
;     pop catalog entries until desired key is found
;     save popped entries (in reverse) with old context
;     begin evaluating a new state with the remaining catalog
;       (state clg-oblig-term (return-context (old-cont old-env reversed-entries)) clg-oblig-env)
;     when returning to former context, re-push its entries onto the new catalog
;   the catalog as described is actually a special case of a more general 'effect log'
;     entry types can be added for memory allocation and writes
; given a catalog partitioning at any event boundary: members of older part do not depend on members of newer
;   minimize duplication when splitting worlds around a hypothetical equality
;     assumption boundary must be made before any key that would depend on it
;       given key D being guessed
;         assumption entry must be made before first E depending on D's value
;         there may be entries between D and its assumption entry
;           this would be because they depend on D's effects, but don't depend on D's value
;       if made earlier than existing assumption, must split the splits
;         this will happen with out-of-order case analysis on opaque values:
;           first, case-analysis occurs on some D that happens to depend on C
;           later, in one branch of (case D), retroactive case-analysis occurs on C
;           the case-analysis on C must be pulled above that of D
;             C's assumption entry must be made earlier than D's entry
;             new split muts be made earlier than existing split, duplicating that split
;             some waste produced (hopefully only temporarily) for the branch of D not analyzing C
;       older keys definitely don't depend on assumption and make up the old region
;       newer keys that happen to also not depend on assumption may be moved across it into the old region
;         moving across also requires no effect dependencies
;       old region ends up shared by both hypothetical worlds
;   cleaning up after a transformation attempt on a subterm
;     when producing result, only need to garbage collect entries newer than the subterm
;   there should be a single key allocator so that every value, even across partitions/worlds, has a unique key
;     when partitioning based on hypothetical equality, copy all keys dependent on assumed value
;       when re-combining worlds, new world only contributes keys newer than split
; assumptions in effect log mark when the world split
;   new world not responsible for old effects, though may evaluate them under assumptions to see what they would provide
;   when re-combining with old world, only effects after assumption are contributed
;   after re-combining, assumptions are used to unify target keys and generate code to define new keys
; diagram:
;   example of optimal assumption placement
;   case D, where e's depend on D, c's do not depend on D at all, x's depend only on D's effect
; newer ---------------------------------- older
; ... e e e e (assume D = _) x x x D c c c c ...
;
; future small-step ideas
;(data assumption
  ;(assume-eq (key0 key1))
  ;(assume-neq (key0 key1))
  ;(assume-value (key new-keys value)))
;(data clg-entry
  ;(clg-data (kvs)))   plural, allowing SCCs (let-rec) to satisfy partition property
  ;(clg-obligation (key term env notes))
  ;(clg-assumption (assumed)))
  ;(clg-memory-effect ())
  ;(clg-stream-effect ())
  ;(clg-reset (marker))
  ;etc.
;(data cont
  ;(ohc (cont oh))
  ;(halt ())
  ;(return-caller (cont env)))
  ;(return-context (cont env clg-replay)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; abstract syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(data value-atomic
  (promise (uid))
  (indirect (uid))
  (uno ())
  (sym (name)))

(data value-compound
  (lam (body env))
  (pair (l r)))

(data term
  (val-a (x))
  (val-c (x))
  (bound (idx))
  (app (proc arg))
  (if-eq (sym0 sym1 true false))
  (pair-left (x))
  (pair-right (x))
  (let-rec (defs body)))

(variant (term-context (base finished pending)))

(define (term->context term)
  (match-let (((cons base pending)
    (match term
      ((val-a x)                    (cons term '()))
      ((val-c (lam body env))       (cons (val-c
                                            (lam (term->context body) env))
                                          '()))
      ((val-c (pair l r))           (cons (val-c (pair '() '())) (list l r)))
      ((bound idx)                  (cons term '()))
      ((app proc arg)               (cons (app '() '()) (list proc arg)))
      ((if-eq sym0 sym1 true false) (cons (if-eq '() '()
                                                 (term->context true)
                                                 (term->context false))
                                          (list sym0 sym1)))
      ((pair-left x)                (cons (pair-left '()) (list x)))
      ((pair-right x)               (cons (pair-right '()) (list x)))
      ((let-rec defs body)          (cons (let-rec (map term->context defs)
                                                   (term->context body))
                                          '())))))
    (term-context base '() (map term->context pending))))

(define (context->term tc)
  (match tc
    ((term-context base finished _)
     (let ((finished (reverse finished)))
       (match base
         ((val-a x)              base)
         ((val-c (lam _ _))      base)
         ((val-c (pair _ _))     (val-c (apply pair finished)))
         ((bound idx)            base)
         ((app proc arg)         (apply app finished))
         ((if-eq _ _ true false) (apply
                                   (lambda (s0 s1) (if-eq s0 s1 true false))
                                   finished))
         ((pair-left _)          (apply pair-left finished))
         ((pair-right _)         (apply pair-right finished))
         ((let-rec _ _)          base))))))

(define (term-context-add tc val)
  (match tc
    ((term-context base finished pending)
     (term-context base (cons val finished) pending))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; small-step interpretation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(data cont
  (ohc (oh cont))
  (halt ())
  (return-caller (env cont))
  (return-update (puid env cont)))

(variant (state (focus cont env clg next-uid)))

(data promise-entry
  (delayed (tc env))
  (kept (atom))
  (broken (x)))

(data promise-entry-broken
  (skolem (uid))
  (abandoned (tc env))
  (sym-ineq (names promises)))

(variant (catalog (data promises)))
(define clg-empty (catalog dict-empty dict-empty))
(define (clg-add-datum clg uid val)
  (match clg
    ((catalog data promises)
     (catalog (dict-add data uid val) promises))))
(define (clg-add-data clg uids vals)
  (foldl (lambda (k v c) (clg-add-datum c k v)) clg uids vals))
(define (clg-get-datum clg uid) (dict-get (catalog-data clg) uid))
(define (clg-add-promise clg uid prom)
  (match clg
    ((catalog data promises)
     (catalog data (dict-add promises uid prom)))))
(define (clg-get-promise clg uid) (dict-get (catalog-promises clg) uid))

(define (atom->compound pred desc-str clg atom)
  (match atom
    ((indirect uid)
     (do either-monad
       val <- (maybe->either
                (format "(indirect ~a) not found in catalog: ~s" clg uid)
                (clg-get-datum clg uid))
       (if (pred val) (right val)
         (left (format "expected ~a but found: ~s" desc-str val)))))
    (_ (left (format "expected indirection but found: ~s" atom)))))
(define (atom->lam clg atom) (atom->compound lam? "lambda" clg atom))
(define (atom->pair clg atom) (atom->compound pair? "pair" clg atom))
(define (atom->sym atom)
  (match atom
    ((sym name) (right name))
    (_ (left (format "expected symbol but found: ~s" atom)))))

(define env-empty '())
(define (env-extend env v) (cons v env))
(define (env-extends env vs) (append (reverse vs) env))
(define (env-lookup env idx)
  (match env
    ('() (nothing))
    ((cons val env)
     (if (= idx 0) (just val) (env-lookup env (- idx 1))))))

(define (val-a-context atom) (term->context (val-a atom)))

(define (state-init tctxt) (state tctxt (halt) env-empty clg-empty 0))
(define (state-refocus st clg-inject item uid->atom)
  (let ((clg (state-clg st)) (cur-uid (state-next-uid st)))
    (let ((clg (clg-inject clg cur-uid item))
          (focus (val-a-context (uid->atom cur-uid)))
          (next-uid (+ cur-uid 1)))
      (state focus (state-cont st) (state-env st) clg next-uid))))
(define (state-alloc st val) (state-refocus st clg-add-datum val indirect))
(define (state-alloc-uids st count)
  (match st
    ((state foc cont env clg cur-uid)
     (let* ((new-uid (+ cur-uid count))
            (uids (sequence->list (in-range cur-uid new-uid))))
       (cons (state foc cont env clg new-uid) uids)))))
(define (state-focus! st focus)
  (match st
    ((state _ cont env clg cur-uid) (state focus cont env clg cur-uid))))
(define (state-focus+cont! st focus cont)
  (match st
    ((state _ _ env clg cur-uid) (state focus cont env clg cur-uid))))
(define (state-cont+env! st cont env)
  (match st
    ((state focus _ _ clg cur-uid) (state focus cont env clg cur-uid))))
(define (state-call! st body env)
  (let ((ret-cont (return-caller (state-env st) (state-cont st))))
        (state-focus! (state-cont+env! st ret-cont env) body)))
(define (state-clg-update st proc)
  (match st
    ((state focus cont env clg cur-uid)
     (let ((clg (proc clg)))
       (state focus cont env clg cur-uid)))))
(define (state-add-data st uids vals)
  (state-clg-update st (lambda (clg) (clg-add-data clg uids vals))))
(define (state-add-promise st uid prom)
  (state-clg-update st (lambda (clg) (clg-add-promise clg uid prom))))

(define (state-activate-val-a st atom)
  (match (state-cont st)
    ((ohc oh cont)
     (right (state-focus+cont! st (term-context-add oh atom) cont)))
    ((return-caller env cont) (right (state-cont+env! st cont env)))
    ((return-update puid env cont)
     (let ((st (state-add-promise st puid (kept atom))))
       (right (state-cont+env! st cont env))))
    ((halt) (left (format "halted on: ~a" (val-a-show atom))))))
(define (state-activate-val-c st val)
  (match val
    ((lam body _) (right (state-alloc st (lam body (state-env st)))))
    ((pair _ _) (right (state-alloc st val)))))
(define (state-activate-bound st idx)
  (do either-monad
    env = (state-env st)
    atom <- (maybe->either
              (format "invalid index ~a in env: ~s" env idx)
              (env-lookup env idx))
    focus = (val-a-context atom)
    (pure (state-focus! st focus))))
(define (state-activate-app st proc arg)
  (do either-monad
    (lam body env) <- (atom->lam (state-clg st) proc)
    env = (env-extend env arg)
    (pure (state-call! st body env))))
(define (state-activate-if-eq st sym0 sym1 true false)
  (do either-monad
    n0 <- (atom->sym sym0)
    n1 <- (atom->sym sym1)
    focus = (if (equal? n0 n1) true false)
    (pure (state-focus! st focus))))
(define (state-activate-pair-select select st atom)
  (do either-monad
    pair <- (atom->pair (state-clg st) atom)
    (pure (state-focus! st (val-a-context (select pair))))))
(define (state-activate-let-rec st defs body)
  (match-let* (((cons st uids) (state-alloc-uids st (length defs)))
               (renv (env-extends (state-env st) (map indirect uids)))
               (vals (map (lambda (dbody) (lam dbody renv)) defs))
               (st (state-add-data st uids vals)))
    (right (state-call! st body renv))))

(define (state-activate-term st term)
  (match term
    ((val-a x) (state-activate-val-a st x))
    ((val-c x) (state-activate-val-c st x))
    ((bound idx) (state-activate-bound st idx))
    ((app proc arg) (state-activate-app st proc arg))
    ((if-eq sym0 sym1 true false)
     (state-activate-if-eq st sym0 sym1 true false))
    ((pair-left x) (state-activate-pair-select pair-l st x))
    ((pair-right x) (state-activate-pair-select pair-r st x))
    ((let-rec defs body) (state-activate-let-rec st defs body))))

(define (state-step st)
  (match st
    ((state focus cont env clg cur-uid)
     (match focus
       ((term-context base finished pending)
        (match pending
          ((cons focus pending)
           (let ((tc-hole (term-context base finished pending)))
             (right (state focus (ohc tc-hole cont) env clg cur-uid))))
          ('() (state-activate-term st (context->term focus)))))))))

(define (state-delay st)
  (right (state-refocus st clg-add-promise
                        (delayed (state-focus st) (state-env st)) promise)))

(define (state-force st)
  (match st
    ((state focus cont env clg cur-uid)
     (match focus
       ((term-context base finished pending)
        (if (= 0 (length pending))
          (match (context->term focus)
            ((val-a (promise uid))
              (do either-monad
                prom <- (maybe->either
                          (format "(promise ~a) not found in catalog: ~s"
                                  clg uid)
                          (clg-get-promise clg uid))
                (match prom
                  ((delayed dtc denv)
                   (let ((update-cont (return-update uid env cont)))
                     (right (state dtc update-cont denv clg cur-uid))))
                  ((kept atom) (right (state-focus! st (val-a-context atom))))
                  ((broken bp)
                   (left (format "forced broken promise: ~s" bp))))))
            (term (left (format "expected promise but found: ~a"
                                (term-context-show focus)))))
          (left (format "expected promise but found: ~a"
                        (term-context-show focus)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; term/state conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (term->state term) (state-init (term->context term)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; visualization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (val-a-show val)
  (match val
    ((? void?)      "_")
    ((promise uid)  (format "?~a" uid))
    ((indirect uid) (format "@~a" uid))
    ((uno)          "{}")
    ((sym name)     (format "'~a'" name))
    (_ val)))
(define (val-c-show val)
  (match val
    ((lam body env) (format "(lam ~a ~a)"
                            (term-context-show body) (env-show env)))
    ((pair l r)     (format "{~a . ~a}" (val-a-show l) (val-a-show r)))))
(define (promise-show prom)
  (match prom
    ((delayed tc env) (format "(delayed ~a ~a)"
                              (term-context-show tc) (env-show env)))
    ((kept atom)      (format "(kept ~a)" (val-a-show atom)))
    ((broken x)
     (match x
       ((skolem uid)              (format "*~a*" uid))
       ((abandoned tc env)        (format "(abandoned ~a ~a)"
                                          (term-context-show tc)
                                          (env-show env)))
       ((sym-ineq names promises) (format "(sym-!= ~s ~s)" names promises))))))

(define (term-context-show tc)
  (match tc
    ((term-context base finished pending)
     (let* ((finished (map (lambda (atom) (format "<~a>" (val-a-show atom)))
                           finished))
            (term (context->term
                    (term-context
                      base (append (reverse (map term-context-show pending))
                                   finished) '()))))
       (match term
         ((val-a x)                (val-a-show x))
         ((val-c x)                (val-c-show x))
         ((bound idx)              (format "$~a" idx))
         ((app proc arg)           (format "(~a ~a)" proc arg))
         ((if-eq s0 s1 true false) (format "(if (~a = ~a) ~a ~a)" s0 s1
                                           (term-context-show true)
                                           (term-context-show false)))
         ((pair-left p)            (format "(pair-left ~a)" p))
         ((pair-right p)           (format "(pair-right ~a)" p))
         ((let-rec defs body)
          (format "(let-rec ~a; ~a)"
                  (string-join
                    (map (lambda (defstr) (format "(lam ~a [])" defstr))
                         (map term-context-show defs)) "; ")
                  (term-context-show body))))))))

(define (cont-group-show cont)
  (let group ((cont cont) (tcs '()))
    (match cont
      ((ohc oh cont)
       (let ((tc (format "~a" (term-context-show
                                (term-context-add oh (void))))))
         (group cont (cons tc tcs))))
      ((return-caller env cont)
       (cons
         (cons (reverse tcs) (string-append "return: " (env-show env)))
         (group cont '())))
      ((return-update uid env cont)
       (cons
         (cons (reverse tcs)
               (string-append (format "update ?~a: " uid) (env-show env)))
         (group cont '())))
      ((halt) (list (cons (reverse tcs) "halt!"))))))
(define (cont-show cont)
  (string-join (map (match-lambda
                      ((cons tcs delim)
                       (string-append (string-join tcs " |> "
                                                   #:before-first "[| "
                                                   #:after-last " |]")
                                      "\n" delim)))
                    (cont-group-show cont))
               "\n"))
(define (env-show env)
  (let ((strs (map (lambda (a) (format "~a" (val-a-show a))) env)))
    (string-join strs ", " #:before-first "[" #:after-last "]")))
(define (dict-show prefix val-show dict)
  (let* ((kvs (sort (dict->alist dict) (assoc-cmp >=)))
         (kvstrs (map (match-lambda
                        ((cons k (just v))
                         (format "~a~a: ~a" prefix k (val-show v))))
                      kvs)))
    (string-join kvstrs "\n")))
(define (clg-show clg)
  (string-join
    (list (dict-show "@" val-c-show (catalog-data clg))
          (dict-show "?" promise-show (catalog-promises clg))) "\n"))

(define (state-show st)
  (define border (make-string 79 #\=))
  (define divider (make-string 79 #\-))
  (match st
    ((state focus cont env clg cur-uid)
     (string-join
       (list
         border
         (term-context-show focus)
         divider
         (cont-show cont)
         divider
         (env-show env)
         divider
         (string-trim (clg-show clg))
         (format "~a" cur-uid)
         border)
       "\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; denotational interpretation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (denote-eval term) ((denote term) denote-env-empty))

(define (denote term)
  (match term
    ((val-a v) (denote-atom v))
    ((val-c v) (denote-compound v))
    ((bound idx) (lambda (env) (denote-env-lookup env idx)))
    ((app proc arg)
      (let ((dproc (denote proc)) (darg (denote arg)))
        (lambda (env)
          (let ((vproc (dproc env)) (varg (darg env)))
            (vproc varg)))))
    ((if-eq sym0 sym1 true false)
      (let ((ds0 (denote sym0)) (ds1 (denote sym1))
            (dt (denote true)) (df (denote false)))
        (lambda (env) (if (eq? (ds0 env) (ds1 env)) (dt env) (df env)))))
    ((pair-left x) (let ((dx (denote x))) (lambda (env) (car (dx env)))))
    ((pair-right x) (let ((dx (denote x))) (lambda (env) (cdr (dx env)))))
    ((let-rec defs body)
      (let ((dbody (denote body)) (ddefs (map denote-lam defs)))
        (lambda (env)
          (let* ((xenv (let loop ((env env) (ndefs (length ddefs)))
                         (if (> ndefs 0)
                           (loop (denote-env-extend env (void)) (- ndefs 1))
                           env)))
                 (vdefs (map (lambda (ddef) (ddef xenv)) ddefs)))
            (begin (denote-env-backfill xenv vdefs) (dbody xenv))))))))

(define denote-env-empty '())
(define (denote-env-lookup env idx) (unbox (list-ref env idx)))
(define (denote-env-extend env v) (cons (box v) env))
(define (denote-env-backfill env vs)
  (let loop ((env env) (vs (reverse vs)))
    (match vs
      ('() (void))
      ((cons v vs) (begin (set-box! (car env) v) (loop (cdr env) vs))))))
(define (denote-lam body)
  (let ((dbody (denote body)))
    (lambda (env) (lambda (arg) (dbody (denote-env-extend env arg))))))

(define (denote-atom v)
  (match v
    ((sym name) (lambda (env) name))
    ((uno) (lambda (env) '()))))
(define (denote-compound v)
  (match v
    ((lam body _) (denote-lam body))
    ((pair l r) (let ((dl (denote l)) (dr (denote r)))
                  (lambda (env) (cons (dl env) (dr env)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(data penv (penv (syntax vars)))
(define penv-empty (penv dict-empty '()))
(define (penv-syntax-add pe name op)
  (match pe
    ((penv syntax vars) (penv (dict-add syntax name op) vars))))
(define (penv-syntax-del pe name)
  (match pe
    ((penv syntax vars) (penv (dict-del syntax name) vars))))
(define (penv-syntax-get pe name) (dict-get (penv-syntax pe) name))
(define (penv-syntax-rename pe old new)
  (let ((check-vars (lambda (name msg)
                      (match (penv-vars-get pe name)
                        ((nothing) (right '()))
                        ((just _) (left msg))))))
    (do either-monad
      _ <- (check-vars old "cannot rename non-keyword")
      _ <- (check-vars new "rename-target already bound as a non-keyword")
      syn-old <- (maybe->either "cannot rename non-existent keyword"
                                (penv-syntax-get pe old))
      pe0 = (penv-syntax-del pe old)
      (pure (penv-syntax-add pe0 new syn-old)))))
(define (penv-vars-add pe name)
  (match pe
    ((penv syntax vars) (penv syntax (cons name vars)))))
(define (penv-vars-get pe name) (list-index (penv-vars pe) name))

(define (check-arity arity form)
  (if (equal? (length form) arity)
    (right '())
    (left (format "expected arity-~a form but found arity-~a form: ~s"
                  arity (length form) form))))
(define (check-symbol form)
  (if (symbol? form)
    (right '())
    (left (format "expected symbol but found: ~s" form))))

(define (parse pe form)
  (match form
    ('() (right (val-a (uno))))
    ((? symbol?) (parse-bound pe form))
    ((cons op rest) (parse-combination pe op form))
    (_ (left (format "cannot parse: ~s" form)))))
(define (parse-combination pe op form)
  ((if (symbol? op)
     (maybe-from parse-app (penv-syntax-get pe op))
     parse-app)
   pe form))

(define (map-parse pe form) (map-monad either-monad (curry parse pe) form))
(define ((parse-apply proc arity) pe form)
  (do either-monad
    _ <- (check-arity arity form)
    args <- (map-parse pe (cdr form))
    (pure (apply proc args))))
(define (parse-under pe param body)
  (do either-monad
    _ <- (check-symbol param)
    pe = (penv-vars-add pe param)
    (parse pe body)))

(define (parse-bound-index pe name)
  (do either-monad
    _ <- (check-symbol name)
    idx <- (maybe->either (format "unbound variable '~a'" name)
                          (penv-vars-get pe name))
    (pure idx)))
(define (parse-bound pe name)
  (do either-monad
    idx <- (parse-bound-index pe name)
    (pure (bound idx))))
(define (parse-app pe form)
  (do either-monad
    form <- (map-parse pe form)
    (cons proc args) = form
    (pure
      (let loop ((proc proc) (args args))
        (match args
          ('() proc)
          ((cons arg args) (loop (app proc arg) args)))))))
(define (parse-lam pe form)
  (do either-monad
    _ <- (check-arity 3 form)
    `(,_ ,name ,body) = form
    body <- (parse-under pe name body)
    (pure (val-c (lam body '())))))
(define (parse-let-rec pe form)
  (define-struct lrdef (name param body))
  (define (lr-def form)
    (do either-monad
      _ <- (check-arity 3 form)
      `(,name ,param ,body) = form
      _ <- (check-symbol name)
      _ <- (check-symbol param)
      (pure (lrdef name param body))))
  (do either-monad
    _ <- (check-arity 3 form)
    `(,_ ,defs ,body) = form
    defs <- (map-monad either-monad lr-def defs)
    names = (map lrdef-name defs)
    pe = (foldl (flip penv-vars-add) pe names)
    defs <- ((flip (curry map-monad either-monad)) defs
              (lambda (def)
                (parse-under pe (lrdef-param def) (lrdef-body def))))
    body <- (parse pe body)
    (pure (let-rec defs body))))
(define (parse-sym pe form)
  (do either-monad
    _ <- (check-arity 2 form)
    `(,_ ,name) = form
    (pure (val-a (sym name)))))
(define parse-if-eq (parse-apply if-eq 5))
(define parse-pair (parse-apply (compose1 val-c pair) 3))
(define parse-pair-left (parse-apply pair-left 2))
(define parse-pair-right (parse-apply pair-right 2))

(define penv-init
  (foldr (lambda (keyval pe) (apply (curry penv-syntax-add pe) keyval))
         penv-empty
         `((lam ,parse-lam)
           (sym ,parse-sym)
           (pair ,parse-pair)
           (if-eq ,parse-if-eq)
           (pair-left ,parse-pair-left)
           (pair-right ,parse-pair-right)
           (let-rec ,parse-let-rec))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-form form) (parse penv-init form))
(define (denote-form form)
  (do either-monad
    term <- (parse-form form)
    (pure (denote term))))
(define (denote-eval-form form)
  (do either-monad
    term <- (parse-form form)
    (pure (denote-eval term))))

; TODO: use racket's test facilities
(define tests
  `((((lam x (lam y ())) (sym one)) (sym two))
    (((lam x (lam y x)) (sym one)) (sym two))
    (((lam x (lam y y)) (sym one)) (sym two))
    (((lam x (lam y (pair-left (pair x y)))) (sym left)) (sym right))
    (((lam x (lam y (pair-right (pair x y)))) (sym left)) (sym right))
    (let-rec ((x arg (y arg)) (y arg arg)) (x ()))
    (let-rec ((x y (y x))) (x x))
    (let-rec ((x arg (y arg)) (y arg (x arg))) (x ()))
    (lam x x)
    (lam x (lam y x))
    ()
    (lam x (pair x x))
    (sym abc)
    (if-eq (sym abc) (sym def) () ())
    (x y)
    (pair () ())
    (pair () () ())))

(define parsed-tests (map (lambda (form) (parse penv-init form)) tests))
parsed-tests

(displayln "map-parse:")
(map-parse penv-init tests)

(displayln "eval:")
(denote-eval (right-x (list-ref parsed-tests 0)))
(denote-eval (right-x (list-ref parsed-tests 1)))
(denote-eval (right-x (list-ref parsed-tests 2)))
(denote-eval (right-x (list-ref parsed-tests 3)))
(denote-eval (right-x (list-ref parsed-tests 4))) ; 'right
(denote-eval (right-x (list-ref parsed-tests 5))) ; '()
;(denote-eval (right-x (list-ref parsed-tests 6))) ; infinite loop
;(denote-eval (right-x (list-ref parsed-tests 7))) ; another infinite loop

(define tstart (term->state (right-x (list-ref parsed-tests 4))))
tstart

(define (step-n st count (act (lambda (x) (void))))
  (if (= count 0) (right st)
    (begin (act st)
           (do either-monad
             next <- (state-step st)
             (step-n next (- count 1) act)))))

(define (step-n-show st count)
  (match (step-n st count (lambda (sti) (printf "~a\n\n\n" (state-show sti))))
    ((left msg) (begin (displayln msg) st))
    ((right st) (begin (displayln (state-show st)) st))))

(define tinfloop (term->state (right-x (list-ref parsed-tests 7))))
tinfloop

(define (state-interact st)
  (let loop ((st st))
    (printf "\n~a\n\n" (state-show st))
    (display "[s]tep,[d]elay,[f]orce,[c]omplete,[q]uit> ")
    (do either-monad
      st <- (match (read-line)
              ("s" (state-step st))
              ("d" (state-delay st))
              ("f" (state-force st))
              ("c" (step-n st -1))
              ("q" (left "quitting"))
              (_ (displayln "invalid choice") (right st)))
      (loop st))))

(displayln "")
(state-interact tstart)
(displayln "")
(state-interact tinfloop)

;(left "halted on: #(struct:sym right)")

;> (denote-eval (right-x (list-ref parsed-tests 0)))
;'()
;> (denote-eval (right-x (list-ref parsed-tests 1)))
;'one
;> (denote-eval (right-x (list-ref parsed-tests 2)))
;'two
;>

;> (define pe (penv-vars-add (penv-vars-add (penv-syntax-add penv-empty 'x 'y) 'z) 'w))
;> (penv-syntax-rename pe 'x 'y)
;(right
 ;(penv
   ;(dict (list (cons 'y (just 'y)) (cons 'x (nothing)) (cons 'x (just 'y))))
   ;'(w z)))
;>
