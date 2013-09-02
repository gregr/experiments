#lang racket
(require "util.rkt")
(provide (all-defined-out))

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
    ((term-context base finished pending)
     (let ((finished (append (reverse finished) (map context->term pending))))
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

(define (term-context-finish ohc val)
  (match ohc
    ((term-context base finished pending)
     (term-context base (cons val finished) pending))))
(define (term-context-pend ohc tc)
  (match ohc
    ((term-context base finished pending)
     (term-context base finished (cons tc pending)))))
(define (term-context-rewind tc)
  (match tc
    ((term-context base finished pending)
     (let ((finished (map val-a-context finished)))
       (term-context base '() (append (reverse finished) pending))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; small-step interpretation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(data cont
  (ohc (oh cont))
  (halt ())
  (return-caller (env cont))
  (return-update (puid env cont))
  (return-skolem (suid env cont)))

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
(define (env-append env0 env1) (append env1 env0))
(define (env-lookup env idx)
  (match env
    ('() (nothing))
    ((cons val env)
     (if (= idx 0) (just val) (env-lookup env (- idx 1))))))

(define (val-a-context atom) (term->context (val-a atom)))
(define (val-c-context vc) (term-context (val-c vc) '() '()))

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
     (right (state-focus+cont! st (term-context-finish oh atom) cont)))
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

(define (val-atomic-roots va)
  (match va
    ((promise uid) (set va))
    ((indirect uid) (set va))
    ((uno) set-empty)
    ((sym name) set-empty)))

(define (val-compound-roots env vc)
  (define (atom-or-null-roots aon)
    (if (null? aon) set-empty (val-atomic-roots aon)))
  (match vc
    ((lam body le)
     ((term-context-roots (env-extend (env-append env le) (uno))) body))
    ((pair l r) (set-unions (map atom-or-null-roots (list l r))))))

(define (promise-roots prom)
  (match prom
    ((delayed tc env) ((term-context-roots env) tc))
    ((kept atom) (val-atomic-roots atom))
    ((broken x)
     (match x
       ((skolem suid) set-empty)
       ((abandoned tc env) ((term-context-roots env) tc))
       (_ set-empty)))))

(define ((term-context-roots env) tc)
  (let ((common-roots
          (set-unions
            (append (map val-atomic-roots (term-context-finished tc))
                    (map (term-context-roots env) (term-context-pending tc)))))
        (special-roots
          (match (term-context-base tc)
            ((val-a x) (val-atomic-roots x))
            ((val-c x) (val-compound-roots env x))
            ((bound idx) (val-atomic-roots (just-x (env-lookup env idx))))
            ((if-eq _ _ true false)
             (set-unions
               (map (term-context-roots env) (list true false))))
            ((let-rec defs body)
             (let* ((env0 (env-extends env (make-list (length defs) (uno))))
                    (env1 (env-extend env0 (uno))))
               (set-unions
                 (cons ((term-context-roots env0) body)
                       (map (term-context-roots env1) defs)))))
            (_ set-empty))))
    (set-union common-roots special-roots)))

(define (term-context-unbind env threshold tc)
  (match tc
    ((term-context base finished pending)
     (term-context
       (match base
         ((val-c (lam body le))
          (val-c (lam (term-context-unbind env (+ threshold 1) body) le)))
         ((bound idx)
          (if (< idx threshold) base
            (val-a (just-x (env-lookup env (- idx threshold))))))
         ((if-eq s0 s1 true false)
          (if-eq s0 s1
                 (term-context-unbind env threshold true)
                 (term-context-unbind env threshold false)))
         ((let-rec defs body)
          (let* ((threshold (+ threshold (length defs)))
                 (defs (map (curry term-context-unbind env (+ threshold 1))
                            defs))
                 (body (term-context-unbind env threshold body)))
            (let-rec defs body)))
         (_ base))
       finished
       (map (curry term-context-unbind env threshold) pending)))))

(define (root-inline clg iop)
  (match iop
    ((indirect uid)
     (compound-inline (just-x (clg-get-datum clg uid))))
    ((promise uid) (promise-inline (just-x (clg-get-promise clg uid))))))
(define (compound-inline vc)
  (match vc
    ((pair l r) (term-context (val-c (pair '() '())) (list l r) '()))
    ((lam body env)
     (val-c-context (lam (term-context-unbind env 1 body) env-empty)))))
(define (promise-inline prom)
  (match prom
    ((delayed tc env) (term-context-unbind env 0 tc))
    ((kept atom) (val-a-context atom))
    ((broken x)
     (match x
       ((skolem _) (val-a-context (uno)))  ; should not happen
       ((abandoned tc env) (term-context-unbind env 0 tc))
       ((sym-ineq _ _) (val-a-context (uno)))))))  ; also should not happen

(define (focus+cont-collapse focus cont env)
  (define (cont-collapse focus cont env)
    (match cont
      ((ohc oh cont)
       (let* ((tc (term-context-unbind env 0 oh))
              (tc (term-context-pend tc focus))
              (tc (term-context-rewind tc)))
         (cont-collapse tc cont env)))
      ((halt) (list focus cont))
      ((return-caller env cont) (cont-collapse focus cont env))
      ((return-update puid env cont) (cont-collapse focus cont env))
      ((return-skolem puid _ _) (list focus cont))))
  (cont-collapse
    (term-context-rewind (term-context-unbind env 0 focus))
    cont env))

(define (roots->graph clg roots)
  (define (root-roots clg root)
    (match root
      ((indirect iuid) (val-compound-roots env-empty
                                           (just-x (clg-get-datum clg iuid))))
      ((promise puid) (promise-roots (just-x (clg-get-promise clg puid))))))
  (define (graphify src visited gr)
    (if (set-member? visited src) (list visited gr)
      (foldl (match-lambda**
               ((tgt (list visited gr))
                (graphify tgt visited (graph-add-edge gr src tgt))))
             (list (set-add visited src) (graph-add-src gr src))
             (set->list (root-roots clg src)))))
  (cadr (foldl (match-lambda**
                 ((src (list visited gr))
                  (graphify src visited gr)))
               (list (set) graph-empty)
               (set->list roots))))

(define (roots-all-sccs clg roots)
  (let ((gr (roots->graph clg roots))) (graph-topsort gr)))
(define (roots-relevant-sccs clg roots relevant)
  (let* ((gr (roots->graph clg roots))
         (sccs (graph-topsort gr))
         (relevant (sccs-relevant gr sccs relevant roots)))
    (sccs-filter sccs relevant)))
(define (root-bounds sccs)
  (define/match (add-bound root bmidx)
    ((root (list bm idx)) (list (hash-set bm root idx) (+ idx 1))))
  (foldr (lambda (scc bmidx) (foldr add-bound bmidx scc))
         (list (hash) 0) sccs))

(define (sccs->lets sccs body)
  (define (make-let scc body)
    (match scc
      ((list tc)
       (term-context (app '() '()) '()
                     (list (val-c-context (lam body env-empty))
                           (term-context-rewind tc))))
      (_
        (let ((defs (map (match-lambda
                           ((term-context (val-c (lam body _)) '() '()) body))
                         scc)))
          (term-context (let-rec defs body) '() '())))))
  (foldr make-let body sccs))

(define (term-context-rebind root->bound depth tc)
  (define (new-bound root) (bound (+ (hash-ref root->bound root) depth)))
  (define (recurse depth tc) (term-context-rebind root->bound depth tc))
  (match tc
    ((term-context base finished pending)
     (let ((base
             (match base
               ((val-a root)
                (match root
                  ((indirect _) (new-bound root))
                  ((promise _) (new-bound root))
                  (_ base)))
               ((val-c (lam body _))
                (val-c (lam (recurse (+ depth 1) body) env-empty)))
               ((if-eq s0 s1 true false)
                (apply (curry if-eq s0 s1)
                       (map (curry recurse depth) (list true false))))
               ((let-rec defs body)
                (let ((depth (+ depth (length defs))))
                  (let-rec (map (curry recurse (+ depth 1)) defs)
                           (recurse depth body))))
               (_ base)))
           (pending (map (curry recurse depth) pending)))
       (term-context base finished pending)))))

(define (state-rebuild st)
  (match st
    ((state focus cont env clg cur-uid)
     (match-let* (((list focus cont) (focus+cont-collapse focus cont env))
                  (roots ((term-context-roots env) focus))
                  ((list sccs env cont has-skolem)
                   (match cont
                     ((return-skolem puid env cont)
                      (list (roots-relevant-sccs clg roots
                                                 (set (promise puid)))
                            env cont #t))
                     ((halt)
                      (list (roots-all-sccs clg roots) env-empty cont #f))))
                  ((list root->bound max-idx) (root-bounds sccs))
                  (sccs (if has-skolem (cdr sccs) sccs))
                  (sccs (map (curry map (curry root-inline clg)) sccs))
                  (focus (sccs->lets sccs focus))
                  (focus (term-context-rebind root->bound (- max-idx) focus))
                  (focus (if has-skolem
                           (val-c-context (lam focus env-empty)) focus)))
       (state focus cont env clg cur-uid)))))

; TODO
;(define (state->term st)

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
                                (term-context-finish oh (void))))))
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
    (pure (val-c (lam body env-empty)))))
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
;;; interact
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (step-n st count (act (lambda (x) (void))))
  (if (= count 0) (right st)
    (begin (act st)
           (do either-monad
             next <- (state-step st)
             (step-n next (- count 1) act)))))

(define (state-interact st)
  (let loop ((st st))
    (printf "\n~a\n\n" (state-show st))
    (display "[s]tep,[d]elay,[f]orce,s[k]olemize,re[b]uild,[c]omplete,[q]uit> ")
    (do either-monad
      st <- (match (read-line)
              ("s" (state-step st))
              ("d" (state-delay st))
              ("f" (state-force st))
              ; TODO: transition to rebuilt state
              ("b" (begin (printf "rebuilt:\n~a\n***\n"
                                  (state-show (state-rebuild st))) (pure st)))
              ("c" (step-n st -1))
              ("q" (left "quitting"))
              (_ (displayln "invalid choice") (right st)))
      (loop st))))

(define (parse-default form) (parse penv-init form))

(define (interpret-port interpret return inp)
  (let loop ()
    (let ((form (read inp)))
      (if (eq? form eof) (right (void))
        (do either-monad
          term <- (parse-default form)
          (begin
            (return (interpret term))
            (loop)))))))

(define step-eval-port
  (curry interpret-port (compose1 state-interact term->state)
         (lambda (x) (displayln (left-x x)))))

(define denote-eval-port
  (curry interpret-port denote-eval
         (lambda (x) (displayln (format "~s" x)))))
