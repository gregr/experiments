#lang racket

(require "util.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; abstract syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(data value-bit
  (b-0 ())
  (b-1 ()))

(data term-value
  (bit  (b))
  (uno  ())
  (pair (l r))
  (bvar (idx))
  (lam  (body)))

(data term-action-2
  (pair-access ())
  (lam-apply   ()))

(data term
  (value     (v))
  (produce   (t))
  (action-2  (act t0 t1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; denotational semantics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (null-consume v) '())
(define (noisy-consume val) (displayln (format "produced: ~v" val)) '())

(define (denote-eval consume term) ((denote consume term) denote-env-empty))

(define (denote consume term)
  (match term
    ((value val)  (denote-value consume val))
    ((produce tm) (compose1 consume (denote consume tm)))
    ((action-2 act t0 t1)
     (let ((d0 (denote consume t0))
           (d1 (denote consume t1))
           (dact (denote-action-2 act)))
       (lambda (env) (dact (d0 env) (d1 env)))))))
(define (denote-action-2 act)
  (match act
    ((pair-access) (lambda (vbit vpair)
                     ((vector-ref (vector car cdr) vbit) vpair)))
    ((lam-apply)   (lambda (vproc varg) (vproc varg)))))
(define (denote-value consume val)
  (match val
    ((bit vb)   (denote-value-bit vb))
    ((uno)      (lambda (env) '()))
    ((pair l r) (let ((dl (denote-value consume l))
                      (dr (denote-value consume r)))
                  (lambda (env) (cons (dl env) (dr env)))))
    ((bvar idx) (lambda (env) (denote-env-lookup env idx)))
    ((lam body) (let ((db (denote consume body)))
                  (lambda (env)
                    (lambda (arg) (db (denote-env-extend env arg))))))))
(define (denote-value-bit vb)
  (match vb
    ((b-0) (lambda (env) 0))
    ((b-1) (lambda (env) 1))))

(define denote-env-empty            '())
(define (denote-env-lookup env idx) (list-ref env idx))
(define (denote-env-extend env v)   (cons v env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; operational semantics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pair-map f l r) (apply pair (map f (list l r))))
(define (action-2-map f act t0 t1)
  (apply (curry action-2 act) (map f (list t0 t1))))

(define (lift-bvars-value idx val)
  (match val
    ((pair l r)   (pair-map (curry lift-bvars-value idx) l r))
    ((bvar index) (if (< index idx) (bvar index) (bvar (+ index 1))))
    ((lam body)   (lam (lift-bvars (+ idx 1) body)))
    (_            val)))
(define (lift-bvars idx term)
  (match term
    ((value val)          (value (lift-bvars-value idx val)))
    ((produce tm)         (produce (lift-bvars idx tm)))
    ((action-2 act t0 t1) (action-2-map (curry lift-bvars idx) act t0 t1))))

(define (substitute-value idx val tv)
  (match tv
    ((pair l r)   (pair-map (curry substitute-value idx val) l r))
    ((bvar index) (if (< index idx) (bvar index)
                    (if (> index idx) (bvar (- index 1)) val)))
    ((lam body)   (lam (substitute (+ idx 1) (lift-bvars-value 0 val) body)))
    (_            tv)))
(define (substitute idx val term)
  (match term
    ((value tv)           (value (substitute-value idx val tv)))
    ((produce tm)         (produce (substitute idx val tm)))
    ((action-2 act t0 t1) (action-2-map
                            (curry substitute idx val) act t0 t1))))

(define/match (execute-action-2 act v0 v1)
  (((lam-apply)   (lam body)  _)            (just (substitute 0 v1 body)))
  (((pair-access) (bit (b-0)) (pair p0 p1)) (just (value p0)))
  (((pair-access) (bit (b-1)) (pair p0 p1)) (just (value p1)))
  ((_             _           _)            (nothing)))

(define (step term)
  (match term
    ((value _) (left (format "cannot step irreducible term: ~v" term)))
    ((produce tm-0)
     (do either-monad
       tm-1 <- (step tm-0)
       (pure (produce tm-1))))
    ((action-2 act (value v0) (value v1))
     (maybe->either (format "cannot step stuck term: ~v" term)
                    (execute-action-2 act v0 v1)))
    ((action-2 act (value val) t1-0)
     (do either-monad
       t1-1 <- (step t1-0)
       (pure (action-2 act (value val) t1-1))))
    ((action-2 act t0-0 t1)
     (do either-monad
       t0-1 <- (step t0-0)
       (pure (action-2 act t0-1 t1))))))

(define (step-safe term)
  (if (or (value? term) (produce? term) (action-2? term)) (step term)
    (left (format "cannot step non-term: ~v" term))))

(define step-complete (curry either-iterate step-safe))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; interaction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(data hole-term-value
  (hole-pair-l (r))
  (hole-pair-r (l))
  (hole-lam    ()))

(data hole-term
  (hole-value      ())
  (hole-produce    ())
  (hole-action-2-0 (act t1))
  (hole-action-2-1 (act t0)))

(variant (interact-context (holes focus)))

(define (interact-context-init term) (interact-context '() term))

(define (hole-fill hole subterm)
  (match hole
    ((hole-pair-l r)          (list 0 (pair subterm r)))
    ((hole-pair-r l)          (list 1 (pair l subterm)))
    ((hole-lam)               (list 0 (lam subterm)))
    ((hole-value)             (list 0 (value subterm)))
    ((hole-produce)           (list 0 (produce subterm)))
    ((hole-action-2-0 act t1) (list 0 (action-2 act subterm t1)))
    ((hole-action-2-1 act t0) (list 1 (action-2 act t0 subterm)))))

(define/match (hole-make idx focus)
  ((0 (pair l r))           (right (list (hole-pair-l r)          l)))
  ((1 (pair l r))           (right (list (hole-pair-r l)          r)))
  ((0 (lam body))           (right (list (hole-lam)               body)))
  ((0 (value val))          (right (list (hole-value)             val)))
  ((0 (produce tm))         (right (list (hole-produce)           tm)))
  ((0 (action-2 act t0 t1)) (right (list (hole-action-2-0 act t1) t0)))
  ((1 (action-2 act t0 t1)) (right (list (hole-action-2-1 act t0) t1)))
  ((_ _) (left (format "cannot select subterm ~a of: ~v" idx focus))))

(define (interact-ascend-index context)
  (match context
    ((interact-context holes focus)
     (match holes
       ((cons hole holes)
        (match-let (((list idx new-focus) (hole-fill hole focus)))
          (right (list idx (interact-context holes new-focus)))))
       (_ (left "no hole to fill"))))))
(define (interact-ascend context)
  (do either-monad
    (list _ context) <- (interact-ascend-index context)
    (pure context)))

(define (interact-descend-index idx context)
  (match context
    ((interact-context holes focus)
     (do either-monad
       (list hole new-focus) <- (hole-make idx focus)
       (pure (interact-context (cons hole holes) new-focus))))))
(define interact-descend (curry interact-descend-index 0))

(define ((interact-shift offset) context)
  (do either-monad
    (list idx context1) <- (interact-ascend-index context)
    (interact-descend-index (+ idx offset) context1)))
(define interact-shift-left (interact-shift -1))
(define interact-shift-right (interact-shift 1))

(define (interact-with-focus f context)
  (match context
    ((interact-context holes focus)
     (do either-monad
       new-focus <- (f focus)
       (pure (interact-context holes new-focus))))))

(define interact-step (curry interact-with-focus step-safe))
(define interact-complete
  (curry interact-with-focus (compose1 right step-complete)))

(define (interact-context-present context)
  (define (hole-present hole) (list-ref (hole-fill hole (void)) 1))
  (match context
    ((interact-context holes focus)
     (reverse (cons focus (map hole-present holes))))))

(variant (void-closure (is-value upenv)))
(define (unparse-void-closure upe term)
  (if (void? term) (void-closure #f upe)
    (unparse-orec unparse-void-closure unparse-value-void-closure upe term)))
(define (unparse-value-void-closure upe val)
  (if (void? val) (void-closure #t upe)
    (unparse-value-orec unparse-void-closure unparse-value-void-closure
                        upe val)))
(define (void-closures term)
  (match term
    ((? void-closure?) (list term))
    ((? list?)         (foldr append '() (map void-closures term)))
    (_                 '())))
(define (chain-unparse chain)
  (define (unparse-vc-chain term-or-value parents)
    (match (car (void-closures (car parents)))
      ((void-closure is-value upe)
       (cons (if is-value
               (unparse-value-void-closure upe term-or-value)
               (unparse-void-closure upe term-or-value))
             parents))))
  (cdr (reverse
         (foldl unparse-vc-chain (list (void-closure #f upenv-empty)) chain))))
(define (holed-substitute hole? replacement term)
  (match term
    ((? hole?) replacement)
    ((? list?) (map (curry holed-substitute hole? replacement) term))
    (_ term)))
(define (chain-unparse-void chain)
  (map (curry holed-substitute void-closure? (void)) (chain-unparse chain)))

(define (chain-show chain)
  (string-join
    (list ""
          (string-join (map pretty-string chain) "\n----------------\n\n")
          "")
    "\n================================\n\n"))

(define view-syntax-raw
  (compose1 chain-show interact-context-present))
(define view-syntax-0
  (compose1 chain-show chain-unparse-void interact-context-present))
(define (view-toggle current-view)
  (right (if (eq? view-syntax-raw current-view)
           view-syntax-0 view-syntax-raw)))

(variant (interact-state (view context history)))
(define (interact-state-viewcontext st)
  ((interact-state-view st) (interact-state-context st)))
(define/match (interact-state-lens-context st)
  (((interact-state view context history))
    (lens-result context (lambda (ctxt) (interact-state view ctxt history)))))
(define/match (interact-state-lens-view st)
  (((interact-state view context history))
    (lens-result view (lambda (view) (interact-state view context history)))))
(define/match (interact-state-lens-history st)
  (((interact-state view context history))
    (lens-result history (lambda (hist) (interact-state view context hist)))))

(define ((left-display-default default) result)
  (either-fold (lambda (msg) (display msg) default) identity result))

(define ((interact-safe lens) trans st)
  (right (:~ st (compose1 (left-display-default (:. st lens)) trans) lens)))
(define interact-safe-context
  (interact-safe interact-state-lens-context))
(define interact-safe-view
  (interact-safe interact-state-lens-view))

(define (interact-loop state)
  (let loop ((st state))
    (printf "~a" (interact-state-viewcontext st))
    (display "[hjkl](movement),[s]tep(count),[c]omplete,toggle-synta[x],[u]ndo,[q]uit> ")
    (do either-monad
      prev-st = st
      input = (read-line)
      st <- (match input
              ("h" (interact-safe-context interact-shift-left st))
              ("l" (interact-safe-context interact-shift-right st))
              ("j" (interact-safe-context interact-descend st))
              ("k" (interact-safe-context interact-ascend st))
              ((pregexp #px"^\\s*s\\s*(\\d+)?\\s*$" (list _ count))
               (let ((count (if count (string->number count) 1)))
                 (last (iterate
                         (compose1 (curry interact-safe-context interact-step)
                                   right-x)
                         (right st) count))))
              ("c" (interact-safe-context interact-complete st))
              ("x" (interact-safe-view view-toggle st))
              ("u" (match (:. st interact-state-lens-history)
                     ('() (displayln "nothing to undo!") (right st))
                     ((cons prev-state hist) (right prev-state))))
              ("q" (left "quitting"))
              (_ (displayln "invalid choice") (right st)))
      st = (if (equal? input "u") st
             (:~ st (curry cons prev-st) interact-state-lens-history))
      (loop st))))

(define (interact-with term)
  (interact-loop
    (interact-state view-syntax-0 (interact-context-init term) '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(variant (penv (syntax vars)))
(define penv-empty (penv dict-empty '()))
(define (penv-syntax-add pe name op)
  (match pe
    ((penv syntax vars) (penv (dict-add syntax name op) vars))))
(define (penv-syntax-del pe name)
  (match pe
    ((penv syntax vars) (penv (dict-del syntax name) vars))))
(define (penv-syntax-get pe name) (dict-get (penv-syntax pe) name))
(define (penv-syntax-rename pe old new)
  (define (check-vars name msg)
    (match (penv-vars-get pe name)
      ((nothing) (right '()))
      ((just _) (left msg))))
  (do either-monad
    _ <- (check-vars old "cannot rename non-keyword")
    _ <- (check-vars new "rename-target already bound as a non-keyword")
    syn-old <- (maybe->either "cannot rename non-existent keyword"
                              (penv-syntax-get pe old))
    pe0 = (penv-syntax-del pe old)
    (pure (penv-syntax-add pe0 new syn-old))))
(define penv-syntax-op-empty '||)
(define (penv-syntax-op-get pe op)
  (match (penv-syntax-get pe op)
    ((just result) (right result))
    ((nothing) (maybe->either (format "invalid operator: ~s" op)
                              (penv-syntax-get pe penv-syntax-op-empty)))))
(define (penv-vars-add pe name)
  (match pe
    ((penv syntax vars) (penv syntax (cons name vars)))))
(define (penv-vars-get pe name) (list-index (penv-vars pe) name))

(define v-uno (value (uno)))
(define v-0 (value (bit (b-0))))
(define v-1 (value (bit (b-1))))
(define new-lam-apply (curry action-2 (lam-apply)))
(define new-pair-access (curry action-2 (pair-access)))
(define/match (new-pair l r)
  (((value vl) (value vr)) (right (value (pair vl vr))))
  ((_ _) (left (format "pair arguments must be values: ~v ~v" l r))))

(define (check-arity arity form)
  (if (equal? (length form) arity)
    (right '())
    (left (format "expected arity-~a form but found arity-~a form: ~s"
                  arity (length form) form))))
(define (check-symbol form)
  (if (symbol? form)
    (right '())
    (left (format "expected symbol but found: ~s" form))))
(define (parse-combination pe op form)
  (do either-monad
    proc <- (penv-syntax-op-get pe op)
    (proc pe form)))
(define ((map-parse parse) pe form)
  (map-monad either-monad (curry parse pe) form))
(define (((parse-apply map-parse) proc arity) pe form)
  (do either-monad
    _ <- (check-arity arity form)
    args <- (map-parse pe (cdr form))
    (pure (apply proc args))))
(define ((parse-under parse) pe params body)
  (do either-monad
    _ <- (map-monad either-monad check-symbol params)
    pe = (foldl (flip penv-vars-add) pe params)
    (parse pe body)))
(define (parse-bvar pe name)
  (do either-monad
    _ <- (check-symbol name)
    _ <- (if (equal? name '_) (left "unexpected reference to _") (right name))
    idx <- (maybe->either (format "unbound variable '~a'" name)
                          (penv-vars-get pe name))
    (pure (value (bvar idx)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parsing (syntax-1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-1 pe form)
  (match form
    ('() (right uno-1))
    ((? symbol?) (parse-bvar pe form))
    ((cons op rest) (parse-combination pe op form))
    (_ (left (format "cannot parse: ~s" form)))))

(define map-parse-1 (map-parse parse-1))
(define parse-under-1 (parse-under parse-1))

(define (new-lam-apply-1 proc arg)
  (new-lam-apply (new-lam-apply lam-unwrap proc) arg))
(define (new-lam-1 arg-name body)
  (new-lam-apply
    (new-lam-apply lam-wrap (value (_sym arg-name)))
    (value (lam body))))

(define (parse-lam-apply-1 pe form)
  (do either-monad
    form <- (map-parse-1 pe form)
    (cons proc args) = form
    (pure
      (let loop ((proc proc) (args args))
        (match args
          ('() proc)
          ((cons arg args) (loop (new-lam-apply-1 proc arg) args)))))))
(define (parse-lam-1 pe form)
  (do either-monad
    _ <- (check-arity 3 form)
    `(,_ ,names ,body) = form
    _ <- (if (>= (length names) 1) (right (void))
           (left (format "lam must include at least one parameter: ~v" form)))
    body <- (parse-under-1 pe names body)
    (pure (foldr (lambda (arg-name body) (new-lam-1 arg-name body))
                 body names))))

(define penv-init-1
  (foldr (lambda (keyval pe) (apply (curry penv-syntax-add pe) keyval))
         penv-empty
         `((,penv-syntax-op-empty ,parse-lam-apply-1)
           (lam ,parse-lam-1)
           ; TODO:
           ; syntax-lam?
           ; macros?
           )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parsing (syntax-0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-0 pe form)
  (match form
    ('() (right v-uno))
    ((? integer?) (parse-integer-0 pe form))
    ((? symbol?) (parse-bvar pe form))
    ((cons op rest) (parse-combination pe op form))
    (_ (left (format "cannot parse: ~s" form)))))

(define map-parse-0 (map-parse parse-0))
(define parse-apply-0 (parse-apply map-parse-0))
(define parse-under-0 (parse-under parse-0))

(define parse-produce-0 (parse-apply-0 produce 2))
(define (parse-lam-apply-0 pe form)
  (do either-monad
    form <- (map-parse-0 pe form)
    (cons proc args) = form
    (pure
      (let loop ((proc proc) (args args))
        (match args
          ('() proc)
          ((cons arg args) (loop (new-lam-apply proc arg) args)))))))
(define (parse-lam-0 pe form)
  (do either-monad
    _ <- (check-arity 3 form)
    `(,_ ,names ,body) = form
    _ <- (if (>= (length names) 1) (right (void))
           (left (format "lam must include at least one parameter: ~v" form)))
    body <- (parse-under-0 pe names body)
    (pure (foldr (lambda (_ body) (value (lam body))) body names))))
(define parse-pair-access-0 (parse-apply-0 new-pair-access 3))
(define (parse-pair-0 pe form)
  (do either-monad
    _ <- (check-arity 3 form)
    `(,_ ,fl ,fr) = form
    l <- (parse-0 pe fl)
    r <- (parse-0 pe fr)
    (new-pair l r)))
(define (parse-as-thunk-0 pe form) (parse-0 pe `(lam (_) ,form)))
(define (parse-tuple-0 pe form)
  (define/match (tuple-value term)
    (((value val)) (right val))
    ((_)           (left (format "tuple elements must be values: ~v" form))))
  (do either-monad
    `(,_ . ,felems) = form
    velems <- (map-parse-0 pe felems)
    elems <- (map-monad either-monad tuple-value velems)
    (pure (value (tuple-encode elems)))))

; derived syntax
(define (parse-if-0-0 pe form)
  (do either-monad
    _ <- (check-arity 4 form)
    `(,_ ,fcnd ,fzero ,fone) = form
    cnd <- (parse-0 pe fcnd)
    zero <- (parse-as-thunk-0 pe fzero)
    one <- (parse-as-thunk-0 pe fone)
    alts <- (new-pair zero one)
    (pure (new-lam-apply (new-pair-access cnd alts) v-uno))))
(define parse-pair-l-0 (parse-apply-0 (curry new-pair-access v-0) 2))
(define parse-pair-r-0 (parse-apply-0 (curry new-pair-access v-1) 2))

; TODO: encode human-friendly numerals and symbols
(define (parse-integer-0 pe form)
  (match form
    (0 (right v-0))
    (1 (right v-1))
    (_ (left (format "expected 0 or 1 but found: ~s" form)))))

(define (parse-fixpoint-0 pe form)
  (do either-monad
    _ <- (check-arity 3 form)
    `(,_ ,names ,body) = form
    _ <- (if (>= (length names) 1) (right (void))
           (left (format "fix must include at least one parameter: ~v" form)))
    body <- (parse-under-0 pe names body)
    proc = (foldr (lambda (_ body) (value (lam body))) body names)
    (pure (new-lam-apply (value Y-combinator) proc))))

(define penv-init-0
  (foldr (lambda (keyval pe) (apply (curry penv-syntax-add pe) keyval))
         penv-empty
         `((,penv-syntax-op-empty ,parse-lam-apply-0)
           (produce ,parse-produce-0)
           (lam ,parse-lam-0)
           (pair ,parse-pair-0)
           (pair-access ,parse-pair-access-0)
           (pair-l ,parse-pair-l-0)
           (pair-r ,parse-pair-r-0)
           (tuple ,parse-tuple-0)
           (if-0 ,parse-if-0-0)
           (fix ,parse-fixpoint-0)
           )))

(define Y-combinator
  (value-v (right-x (parse-0 penv-init-0
                           `(lam (f) ((lam (d) (d d))
                                      (lam (x a) (f (x x) a))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; unparsing (syntax-0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(variant (upenv (vars)))
(define upenv-empty (upenv '()))
; TODO: use more of the alphabet
(define (upenv-next-name vars)
  (string->symbol (format "x~a" (length vars))))
(define (upenv-free-name upe idx)
  (string->symbol (format "free~a" (- idx (length (upenv-vars upe))))))
(define (upenv-vars-add upe)
  (match upe
    ((upenv vars)
     (let ((next-name (upenv-next-name vars)))
       (cons next-name (upenv (cons next-name vars)))))))
(define (upenv-vars-get upe idx)
  (let ((vars (upenv-vars upe)))
    (if (< idx (length vars)) (list-ref vars idx) (upenv-free-name upe idx))))

(define (unparse upe term)
  (unparse-orec unparse unparse-value upe term))
(define (unparse-value upe term)
  (unparse-value-orec unparse unparse-value upe term))
(define (unparse-orec unparse unparse-value upe term)
  (define (thunk-form? val)
    (define (mentions-bvar-value? idx val)
      (match val
        ((bvar index) (equal? index idx))
        ((pair v0 v1)
         (or (mentions-bvar-value? idx v0) (mentions-bvar-value? idx v1)))
        ((lam body) (mentions-bvar? (+ idx 1) body))
        (_ #f)))
    (define (mentions-bvar? idx term)
      (match term
        ((value val) (mentions-bvar-value? idx val))
        ((produce tm) (mentions-bvar? idx tm))
        ((action-2 _ t0 t1)
         (or (mentions-bvar? idx t0) (mentions-bvar? idx t1)))))
    (match val
      ((lam body) (not (mentions-bvar? 0 body)))
      (_ #f)))
  (define (unparse-thunk upe thunk)
    (match (unparse-value upe thunk)
      (`(lam (,_)            ,body) body)
      (`(lam ,(cons x names) ,body) `(lam ,names ,body))))
  (match term
    ((value v) (unparse-value upe v))
    ((produce tm) `(produce ,(unparse upe tm)))
    ((action-2 (lam-apply)
      (action-2 (pair-access) tcnd
        (value (pair (? thunk-form? alt-0) (? thunk-form? alt-1))))
      (value (uno)))
     `(if-0 ,(unparse upe tcnd)
            ,(unparse-thunk upe alt-0) ,(unparse-thunk upe alt-1)))
    ((action-2 (lam-apply)
               (value (? (curry equal? Y-combinator)))
               (value (lam body)))
     (match (unparse-value upe (lam body))
       (`(lam ,names ,body) `(fix ,names ,body))))
    ((action-2 (lam-apply) tproc targ)
     (unparse-application unparse upe tproc (list targ)))
    ((action-2 (pair-access) (value (bit b)) tpair)
     (list (match b ((b-0) 'pair-l) ((b-1) 'pair-r)) (unparse upe tpair)))
    ((action-2 act t0 t1)
     (unparse-action-2 act (unparse upe t0) (unparse upe t1)))))
(define (unparse-application unparse upe tproc targs)
  (match tproc
    ((and (action-2 (lam-apply) tproc targ)
          ; having to do this is terrible
          (not (action-2 (lam-apply)
                         (value (? (curry equal? Y-combinator)))
                         (value (lam body)))))
     (unparse-application unparse upe tproc (cons targ targs)))
    (_ (map (curry unparse upe) (cons tproc targs)))))
(define (unparse-action-2 act f0 f1)
  (match act
    ((pair-access) `(pair-access ,f0 ,f1))))
(define (unparse-value-orec unparse unparse-value upe val)
  (match val
    ((bit b)    (unparse-value-bit b))
    ((uno)      '())
    ((pair l r)
     (let ((fl (unparse-value upe l)))
      (match (unparse-value upe r)
        ('()               `(tuple ,fl))
        (`(tuple . ,elems) `(tuple . ,(cons fl elems)))
        (fr                `(pair ,fl ,fr)))))
    ((bvar idx) (upenv-vars-get upe idx))
    ((lam body)
     (match-let (((cons new-name new-upe) (upenv-vars-add upe)))
       (match (unparse new-upe body)
         (`(lam ,names ,body) (list 'lam (cons new-name names) body))
         (body                (list 'lam (list new-name) body)))))))
(define (unparse-value-bit vb)
  (match vb
    ((b-0) 0)
    ((b-1) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; data representation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tuple
(define tuple-nil         (uno))
(define (tuple-cons x xs) (pair x xs))
(define (tuple? val)
  (match val
    ((pair _ xs) (tuple? xs))
    ((uno)       #t)
    (_           #f)))
(define (tuple-foldr f acc tup)
  (match tup
    ((uno)       acc)
    ((pair x xs) (f x (tuple-foldr f acc xs)))))
(define (tuple-foldl f acc tup)
  (match tup
    ((uno)       acc)
    ((pair x xs) (tuple-foldl f (f x acc) xs))))

(define (tuple-length tup) (tuple-foldl (lambda (_ len) (+ len 1)) 0 tup))

(define (tuple-encode-revappend xs tup)
  (match xs
    ('()         tup)
    ((cons y ys) (tuple-encode-revappend ys (tuple-cons y tup)))))
(define (tuple-encode xs) (tuple-encode-revappend (reverse xs) tuple-nil))
(define (tuple-decode tup) (tuple-foldr cons '() tup))
(define (tuple-pad len val tup)
  (tuple-encode-revappend (make-list (- len (tuple-length tup)) val) tup))

;; nat
(define (nat-encode n)
  (tuple-pad (+ n 1) (bit (b-1)) (tuple-encode (list (bit (b-0))))))
(define (nat-decode nat) (- (tuple-length nat) 1))

;; bit
(define (bit-encode bool)
  (if bool (bit (b-1)) (bit (b-0))))
(define (bit-decode b)
  (match b
    ((bit (b-0)) #f)
    ((bit (b-1)) #t)))

;; bits
(define (bits-encode n)
  (let loop ((acc '()) (n n))
    (if (equal? 0 n)
      (tuple-encode acc)
      (loop (cons (bit-encode (odd? n)) acc)
            (floor (/ n 2))))))
(define (bits-decode bits)
  (tuple-foldl
    (lambda (b total)
      (+ (* 2 total) (if (bit-decode b) 1 0)))
    0 bits))
(define (bits-pad n bits)
  (tuple-pad n (bit (b-0)) bits))
(define (bits-count bits) (tuple-length bits))
(define (bits-required n) (bits-count (bits-encode (- n 1))))

;; length-encoding for tuple-like values
(define (length-encoded tup) (pair (nat-encode (tuple-length tup)) tup))

;; symbol
(variant (symbol-entry (repr sub-table)))
(define (symbol-repr uid bitwidth) (bits-pad bitwidth (bits-encode uid)))

(variant (symbol-table (capacity mapping rev-mapping next-uid)))
(define (symbol-table-empty capacity)
  (symbol-table capacity dict-empty dict-empty 0))
(define (symbol-table-bitwidth table)
  (bits-required (symbol-table-capacity table)))

(define (symbol-table-get table key)
  (dict-get (symbol-table-mapping table) key))
(define (symbol-table-encode table key)
  (match (symbol-table-get table key)
    ((just entry) (symbol-entry-repr entry))
    ((nothing)    (error (format "cannot encode key '~v' with table: ~v"
                                 key table)))))
(define (symbol-table-decode table symbol)
  (just-x (dict-get (symbol-table-rev-mapping table) (bits-decode symbol))))
(define (symbol-table-add table key max-children)
  (match table
    ((symbol-table capacity mapping rev-mapping next-uid)
     (if (equal? capacity next-uid)
       (error (format
                "cannot add symbol '~v' to table at max capacity: ~v"
                key table))
       (void))
     (let ((entry (symbol-entry
                    (symbol-repr next-uid (symbol-table-bitwidth table))
                    (symbol-table-empty max-children))))
       (match (symbol-table-get table key)
         ((nothing)
          (symbol-table capacity
                        (dict-add mapping key entry)
                        (dict-add rev-mapping next-uid key)
                        (+ 1 next-uid)))
         (_ (error (format "symbol already added for key: ~v" key))))))))

(define ((symbol-table-lens key) table)
  (match table
    ((symbol-table capacity mapping rev-mapping next-uid)
     (let* ((entry (just-x (symbol-table-get table key)))
            (next-table (symbol-entry-sub-table entry)))
       (match entry
         ((symbol-entry repr sub-table)
          (define (rebuild new-table)
            (symbol-table
              capacity
              (dict-add mapping key (symbol-entry repr new-table))
              rev-mapping
              next-uid))
          (lens-result next-table rebuild)))))))
(define (symbol-table-lens* keys) (:o (map symbol-table-lens keys)))

(define ((symbol-table-decode-lens symbol) table)
  ((symbol-table-lens (symbol-table-decode table symbol)) table))

(define (symbol-table-encode* table keys)
  (symbol-table-encode
    (:. table (symbol-table-lens* (list-init keys)))
    (last keys)))
(define (symbol-table-decode* table keys symbol)
  (symbol-table-decode
    (:. table (symbol-table-lens* keys))
    symbol))
(define (symbol-table-add* table keys max-children)
  (:~ table
      (lambda (tgt-table)
        (symbol-table-add tgt-table (last keys) max-children))
      (symbol-table-lens* (list-init keys))))

(define (symbol-table-encode** table keys tgt-keys)
  (map (curry symbol-table-encode* table)
       (map (curry append keys) (cdr (list-inits tgt-keys)))))
(define (symbol-table-decode** table keys symbols)
  (match symbols
    ('() '())
    ((cons symbol symbols)
     (let ((next-key (symbol-table-decode* table keys symbol)))
       (cons next-key
             (symbol-table-decode** table (append keys (list next-key))
                                    symbols))))))

(define symbol-capacity-default 256)  ; TODO: arbitrary-precision encoding?
(define *symbol-table* (box (symbol-table-empty symbol-capacity-default)))

(define (symbol-encode key)
  (let* ((table (unbox *symbol-table*))
         (table (match (symbol-table-get table key)
                  ((just _) table)
                  ((nothing) (symbol-add key) (unbox *symbol-table*)))))
    (symbol-table-encode table key)))
(define (symbol-decode symbol)
  (symbol-table-decode (unbox *symbol-table*) symbol))
(define (symbol-add key (max-children 0))
  (set-box! *symbol-table*
            (symbol-table-add (unbox *symbol-table*) key max-children)))

(define (symbol-encode* keys)
  (symbol-table-encode* (unbox *symbol-table*) keys))
(define (symbol-decode* keys symbol)
  (symbol-table-decode* (unbox *symbol-table*) keys symbol))
(define (symbol-add* keys (max-children 0))
  (set-box! *symbol-table*
            (symbol-table-add* (unbox *symbol-table*) keys max-children)))

(define (symbol-encode** keys tgt-keys)
  (symbol-table-encode** (unbox *symbol-table*) keys tgt-keys))
(define (symbol-decode** keys symbols)
  (symbol-table-decode** (unbox *symbol-table*) keys symbols))

(define (syntax-0-le val)
  (unparse upenv-empty (value (length-encoded val))))

;; tagged data
(define (basic-tag-def name (namespace 1))
  (syntax-0-le (symbol-encode (list namespace name))))

(define sym-tag     (basic-tag-def 'sym))
(define lam-tag     (basic-tag-def 'lam))
(define bit-tag     (basic-tag-def 'bit))
(define uno-tag     (basic-tag-def 'uno))
(define pair-tag    (basic-tag-def 'pair))

(define error-effect-tag      (basic-tag-def 'error  2))
(define gen-sym-effect-tag    (basic-tag-def 'gensym 2))

(define (_sym name) (symbol-encode (list 0 name)))
(define (sym name) `(tagged ,sym-tag ,(syntax-0-le (_sym name))))

;; bootstrapping
(define (let-module defs body)
  (foldr (lambda (def body)
           (match def
             (`(,name ,expr) `((lam (,name) ,body) ,expr))))
         body defs))

(define (std prog)
  (let-module `(
    (tagged       (lam (tag datum) (pair tag datum)))
    (tagged-tag   (lam (td) (pair-l td)))
    (tagged-datum (lam (td) (pair-r td)))

    (error        (lam (val) (produce (tagged ,error-effect-tag val))))
    (gen-sym      (lam (sym-name parent)
                    (produce (tagged ,gen-sym-effect-tag
                                     (pair sym-name parent)))))

    (pair-cons    (lam (l r) (pair l r)))
    (size-empty   (pair 0 ()))
    (size-inc     (lam (sz) (pair 1 sz)))
    (size-dec     (lam (sz) (pair-r sz)))
    (tuple-size   (lam (tup) (pair-l tup)))
    (tuple-data   (lam (tup) (pair-r tup)))
    (tuple-empty  (pair size-empty ()))
    (tuple-cons   (lam (val tup) (pair-cons (size-inc (tuple-size tup))
                                            (pair-cons val (tuple-data tup)))))
    (tuple-first  (lam (tup) (pair-l (tuple-data tup))))
    (tuple-rest   (lam (tup) (pair-cons (size-dec (tuple-size tup))
                                        (pair-r (tuple-data tup)))))

    (bit-eq?              (lam (bta btb) (if-0 bta (if-0 btb 0 1) (if-0 btb 1 0))))
    (size-eq?             (fix (size-eq? sa sb)
                            (if-0 (bit-eq? (pair-l sa) (pair-l sb))
                              (if-0 (pair-l sa) 0
                                (size-eq? (pair-r sa) (pair-r sb)))
                              1)))
    (bits-unsized-eq?     (fix (bits-unsized-eq? sz ba bb)
                            (if-0 (pair-l sz)
                              0
                              (if-0 (bit-eq? (pair-l ba) (pair-l bb))
                                (bits-unsized-eq? (pair-r sz) (pair-r ba) (pair-r bb))
                                1))))
    (bits-eq?             (lam (ba bb)
                            (if-0 (size-eq? (tuple-size ba) (tuple-size bb))
                              (bits-unsized-eq? (tuple-size ba) (tuple-data ba) (tuple-data bb))
                              (error ,(sym 'bitsize-mismatch)))))

    (bits-assoc   (fix (bits-assoc default assocs bits)
                    (if-0 (size-eq? size-empty (tuple-size assocs)) default
                      ((lam (assoc)
                        (if-0 (bits-eq? bits (pair-l assoc)) (pair-r assoc)
                          (bits-assoc default
                                      (tuple-rest assocs)
                                      bits)))
                       (tuple-first assocs)))))

    (tagged-with? (lam (tag td) (bits-eq? tag (tagged-tag td))))

    (sym?   (tagged-with? ,sym-tag))
    (lam?   (tagged-with? ,lam-tag))
    (bit?   (tagged-with? ,bit-tag))
    (uno?   (tagged-with? ,uno-tag))
    (pair?  (tagged-with? ,pair-tag))

    (pair-x      (lam (accessor p)
                   (if-0 (pair? p) (accessor (tagged-datum p))
                     (error ,(sym 'expected-pair)))))
    (lam-wrap    (lam (arg-name lam) (tagged ,lam-tag (pair lam arg-name))))
    (_lam-unwrap (lam (wrapped-lam) (pair-l (tagged-datum wrapped-lam))))
    (lam-unwrap  (lam (lm)
                   (if-0 (lam? lm) (_lam-unwrap lm)
                     (error ,(sym 'expected-lam)))))

    (pred-wrap (lam (pred) (lam-wrap () (lam (val) (tagged ,bit-tag (pred val))))))

    (1-sym?  (pred-wrap sym?))
    (1-lam?  (pred-wrap lam?))
    (1-bit?  (pred-wrap bit?))
    (1-uno?  (pred-wrap uno?))
    (1-pair? (pred-wrap pair?))

    (1-sym-eq?      (lam-wrap () (lam (sa) (lam-wrap () (lam (sb)
                      (if-0 (sym? sa)
                        (if-0 (sym? sb)
                          (tagged ,bit-tag
                            (bits-eq? (tagged-datum sa) (tagged-datum sb)))
                          (error ,(sym 'expected-sym-rhs)))
                        (error ,(sym 'expected-sym-lhs))))))))
    (1-uno          (tagged ,uno-tag ()))
    (1-0b           (tagged ,bit-tag 0))
    (1-1b           (tagged ,bit-tag 1))
    (1-pair         (lam-wrap () (lam (l) (lam-wrap () (lam (r)
                      (tagged ,pair-tag (pair l r)))))))
    (1-pair-access  (lam-wrap () (lam (bt) (lam-wrap () (lam (pr)
                      (if-0 (bit? bt)
                        (pair-access (tagged-datum bt) (pair-x (lam (p) p)))
                        (error ,(sym 'expected-bit))))))))

    (1-produce      (lam-wrap () (lam (val) (produce val))))

    (1-error        (lam-wrap () error))
    (1-gen-sym      (lam-wrap () (lam (sym-name) (lam-wrap () (lam (parent)
                      (gen-sym sym-name parent))))))
    )
    prog))

(define interact-with-0
  (compose1 interact-with right-x (curry parse-0 penv-init-0)))

(define std-0-output (tuple-decode (value-v
  (step-complete (right-x (parse-0 penv-init-0 (std `(tuple
    lam-wrap lam-unwrap 1-uno
    1-sym? 1-lam? 1-bit? 1-uno? 1-pair?
    1-sym-eq?
    1-0b 1-1b
    1-pair 1-pair-access
    1-produce
    1-error
    1-gen-sym
  ))))))))

(match-define (cons lam-wrap (cons lam-unwrap (cons uno-1 std-1-input)))
  (map value std-0-output))

(define (std-1 prog)
  (let ((proc (right-x (parse-1 penv-init-1
          `(lam (sym? lam? bit? uno? pair?
                 sym-eq? 0b 1b pair pair-access
                 produce error gen-sym)
              ,prog)))))
    (foldl (lambda (arg proc) (new-lam-apply-1 proc arg)) proc
           std-1-input)))
