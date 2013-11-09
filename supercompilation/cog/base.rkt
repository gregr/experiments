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
  (action-2  (act t0 t1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; denotational semantics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (denote-eval term) ((denote term) denote-env-empty))

(define (denote term)
  (match term
    ((value val) (denote-value val))
    ((action-2 act t0 t1)
     (let ((d0 (denote t0)) (d1 (denote t1)) (dact (denote-action-2 act)))
       (lambda (env) (dact (d0 env) (d1 env)))))))
(define (denote-action-2 act)
  (match act
    ((pair-access) (lambda (vbit vpair)
                     ((vector-ref (vector car cdr) vbit) vpair)))
    ((lam-apply)   (lambda (vproc varg) (vproc varg)))))
(define (denote-value val)
  (match val
    ((bit vb)   (denote-value-bit vb))
    ((uno)      (lambda (env) '()))
    ((pair l r) (let ((dl (denote-value l)) (dr (denote-value r)))
                  (lambda (env) (cons (dl env) (dr env)))))
    ((bvar idx) (lambda (env) (denote-env-lookup env idx)))
    ((lam body) (let ((db (denote body)))
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
  (if (or (value? term) (action-2? term)) (step term)
    (left (format "cannot step non-term: ~v" term))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; interaction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(data hole-term-value
  (hole-pair-l (r))
  (hole-pair-r (l))
  (hole-lam    ()))

(data hole-term
  (hole-value      ())
  (hole-action-2-0 (act t1))
  (hole-action-2-1 (act t0)))

(variant (interact-state (holes focus)))

(define (interact-state-init term) (interact-state '() term))

(define (hole-fill hole subterm)
  (match hole
    ((hole-pair-l r)          (list 0 (pair subterm r)))
    ((hole-pair-r l)          (list 1 (pair l subterm)))
    ((hole-lam)               (list 0 (lam subterm)))
    ((hole-value)             (list 0 (value subterm)))
    ((hole-action-2-0 act t1) (list 0 (action-2 act subterm t1)))
    ((hole-action-2-1 act t0) (list 1 (action-2 act t0 subterm)))))

(define/match (hole-make idx focus)
  ((0 (pair l r))           (right (list (hole-pair-l r)          l)))
  ((1 (pair l r))           (right (list (hole-pair-r l)          r)))
  ((0 (lam body))           (right (list (hole-lam)               body)))
  ((0 (value val))          (right (list (hole-value)             val)))
  ((0 (action-2 act t0 t1)) (right (list (hole-action-2-0 act t1) t0)))
  ((1 (action-2 act t0 t1)) (right (list (hole-action-2-1 act t0) t1)))
  ((_ _) (left (format "cannot select subterm ~a of: ~v" idx focus))))

(define (interact-ascend-index state)
  (match state
    ((interact-state holes focus)
     (match holes
       ((cons hole holes)
        (match-let (((list idx new-focus) (hole-fill hole focus)))
          (right (list idx (interact-state holes new-focus)))))
       (_ (left "no hole to fill"))))))
(define (interact-ascend state)
  (do either-monad
    (list _ state) <- (interact-ascend-index state)
    (pure state)))

(define (interact-descend-index idx state)
  (match state
    ((interact-state holes focus)
     (do either-monad
       (list hole new-focus) <- (hole-make idx focus)
       (pure (interact-state (cons hole holes) new-focus))))))
(define interact-descend (curry interact-descend-index 0))

(define ((interact-shift offset) state)
  (do either-monad
    (list idx state1) <- (interact-ascend-index state)
    (interact-descend-index (+ idx offset) state1)))
(define interact-shift-left (interact-shift -1))
(define interact-shift-right (interact-shift 1))

(define (interact-step state)
  (match state
    ((interact-state holes focus)
     (do either-monad
       new-focus <- (step-safe focus)
       (pure (interact-state holes new-focus))))))

(define (interact-state-present state)
  (define (hole-present hole) (list-ref (hole-fill hole (void)) 1))
  (match state
    ((interact-state holes focus)
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

(define (interact-safe f state)
  (match (f state)
    ((left msg) (displayln msg) (right state))
    ((right state) (right state))))

(define (interact-loop state)
  (let loop ((st state))
    (let ((chain (interact-state-present st)))
      (printf "~a" (chain-show (chain-unparse-void chain)))
      (display "[hjkl](movement),[s]tep,[q]uit> ")
      (do either-monad
        st <- (match (read-line)
                ("h" (interact-safe interact-shift-left st))
                ("l" (interact-safe interact-shift-right st))
                ("j" (interact-safe interact-descend st))
                ("k" (interact-safe interact-ascend st))
                ("s" (interact-safe interact-step st))
                ("q" (left "quitting"))
                (_ (displayln "invalid choice") (right st)))
        (loop st)))))

(define (interact-with term) (interact-loop (interact-state-init term)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parsing (syntax-0)
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

(define (check-arity arity form)
  (if (equal? (length form) arity)
    (right '())
    (left (format "expected arity-~a form but found arity-~a form: ~s"
                  arity (length form) form))))
(define (check-symbol form)
  (if (symbol? form)
    (right '())
    (left (format "expected symbol but found: ~s" form))))

(define v-uno (value (uno)))
(define v-0 (value (bit (b-0))))
(define v-1 (value (bit (b-1))))

(define (parse pe form)
  (match form
    ('() (right v-uno))
    ((? integer?) (parse-integer pe form))
    ((? symbol?) (parse-bvar pe form))
    ((cons op rest) (parse-combination pe op form))
    (_ (left (format "cannot parse: ~s" form)))))

(define (parse-combination pe op form)
  (do either-monad
    proc <- (penv-syntax-op-get pe op)
    (proc pe form)))

(define (map-parse pe form) (map-monad either-monad (curry parse pe) form))
(define ((parse-apply proc arity) pe form)
  (do either-monad
    _ <- (check-arity arity form)
    args <- (map-parse pe (cdr form))
    (pure (apply proc args))))
(define (parse-under pe params body)
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
(define new-lam-apply (curry action-2 (lam-apply)))
(define (parse-lam-apply pe form)
  (do either-monad
    form <- (map-parse pe form)
    (cons proc args) = form
    (pure
      (let loop ((proc proc) (args args))
        (match args
          ('() proc)
          ((cons arg args) (loop (new-lam-apply proc arg) args)))))))
(define (parse-lam pe form)
  (do either-monad
    _ <- (check-arity 3 form)
    `(,_ ,names ,body) = form
    _ <- (if (>= (length names) 1) (right (void))
           (left (format "lam must include at least one parameter: ~v" form)))
    body <- (parse-under pe names body)
    (pure (foldr (lambda (_ body) (value (lam body))) body names))))
(define new-pair-access (curry action-2 (pair-access)))
(define parse-pair-access (parse-apply new-pair-access 3))
(define/match (new-pair l r)
  (((value vl) (value vr)) (right (value (pair vl vr))))
  ((_ _) (left (format "pair arguments must be values: ~v ~v" l r))))
(define (parse-pair pe form)
  (do either-monad
    _ <- (check-arity 3 form)
    `(,_ ,fl ,fr) = form
    l <- (parse pe fl)
    r <- (parse pe fr)
    (new-pair l r)))
(define (parse-as-thunk pe form) (parse pe `(lam (_) ,form)))
(define (parse-tuple pe form)
  (define/match (tuple-value term)
    (((value val)) (right val))
    ((_)           (left (format "tuple elements must be values: ~v" form))))
  (do either-monad
    `(,_ . ,felems) = form
    velems <- (map-parse pe felems)
    elems <- (map-monad either-monad tuple-value velems)
    (pure (value (tuple-encode elems)))))

; derived syntax
(define (parse-if-0 pe form)
  (do either-monad
    _ <- (check-arity 4 form)
    `(,_ ,fcnd ,fzero ,fone) = form
    cnd <- (parse pe fcnd)
    zero <- (parse-as-thunk pe fzero)
    one <- (parse-as-thunk pe fone)
    alts <- (new-pair zero one)
    (pure (new-lam-apply (new-pair-access cnd alts) v-uno))))
(define parse-pair-l (parse-apply (curry new-pair-access v-0) 2))
(define parse-pair-r (parse-apply (curry new-pair-access v-1) 2))

; TODO: encode human-friendly numerals and symbols
(define (parse-integer pe form)
  (match form
    (0 (right v-0))
    (1 (right v-1))
    (_ (left (format "expected 0 or 1 but found: ~s" form)))))

(define (parse-fixpoint pe form)
  (do either-monad
    _ <- (check-arity 3 form)
    `(,_ ,names ,body) = form
    _ <- (if (>= (length names) 2) (right (void))
           (left (format "fix must include at least two parameters: ~v" form)))
    body <- (parse-under pe names body)
    proc = (foldr (lambda (_ body) (value (lam body))) body names)
    (pure (new-lam-apply Y-combinator proc))))

(define penv-init
  (foldr (lambda (keyval pe) (apply (curry penv-syntax-add pe) keyval))
         penv-empty
         `((,penv-syntax-op-empty ,parse-lam-apply)
           (lam ,parse-lam)
           (pair ,parse-pair)
           (pair-access ,parse-pair-access)
           (pair-l ,parse-pair-l)
           (pair-r ,parse-pair-r)
           (tuple ,parse-tuple)
           (if-0 ,parse-if-0)
           (fix ,parse-fixpoint)
           )))

(define Y-combinator
  (right-x (parse penv-init
                  `(lam (f) ((lam (d) (d d))
                             (lam (x a) (f (x x) a)))))))

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

; TODO: support a fixpoint operator?
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
        ((action-2 _ t0 t1)
         (or (mentions-bvar? idx t0) (mentions-bvar? idx t1)))))
    (match val
      ((lam body) (not (mentions-bvar? 0 body)))
      (_ #f)))
  (match term
    ((value v) (unparse-value upe v))
    ((action-2 (lam-apply)
      (action-2 (pair-access) tcnd
        (value (pair (? thunk-form? alt-0) (? thunk-form? alt-1))))
      (value (uno)))
     `(if-0 ,(unparse upe tcnd)
            ,(unparse upe (lam-body alt-0)) ,(unparse upe (lam-body alt-1))))
    ((action-2 (lam-apply) tproc targ)
     (unparse-application unparse upe tproc (list targ)))
    ((action-2 (pair-access) (value (bit b)) tpair)
     (list (match b ((b-0) 'pair-l) ((b-1) 'pair-r)) (unparse upe tpair)))
    ((action-2 act t0 t1)
     (unparse-action-2 act (unparse upe t0) (unparse upe t1)))))
(define (unparse-application unparse upe tproc targs)
  (match tproc
    ((action-2 (lam-apply) tproc targ)
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
(define (tuple-first tup) (pair-l tup))
(define (tuple-rest tup)  (pair-r tup))
(define tuple-nil? uno?)
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

(define ((tuple-lens idx) tup)
  (define ((fill-hole acc tup) filler)
    (tuple-encode-revappend acc (tuple-cons filler tup)))
  (let loop ((acc '()) (idx idx) (tup tup))
    (if (equal? 0 idx)
      (lens-result (tuple-first tup) (fill-hole acc (tuple-rest tup)))
      (loop (cons (tuple-first tup) acc) (- idx 1) (tuple-rest tup)))))

(define (tuple-get idx tup)     (:. tup (tuple-lens idx)))
(define (tuple-set idx val tup) (:= tup val (tuple-lens idx)))

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

(define (bits-select choice default alternatives)
  (alist-get-default alternatives choice default))

;; symbol
(variant (symbol-entry (repr sub-table)))
(define (symbol-repr uid bitwidth) (bits-pad bitwidth (bits-encode uid)))

(variant (symbol-table (capacity mapping rev-mapping next-uid)))
(define (symbol-table-empty capacity)
  (symbol-table capacity dict-empty dict-empty 0))
(define (symbol-table-bitwidth table)
  (bits-required (symbol-table-capacity table)))

(define (symbol-table-get table key)
  (just-x (dict-get (symbol-table-mapping table) key)))
(define (symbol-table-encode table key)
  (symbol-entry-repr (symbol-table-get table key)))
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
       (match (dict-get mapping key)
         ((nothing)
          (symbol-table capacity
                        (dict-add mapping key entry)
                        (dict-add rev-mapping next-uid key)
                        (+ 1 next-uid)))
         (_ (error (format "symbol already added for key: ~v" key))))))))

(define ((symbol-table-lens key) table)
  (match table
    ((symbol-table capacity mapping rev-mapping next-uid)
     (let* ((entry (symbol-table-get table key))
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
  (symbol-table-encode (unbox *symbol-table*) key))
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

;; TODO:
; construct terms that build/recognize/deconstruct tagged data
;   construct symbol-selector terms (more generally, bits-selector terms)

; generalize outermost payload tags to data-schemas: (schema-tag, ... optional polymorphic type info ...)

; non-schematic local tags (for inner payloads) correspond to constructors
;   for example: inner payload tagged with Cons or Nil; outer payload tagged with appropriate List schema
