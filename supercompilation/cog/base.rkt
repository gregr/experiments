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
       (pure (action-2 act val t1-1))))
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

(define (interact-state-show state)
  (define (hole-show hole)
    (pretty-string (list-ref (hole-fill hole (void)) 1)))
  (define (holes-show holes)
    (string-join (map hole-show (reverse holes)) "\n----------------\n\n"))
  (match state
    ((interact-state holes focus)
     (string-join (list "" (holes-show holes) (pretty-string focus) "")
                  "\n================================\n\n"))))

(define (interact-safe f state)
  (match (f state)
    ((left msg) (displayln msg) (right state))
    ((right state) (right state))))

(define (interact-loop state)
  (let loop ((st state))
    (printf "~a\n" (interact-state-show st))
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
      (loop st))))

(define (interact-with term) (interact-loop (interact-state-init term)))

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

(define ((symbol-table-lens keys) table)
  (define ((rebuild chain) new-table)
    (foldl (match-lambda**
             (((list (symbol-table capacity mapping rev-mapping next-uid)
                     key entry) new-table)
              (match entry
                ((symbol-entry repr sub-table)
                 (symbol-table
                   capacity
                   (dict-add mapping key (symbol-entry repr new-table))
                   rev-mapping
                   next-uid)))))
           new-table chain))
  (match-let (((list chain table)
               (let loop ((chain '()) (table table) (keys keys))
                 (match keys
                   ('() (list chain table))
                   ((cons key keys)
                    (let* ((entry (symbol-table-get table key))
                           (next-table (symbol-entry-sub-table entry)))
                      (loop (cons (list table key entry) chain)
                            next-table keys)))))))
    (cons table (rebuild chain))))

(define (symbol-table-get-chain table keys)
  (car ((symbol-table-lens keys) table)))
(define (symbol-table-set-chain table keys sub-table)
  ((cdr ((symbol-table-lens keys) table)) sub-table))

(define (symbol-table-encode-chain table keys)
  (symbol-table-encode
    (symbol-table-get-chain table (list-init keys))
    (last keys)))
(define (symbol-table-decode-chain table keys symbol)
  (symbol-table-decode
    (symbol-table-get-chain table keys)
    symbol))
(define (symbol-table-add-chain table keys max-children)
  (match-let (((cons table rebuild)
               ((symbol-table-lens (list-init keys)) table)))
    (rebuild (symbol-table-add table (last keys) max-children))))

(define symbol-capacity-default 256)  ; TODO: arbitrary-precision encoding?
(define *symbol-table* (box (symbol-table-empty symbol-capacity-default)))

(define (symbol-get table key)
  (symbol-table-get (unbox *symbol-table*) key))
(define (symbol-encode key)
  (symbol-table-encode (unbox *symbol-table*) key))
(define (symbol-decode symbol)
  (symbol-table-decode (unbox *symbol-table*) symbol))
(define (symbol-add key (max-children 0))
  (set-box! *symbol-table*
            (symbol-table-add (unbox *symbol-table*) key max-children)))

(define (symbol-get-chain keys)
  (symbol-table-get-chain (unbox *symbol-table*) keys))
(define (symbol-encode-chain keys)
  (symbol-table-encode-chain (unbox *symbol-table*) keys))
(define (symbol-decode-chain keys symbol)
  (symbol-table-decode-chain (unbox *symbol-table*) keys symbol))
(define (symbol-add-chain keys (max-children 0))
  (set-box! *symbol-table*
            (symbol-table-add-chain (unbox *symbol-table*) keys max-children)))

;; TODO:
; construct terms that build/recognize/deconstruct tagged data
;   construct symbol-selector terms (more generally, bits-selector terms)

; generalize outermost payload tags to data-schemas: (schema-tag, ... optional polymorphic type info ...)

; non-schematic local tags (for inner payloads) correspond to constructors
;   for example: inner payload tagged with Cons or Nil; outer payload tagged with appropriate List schema
