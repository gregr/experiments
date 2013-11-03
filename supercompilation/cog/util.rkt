#lang racket
(require racket/stxparam)
(provide (all-defined-out))

(define (pretty-string x) (call-with-output-string (curry pretty-print x)))

(define-syntax variant
  (syntax-rules ()
    ((_) (void))
    ((_ (name fields) more ...)
      (begin (define-struct name fields #:transparent) (variant more ...)))))
(define-syntax data
  (syntax-rules ()
    ((_ name var ...) (variant var ...))))
; TODO use data name to store properties
; for example, collect typeclass/multimethod instances
;   allow do notation to refer only to data name, rather than name-monad

(data monad (monad (pure bind)))

(define-syntax do-with
  (syntax-rules (<-)
    ((_ combine pat <- stmt rest ...)
      (combine stmt (match-lambda (pat (do-with combine rest ...)))))
    ((_ combine pat = stmt rest ...)
      (match-let ((pat stmt)) (do-with combine rest ...)))
    ((_ combine stmt) stmt)))
(define-syntax-parameter pure
  (lambda (stx) (raise-syntax-error #f "must be used inside 'do'" stx)))
(define-syntax do
  (syntax-rules ()
    ((_ monad stmt ...)
      (syntax-parameterize
        ((pure (syntax-rules () ((_ ex) ((monad-pure monad) ex)))))
          (do-with (monad-bind monad) stmt ...)))))

(define (map-monad monad proc xs)
  (match xs
    ('() (right '()))
    ((cons y ys)
      (do monad
        y0 <- (proc y)
        ys0 <- (map-monad monad proc ys)
        (pure (cons y0 ys0))))))

(data maybe
  (nothing ())
  (just (x)))
; TODO: automatically generate folds from data definitions
(define (maybe-fold nothing-fold just-fold maybe)
  (match maybe
    ((nothing) nothing-fold)
    ((just x) (just-fold x))))
(define (maybe-from nothing-from maybe)
  (match maybe
    ((nothing) nothing-from)
    ((just x) x)))
(define maybe-monad (monad
  just
  (lambda (prev next)
    (match prev
      ((nothing) (nothing))
      ((just x) (next x))))))

(data either
  (left (x))
  (right (x)))
(define (either-fold left-fold right-fold either)
  (match either
    ((left x) (left-fold x))
    ((right x) (right-fold x))))
(define either-monad (monad
  right
  (lambda (prev next)
    (match prev
      ((left x) (left x))
      ((right x) (next x))))))

(define (maybe->either left-arg maybe)
  (maybe-fold (left left-arg) right maybe))

(define ((flip proc) x y) (proc y x))

(define (list-index lst key)
  (let loop ((lst lst) (key key) (index 0))
    (match lst
      ('() (nothing))
      ((cons key0 lst)
        (if (equal? key0 key) (just index) (loop lst key (+ index 1)))))))
(define (list-init lst) (reverse (cdr (reverse lst))))

(define alist-empty '())
(define (alist-build keys vals) (map cons keys vals))
(define (alist-add alst key val) (cons (cons key val) alst))
(define (alist-get alst key)
  (match alst
    ('() (nothing))
    ((cons (cons key0 val) alst)
      (if (equal? key0 key) (just val)
        (alist-get alst key)))))
(define (alist-get-default alst key default)
  (match (alist-get alst key)
    ((nothing) default)
    ((just x) x)))

(define dict-empty (hash))
(define (dict-add dct key val) (hash-set dct key (just val)))
(define (dict-del dct key) (hash-remove dct key))
(define (dict-get dct key) (hash-ref dct key (nothing)))
(define (dict-get-default dct key default)
  (match (dict-get dct key)
    ((nothing) default)
    ((just x) x)))
(define (dict->alist dct) (hash->list dct))

(define (assoc-cmp kcmp)
  (match-lambda** (((cons k0 v0) (cons k1 v1)) (kcmp k0 k1))))

(define set-empty (set))
(define (set-unions ss)
  (match ss ('() set-empty) (_ (apply set-union ss))))

(define graph-empty (hash))
(define (graph-add-src gr src) (hash-update gr src (lambda (x) x) '()))
(define (graph-add-edge gr src tgt)
  (graph-add-src (hash-update gr src (curry cons tgt) '()) tgt))
(define (graph-tgts gr src) (hash-ref gr src '()))
(define graph->alist hash->list)
(define (alist->graph as)
  (foldr (match-lambda** (((cons src tgt) gr) (graph-add-edge gr src tgt)))
         graph-empty as))
(define graph-srcs hash-keys)
(define (graph-reverse gr)
  (foldl (match-lambda**
           (((cons src tgts) gr)
            (foldr (lambda (tgt gr) (graph-add-edge gr tgt src)) gr tgts)))
         graph-empty (graph->alist gr)))
(define (graph-dfs gr srcs visited)
  (let search ((pending srcs) (visited visited) (finished '()))
    (foldl
      (match-lambda**
        ((src (list visited finished))
          (if (set-member? visited src) (list visited finished)
            (match-let* ((targets (graph-tgts gr src))
                        ((list visited finished)
                          (search targets (set-add visited src) finished)))
              (list visited (cons src finished))))))
      (list visited finished)
      pending)))
(define (graph-topsort gr)
  (match-let*
    (((list _ finished)
      (graph-dfs gr (graph-srcs gr) set-empty))
     (rgr (graph-reverse gr))
     ((list _ sccs)
      (foldl (match-lambda**
               ((src (list visited sccs))
                (match-let
                  (((list visited finished)
                    (graph-dfs rgr (list src) visited)))
                  (list visited
                        (if (null? finished) sccs (cons finished sccs))))))
             (list set-empty '()) finished)))
    sccs))
(define (scc-tgts gr scc)
  (set->list
    (set-subtract (foldl (lambda (src total)
                           (set-union (list->set (graph-tgts gr src)) total))
                         set-empty scc) (list->set scc))))
(define (scc-hash scc hm)
  (foldl (lambda (src hm) (hash-set hm src scc)) hm scc))
(define (sccs-hash sccs) (foldl scc-hash (hash) sccs))
(define (sccs-relevant gr sccs relevant start)
  (define scch (sccs-hash sccs))
  (define relevant-init
    (foldl (lambda (src rel)
             (set-union rel (list->set (hash-ref scch src))))
           set-empty (set->list relevant)))
  (define (src-relevant src visited relevant)
    (if (set-member? visited src) (list visited relevant)
      (match-let* ((scc (hash-ref scch src))
                   (sscc (list->set scc))
                   (tgts (scc-tgts gr scc))
                   (visited (set-union visited sscc))
                   ((list visited relevant)
                    (foldl (match-lambda**
                             ((src (list visited relevant))
                              (src-relevant src visited relevant)))
                           (list visited relevant) tgts)))
        (if (set-empty? (set-intersect (list->set tgts) relevant))
          (list visited relevant)
          (list visited (set-union relevant sscc))))))
  (cadr (foldl (lambda (src result)
                 (apply (curry src-relevant src) result))
               (list set-empty relevant-init) (set->list start))))
(define (sccs-filter sccs relevant)
  (filter (lambda (scc)
            (not (set-empty? (set-intersect relevant (list->set scc))))) sccs))

; TODO:
; lenses?
; for1[-monad]: flip last two params of map[-monad]

; testing

;(define test-graph
  ;(alist->graph '((a . b) (b . c) (b . d) (c . e) (d . e) (e . f))))
;(define test-graph2
  ;(alist->graph '((a . b) (b . c) (b . d) (c . e) (d . e) (e . f) (e . b))))
;(define test-tops (graph-topsort test-graph))
;(define test-tops2 (graph-topsort test-graph2))
;test-tops
;'((f) (e) (c) (d) (b) (a))
;test-tops2
;'((f) (c b e d) (a))
;(sccs-relevant test-graph test-tops (set 'a) (set 'b))
;(set 'a)
;(sccs-relevant test-graph test-tops (set 'b) (set 'a))
;(set 'a 'b)
;(sccs-relevant test-graph2 test-tops2 (set 'b) (set 'b))
;(set 'b 'c 'd 'e)
;(sccs-relevant test-graph2 test-tops2 (set 'a) (set 'b))
;(set 'a)
;(sccs-relevant test-graph2 test-tops2 (set 'b) (set 'a))
;(set 'b 'c 'd 'a 'e)
;(sccs-relevant test-graph2 test-tops2 (set 'f) (set 'a))
;(set 'b 'c 'd 'a 'f 'e)
;(sccs-filter test-tops2 (set 'a))
;'((a))
;(sccs-filter test-tops2 (set 'b))
;'((d b e c))
;(sccs-filter test-tops2 (set 'a 'b))
;'((d b e c) (a))
;(sccs-filter test-tops2 (set 'c 'f))
;'((f) (d b e c))
;(sccs-filter test-tops2 (set 'a 'd 'f))
;'((f) (d b e c) (a))

;(display
  ;(do-with (lambda (prev next) (+ 1 (next prev)))
    ;a (+ 3 4)
    ;b (+ a 5)
    ;b))
;(newline)

;> (alist-add alist-empty 'x 4)
;(list (cons 'x (just 4)))
;> (alist-add (alist-add alist-empty 'x 4) 'y 3)
;(list (cons 'y (just 3)) (cons 'x (just 4)))
;> (alist-del (alist-add (alist-add alist-empty 'x 4) 'y 3) 'x)
;(list (cons 'x (nothing)) (cons 'y (just 3)) (cons 'x (just 4)))
;> (alist-get (alist-del (alist-add (alist-add alist-empty 'x 4) 'y 3) 'x) 'y)
;(just 3)
;> (alist-get (alist-del (alist-add (alist-add alist-empty 'x 4) 'y 3) 'x) 'x)
;(nothing)
;> (alist-get (alist-del (alist-add (alist-add alist-empty 'x 4) 'y 3) 'x) 'z)
;(nothing)

;> (penv-syntax-add penv-empty 'x 'y)
;(penv (dict (list (cons 'x (just 'y)))) '())
;> (penv-vars-add penv-empty 'z)
;(penv (dict '()) '(z))
;> (penv-vars-add (penv-syntax-add penv-empty 'x 'y) 'z)
;(penv (dict (list (cons 'x (just 'y)))) '(z))
;> (penv-syntax-del (penv-vars-add (penv-syntax-add penv-empty 'x 'y) 'z) 'x)
;(penv (dict (list (cons 'x (nothing)) (cons 'x (just 'y)))) '(z))
;>

;> (list-index (list 1 2 3 4 5 'a 'b 'c) 4)
;(just 3)
;> (list-index (list 1 2 3 4 5 'a 'b 'c) 'b)
;(just 6)
;> (list-index (list 1 2 3 4 5 'a 'b 'c) 'd)
;(nothing)
;> (list-index (list 1 2 3 4 5 'a 'b 'c) 7)
;(nothing)
;> (list-index (list 1 2 3 4 5 'a 'b 'c) 0)
;(nothing)
;> (list-index (list 1 2 3 4 5 'a 'b 'c) 1)
;(just 0)
;>
