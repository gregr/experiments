#lang racket
(require "util.rkt")
(require "syntax-abstract.rkt")
(require "semantics-operational.rkt")
(require "syntax-0-unparsing.rkt")
(provide (all-defined-out))

(data hole-term-value
  (hole-pair-l (r))
  (hole-pair-r (l))
  (hole-lam    ()))

(data hole-term
  (hole-value      ())
  (hole-produce    ())
  (hole-subst-t    (s))
  (hole-subst-s    (prefix suffix t))
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
    ((hole-action-2-1 act t0) (list 1 (action-2 act t0 subterm)))
    ((hole-subst-t s)         (list 0 (subst s subterm)))
    ((hole-subst-s prefix suffix t)
     (list (+ 1 (length prefix))
           (subst (foldr bvar-use (bvar-use subterm suffix) prefix) t)))))

(define/match (hole-make idx focus)
  ((0 (pair l r))           (right (list (hole-pair-l r)          l)))
  ((1 (pair l r))           (right (list (hole-pair-r l)          r)))
  ((0 (lam body))           (right (list (hole-lam)               body)))
  ((0 (value val))          (right (list (hole-value)             val)))
  ((0 (produce tm))         (right (list (hole-produce)           tm)))
  ((0 (action-2 act t0 t1)) (right (list (hole-action-2-0 act t1) t0)))
  ((1 (action-2 act t0 t1)) (right (list (hole-action-2-1 act t0) t1)))
  ((0 (subst sub tm))       (right (list (hole-subst-t sub)       tm)))
  (((? (lambda (k) (and (< 0 k) (>= (subst-length (subst-s focus)) k))) k)
    (subst sub tm))
   (match-let (((list val prefix suffix) (subst-hole-make (- idx 1) sub)))
     (right (list (hole-subst-s prefix suffix tm) val))))
  ((_ _) (left (format "cannot select subterm ~a of: ~v" idx focus))))

(define (subst-length sub)
  (match sub
    ((bvar-lift _)    0)
    ((bvar-use _ sub) (+ 1 (subst-length sub)))))

(define (subst-hole-make idx sub)
  (let loop ((idx idx) (sub sub) (acc '()))
    (match* (idx sub)
      ((0 (bvar-use v sub)) (list v (reverse acc) sub))
      ((k (bvar-use v sub)) (loop (- k 1) sub (cons v acc))))))

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
(define interact-big (curry interact-with-focus (compose1 right step-big)))

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
    (display "[hjkl](movement),[s]tep(count),[b]ig-step,[c]omplete,toggle-synta[x],[u]ndo,[q]uit> ")
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
              ("b" (interact-safe-context interact-big st))
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
