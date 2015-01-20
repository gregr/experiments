#lang racket
(provide
  interact-with
  )

(require
  "semantics-operational.rkt"
  "syntax-0-unparsing.rkt"
  "syntax-abstract.rkt"
  "util.rkt"
  gregr-misc/cursor
  gregr-misc/either
  gregr-misc/list
  gregr-misc/match
  gregr-misc/monad
  gregr-misc/record
  gregr-misc/sugar
  )

(def (subst-keys sub)
  sub-keys =
  (let loop ((prev '(v)) (sub sub))
    (match sub
      ((bvar-lift _)      '())
      ((bvar-use _ _ sub)
       (lets
         next = (list* 's prev)
         (list* next (loop next sub))))))
  _ = (displayln "sub keys:")
  _ = (displayln sub-keys)
  (list* '(t) sub-keys))

(define (hole-keys focus)
  (match focus
    ((subst sub _) (subst-keys sub))
    ((lam _ _)     '((body)))
    (_ (if (or (term? focus) (pair? focus))
         (map list (dict-keys focus)) '()))))

(def (hole-key idx focus)
  keys = (hole-keys focus)
  (if (and (<= 0 idx) (< idx (length keys)))
    (right (list-ref keys idx))
    (left (format "cannot select subterm ~a of: ~v" idx focus))))

(define (interact-ascend-index cterm)
  (if (empty? (cursor-trail cterm)) (left "no hole to fill")
    (lets
      (list count new-cterm) =
      (let loop ((count 1) (cterm (::^ cterm)))
        (if (term-substitution? (::.* cterm))
          (loop (+ count 1) (::^ cterm)) (list count cterm)))
      key = (reverse (take (cursor-trail cterm) count))
      keys = (hole-keys (::.* new-cterm))
      idx = (list-index-equal keys key)
      (right (list idx new-cterm)))))

(def (interact-descend-index idx cterm)
  (begin/with-monad either-monad
    key <- (hole-key idx (::.* cterm))
    (pure (::@ cterm key))))

(def (interact-with-focus f cterm)
  (begin/with-monad either-monad
    new-focus <- (f (::.* cterm))
    (pure (::=* cterm new-focus))))

(def (interact-context-present cterm)
  trail =
  (let loop ((cterm cterm))
    (match (interact-ascend-index (::=* cterm (void)))
      ((left _) '())
      ((right (list _ void-cterm))
       (list* (::.* void-cterm) (loop void-cterm)))))
  (reverse (list* (::.* cterm) trail)))

(define (interact-ascend context)
  (begin/with-monad either-monad
    (list _ context) <- (interact-ascend-index context)
    (pure context)))

(define interact-descend (curry interact-descend-index 0))

(define ((interact-shift offset) context)
  (begin/with-monad either-monad
    (list idx context1) <- (interact-ascend-index context)
    (interact-descend-index (+ idx offset) context1)))
(define interact-shift-left (interact-shift -1))
(define interact-shift-right (interact-shift 1))

(define interact-step (curry interact-with-focus step-safe))
(define interact-complete (curry interact-with-focus step-complete-safe))

(record void-closure is-value upenv)
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

(record interact-state view context history)
(def (interact-state-viewcontext (interact-state view ctx _)) (view ctx))

(define ((left-display-default default) result)
  (either-fold (lambda (msg) (display msg) default) identity result))

(define ((interact-safe path) trans st)
  (right (:~ st (compose1 (left-display-default (:. st path)) trans) path)))
(define interact-safe-context (interact-safe '(context)))
(define interact-safe-view (interact-safe '(view)))

(define (interact-loop state)
  (let loop ((st state))
    (printf "~a" (interact-state-viewcontext st))
    (display "[hjkl](movement),[s]tep(count),[c]omplete,toggle-synta[x],[u]ndo,[q]uit> ")
    (begin/with-monad either-monad
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
              ("u" (match (:.* st 'history)
                     ('() (displayln "nothing to undo!") (right st))
                     ((cons prev-state hist) (right prev-state))))
              ("q" (left "quitting"))
              (_ (displayln "invalid choice") (right st)))
      st = (if (equal? input "u") st
             (:~* st (curry cons prev-st) 'history))
      (loop st))))

(define (interact-with term)
  (interact-loop
    (interact-state view-syntax-0 (::0 term) '())))
