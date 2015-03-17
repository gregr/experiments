#lang racket
(provide
  interact-with
  )

(require
  "presentation.rkt"
  "semantics-operational.rkt"
  "substitution.rkt"
  "syntax.rkt"
  "syntax-0-unparsing.rkt"
  "syntax-abstract.rkt"
  "util.rkt"
  gregr-misc/cursor
  gregr-misc/either
  gregr-misc/list
  gregr-misc/match
  gregr-misc/maybe
  gregr-misc/monad
  gregr-misc/navigator
  gregr-misc/record
  gregr-misc/sugar
  gregr-misc/terminal
  gregr-misc/ui
  )

(def (subst-keys (substitution uses _))
  use-keys =
  (forl
    use-list-key <- (list-navigator-keys uses)
    (append '(s uses) use-list-key '(v)))
  (list* '(t) use-keys))
(def (app-keys app)
  count = (- (length (gather-applications app)) 2)
  rargs = (iterate (fn (path) (list* 'proc path)) '(arg) count)
  args = (reverse rargs)
  proc = (make-list (+ 1 count) 'proc)
  (list* proc args))

(define (hole-keys focus)
  (match focus
    ((lam-apply _ _) (app-keys focus))
    ((value v)       (map (fn (key) (list* 'v key)) (hole-keys v)))
    ((subst sub _)   (subst-keys sub))
    ((lam _ _)       '((body)))
    (_ (if (or (term? focus) (pair? focus))
         (map list (dict-keys focus)) '()))))

(define (interact-ascend nav)
  (maybe->either "no hole to fill" (navigator-ascend nav)))
(define (interact-descend nav (idx 0))
  (maybe->either
    (format "cannot select subterm ~a of: ~v" idx (navigator-focus nav))
    (navigator-descend nav idx)))
(define ((interact-shift offset) nav)
  (maybe->either (format "cannot shift by ~a" offset)
                 (navigator-shift nav offset)))
(define interact-shift-left (interact-shift -1))
(define interact-shift-right (interact-shift 1))
(define (interact-with-focus f nav)
  (begin/with-monad either-monad
    new-focus <- (f (navigator-focus nav))
    (pure (navigator-focus-set nav new-focus))))
(define interact-step (curry interact-with-focus step-safe))
(define interact-complete (curry interact-with-focus step-complete-safe))
(define interact-substitute-full
  (curry interact-with-focus (compose1 right substitute-full)))
(define (interact-context-present nav)
  (forl
    (list focus hole-pos) <- (navigator-path nav)
    (match hole-pos
      ((nothing) focus)
      ((just (list _ key)) (:= focus (void) key)))))

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

(define view-syntax-doc
  (compose1 doc-show nav-term->doc))
(define view-syntax-raw
  (compose1 chain-show interact-context-present))
(define view-syntax-0
  (compose1 chain-show chain-unparse-void interact-context-present))
(define (view-toggle current-view)
  (right (if (eq? view-syntax-doc current-view)
           view-syntax-0 view-syntax-doc)))

(record interact-state view context history)
(def (interact-state-viewcontext (interact-state view ctx _)) (view ctx))

(define ((left-display-default default) result)
  (either-fold (lambda (msg) (displayln msg) default) identity result))

(define ((interact-safe path) trans st)
  (right (:~ st (compose1 (left-display-default (:. st path)) trans) path)))
(define interact-safe-context (interact-safe '(context)))
(define interact-safe-view (interact-safe '(view)))

(define (interact-safe-context-count f st count)
  (last (iterate
          (compose1 (curry interact-safe-context f) right-x)
          (right st) count)))

(define (interact-controller st)
  (define (done-ctrl result)
    (lambda (_) (list (done-ctrl result) (list (note-terminated)))))
  (fn ((event-keycount char count))
    prev-st = st
    action =
    (match char
      (#\h (lambda (count) (interact-safe-context-count interact-shift-left st count)))
      (#\l (lambda (count) (interact-safe-context-count interact-shift-right st count)))
      (#\j (lambda (count) (interact-safe-context-count interact-descend st count)))
      (#\k (lambda (count) (interact-safe-context-count interact-ascend st count)))
      (#\S (lambda (_) (interact-safe-context interact-substitute-full st)))
      (#\s (lambda (count) (interact-safe-context-count interact-step st count)))
      (#\c (lambda (_) (interact-safe-context interact-complete st)))
      (#\x (lambda (_) (interact-safe-view view-toggle st)))
      (#\u (lambda (count) (match (:.* st 'history)
                             ('() (displayln "nothing to undo!") (right st))
                             ((cons prev-state hist) (right prev-state)))))
      (#\q (lambda (_) (left (void))))
      (_   (lambda (_) (displayln "invalid choice") (right st))))
    (match (action count)
      ((left result) ((done-ctrl result) (void)))
      ((right st)
       (lets
         st = (if (equal? char #\u) st
                (:~* st (curry cons prev-st) 'history))
         (list (interact-controller st) (list (note-view st))))))))

(define (interact-loop state)
  (with-cursor-hidden (with-stty-direct
    (let loop ((st state)
               (ctrl (keycount-controller (interact-controller state))))
      (screen-clear)
      (displayln "[hjkl](movement),[S]ubstitute,[s]tep(count),[c]omplete,toggle-synta[x],[u]ndo,[q]uit\n")
      (time (printf "~a\n" (interact-state-viewcontext st)))
      (begin/with-monad either-monad
        char = (read-char)
        (list ctrl notes) = (ctrl (event-keypress char))
        st <- (monad-foldl either-monad
                (lambda (_ note)
                  (match note
                    ((note-terminated) (left "quitting"))
                    ((note-view st)    (right st))))
                st notes)
        (loop st ctrl))))))

(define (interact-with term)
  (interact-loop
    (interact-state view-syntax-doc (navigator-new hole-keys term) '())))
