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
  gregr-misc/generator
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
  (string-join (map pretty-string chain) "----\n"))

(define view-syntax-doc nav-term-lifted->doc)
(define view-syntax-raw
  (compose1 string->doc chain-show interact-context-present))
(define view-syntax-0
  (compose1 string->doc chain-show reverse chain-unparse-void
            interact-context-present))
(define (view-toggle current-view)
  (right (if (eq? view-syntax-doc current-view)
           view-syntax-0 view-syntax-doc)))

(record interact-state view context history)
(def (interact-state-viewcontext (interact-state view ctx _)) (view ctx))

(define ((interact-state-trans path) trans st)
  (begin/with-monad either-monad
    component = (:. st path)
    result <- (trans component)
    (pure (:= st result path))))
(define interact-context (interact-state-trans '(context)))
(define interact-view (interact-state-trans '(view)))
(define (interact-context-count f st count)
  (right (let loop ((est (right st)) (count count))
    (if (= count 0) est
      (match est
        ((left _)   est)
        ((right st) (loop (interact-context f st) (- count 1))))))))

(define (interact-controller st)
  (fn ((event-keycount char count))
    current-st = st
    st = (:~* st (curry cons st) 'history)
    action =
    (match char
      (#\h (thunk (interact-context-count interact-shift-left st count)))
      (#\l (thunk (interact-context-count interact-shift-right st count)))
      (#\j (thunk (interact-context-count interact-descend st count)))
      (#\k (thunk (interact-context-count interact-ascend st count)))
      (#\S (thunk (right (interact-context interact-substitute-full st))))
      (#\s (thunk (interact-context-count interact-step st count)))
      (#\c (thunk (right (interact-context interact-complete st))))
      (#\x (thunk (right (interact-view view-toggle st))))
      (#\u (thunk (right (match (:.* current-st 'history)
                           ('() (left "nothing to undo!"))
                           ((cons prev-state hist) (right prev-state))))))
      (#\q (thunk (left "quitting")))
      (_   (thunk (right (left "invalid choice")))))
    (match (action)
      ((left final) (gen-result final))
      ((right result)
       (gen-susp result (interact-controller
                          (either-from current-st result)))))))

(define (keypress-thread chan)
  (thread
    (thunk (gen-loop (apply gen-compose* (map fn->gen
      (list (curry channel-put chan) event-keypress (lambda (_) (read-char)))))
      (void)))))

(define (display-view-thread latency chan)
  (define fetch-chan (make-channel))
  (def (display-view view)
    view-str = (view)
    _ = (screen-clear)
    (display view-str))
  (define (display-loop timer)
    (display-view (channel-get fetch-chan))
    (sleep-remaining latency timer)
    (display-loop (gen-susp-k (timer))))
  (def (fetch-loop view)
    evt = (sync chan (channel-put-evt fetch-chan view))
    (if (channel-put-evt? evt)
      (fetch-loop (channel-get chan))
      (fetch-loop evt)))
  (define fetch-thread (thread (thunk (fetch-loop (channel-get chan)))))
  (define display-thread (thread (thunk (display-loop (timer-now)))))
  (thread (thunk
    (thread-wait fetch-thread)
    (kill-thread display-thread)))
  fetch-thread)

(define (interact-loop state)
  (define commands
    `(("h" "traverse left")
      ("j" "traverse down")
      ("k" "traverse up")
      ("l" "traverse right")
      ("S" "substitute")
      ("s" "step")
      ("c" "complete")
      ("x" "toggle-syntax")
      ("u" "undo")
      ("q" "quit")))
  (define event-chan (make-channel))
  (define display-chan (make-channel))
  (define build-display-str
    (generator* yield (input)
      (letn loop (list st-view input) = (list (void) input)
        (list msg st-view) =
        (match input
          ((left msg) (list msg st-view))
          ((right st-view) (list "" st-view)))
        display-str = (thunk (view->string (tabular-view commands msg st-view)))
        (loop (list st-view (yield display-str))))))
  (define ctrl (gen-compose*
                 (fn->gen
                   (curry either-fold (compose1 left number->string) identity))
                 (either-gen (interact-controller state))
                 keycount-controller))
  (with-cursor-hidden (with-stty-direct
    (lets
      threads = (list (keypress-thread event-chan)
                      (display-view-thread 0.1 display-chan))
      result =
      (gen-loop
        (gen-compose* ctrl
                      (fn->gen (lambda (_) (channel-get event-chan)))
                      (fn->gen (curry channel-put display-chan))
                      build-display-str
                      (fn->gen (curry either-map
                        (lambda (st) (delay (interact-state-viewcontext st))))))
        (right state))
      _ = (for-each kill-thread threads)
      result))))

(define (interact-with term)
  (interact-loop
    (interact-state view-syntax-doc (navigator-new hole-keys term) '())))
