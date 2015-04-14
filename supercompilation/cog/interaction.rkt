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
  gregr-misc/dict
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

(define (commands->desc commands)
  (forl
    (list char desc action) <- commands
    (list (list->string (list char)) desc)))

(define (commands->keymap commands)
  (make-immutable-hash
    (forl
      (list char desc action) <- commands
      (cons char action))))

(def (state->commands st)
  (interact-state _ nav history) = st
  st = (:~* st (curry cons st) 'history)
  common-commands =
  `((#\h "traverse left"
     ,(lambda (count) (interact-context-count interact-shift-left st count)))
    (#\j "traverse down"
     ,(lambda (count) (interact-context-count interact-descend st count)))
    (#\k "traverse up"
     ,(lambda (count) (interact-context-count interact-ascend st count)))
    (#\l "traverse right"
     ,(lambda (count) (interact-context-count interact-shift-right st count)))
    (#\S "substitute"
     ,(lambda (count) (right (interact-context interact-substitute-full st))))
    (#\s "step"
     ,(lambda (count) (interact-context-count interact-step st count)))
    (#\c "complete"
     ,(lambda (count) (right (interact-context interact-complete st))))
    (#\x "toggle-syntax"
     ,(lambda (count) (right (interact-view view-toggle st))))
    (#\u "undo"
     ,(lambda (count) (right (match history
                               ('() (left "nothing to undo!"))
                               ((cons prev-state hist) (right prev-state))))))
    (#\q "quit"
     ,(lambda (count) (left "quitting"))))
  common-commands)

(define interact-controller
  (gn yield (st)
    (letn loop (list handled? st next-st) = (list #t st (right st))
      st = (either-from st next-st)
      msg = (either-fold identity (const "") next-st)
      commands = (state->commands st)
      command-desc = (commands->desc commands)
      result = (if handled? (just (list msg command-desc st)) (nothing))
      (event-keycount char count) = (yield result)
      keymap = (commands->keymap commands)
      (match (dict-get keymap char)
        ((nothing) (loop (list #f st (right st))))
        ((just action)
         (match (action count)
           ((left final) final)
           ((right next-st) (loop (list #t st next-st)))))))))

(records composite-event
  (composite-add key view ctrl)
  (composite-remove key)
  (composite-send key event))

(define composite-controller
  (gn yield (event)
    (let loop ((views (hash)) (ctrls (hash)) (event event))
      (match event
        ((composite-add key view ctrl)
         (lets
           views = (dict-set views key view)
           ctrls = (dict-set ctrls key ctrl)
           (loop views ctrls (yield views))))
        ((composite-remove key)
         (lets
           views = (dict-remove views key)
           ctrls = (dict-remove ctrls key)
           (loop views ctrls (yield views))))
        ((composite-send key event)
         (match ((dict-ref ctrls key) event)
           ((gen-result final)
            (lets
              views = (dict-remove views key)
              ctrls = (dict-remove ctrls key)
              (loop views ctrls (yield (just (list (just final) views))))))
           ((gen-susp result ctrl)
            (match result
              ((nothing) (loop views ctrls (yield (nothing))))
              ((just view)
               (lets
                 views = (dict-set views key view)
                 ctrls = (dict-set ctrls key ctrl)
                 (loop views ctrls
                       (yield (just (list (nothing) views))))))))))))))

(define composite-interact-controller
  (lets
    views->composite-view = (fn (layout focus-index views)
      (list msgs command-descs st-views) =
      (zip-default '(() () ())
        (forl
          key <- layout
          (list msg command-desc st) = (dict-ref views key)
          st-view = (delay (interact-state-viewcontext st))
          (list msg command-desc st-view)))
      msg = (list-ref-default msgs focus-index "")
      command-desc = (list-ref-default command-descs focus-index '())
      (list msg command-desc (delay (composite->doc (map force st-views)))))
    (gn yield (init-sts)
      focus-index = 0
      layout = (range (length init-sts))
      (gen-susp views composite) =
      (forf
        (gen-susp _ composite) = (gen-susp (hash) composite-controller)
        st <- init-sts
        key <- layout
        (gen-susp (just view) ctrl) = (interact-controller st)
        (composite (composite-add key view ctrl)))
      composite-view = (views->composite-view layout focus-index views)
      event = (yield (just composite-view))
      (letn loop (list composite composite-view layout focus-index event) =
                 (list composite composite-view layout focus-index event)
        (event-keycount char count) = event
        ; TODO: first try checking for char in top-level actions
        (gen-susp result composite) =
        (match (list-get layout focus-index)
          ((nothing) (gen-susp (nothing) composite))
          ((just key) (composite (composite-send key event))))
        (list layout new-composite-view) =
        (match result
          ((nothing)
           ; TODO: try backup commands first
           (list layout (nothing)))
          ((just (list final views))
           (lets
             layout = (if (nothing? final) layout
                        (list-remove layout focus-index))
             (list layout
                   (just (views->composite-view layout focus-index views))))))
        focus-index = (min focus-index (length layout))
        composite-view = (maybe-from composite-view new-composite-view)
        (if (empty? layout) "quitting: all interactions closed"
          (loop (list composite composite-view layout focus-index
                      (yield new-composite-view))))))))

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

(define (interact-loop states)
  (define event-chan (make-channel))
  (define display-chan (make-channel))
  (define build-display-str
    (generator* yield (input)
      (letn loop (list command-desc st-view input) = (list (void) (void) input)
        (list msg command-desc st-view) =
        (match input
          ((left count) (list (number->string count) command-desc st-view))
          ((right result)
           (match result
             ((nothing) (list "invalid choice" command-desc st-view))
             ((just view) view))))
        display-str =
        (thunk (view->string (tabular-view command-desc msg st-view)))
        (loop (list command-desc st-view (yield display-str))))))
  (with-cursor-hidden (with-stty-direct
    (lets
      threads = (list (keypress-thread event-chan)
                      (display-view-thread 0.1 display-chan))
      result =
      (gen-loop
        (gen-compose*
          keycount-controller
          (fn->gen (lambda (_) (channel-get event-chan)))
          (fn->gen (curry channel-put display-chan))
          build-display-str
          (either-gen composite-interact-controller))
        (right states))
      _ = (for-each kill-thread threads)
      result))))

(define (interact-with terms)
  (interact-loop
    (forl
      term <- terms
      (interact-state view-syntax-doc (navigator-new hole-keys term) '()))))
