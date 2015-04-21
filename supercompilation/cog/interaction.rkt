#lang racket
(provide
  interact-with
  )

(require
  "presentation.rkt"
  "semantics-operational.rkt"
  "substitution.rkt"
  "syntax.rkt"
  "syntax-abstract.rkt"
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

(define view-syntax-doc nav-term-lifted->doc)
(define view-syntax-raw nav-term-lifted-old-raw->doc)
(define view-syntax-0 nav-term-lifted-old->doc)
(define (view-toggle current-view)
  (right (if (eq? view-syntax-doc current-view)
           view-syntax-0 view-syntax-doc)))

(record interact-state view context history)
(def (interact-state-viewcontext (interact-state view ctx _)) (view ctx))

(records interact-resp
  (interact-resp-done msg)
  (interact-resp-error msg)
  (interact-resp-next st))

(define ((interact-state-trans path) trans st)
  (begin/with-monad either-monad
    component = (:. st path)
    result <- (trans component)
    (pure (:= st result path))))
(define interact-context (interact-state-trans '(context)))
(define interact-view (interact-state-trans '(view)))
(define (interact-context-count f st count)
  (either-fold interact-resp-error interact-resp-next
    (let loop ((est (right st)) (count count))
      (if (= count 0) est
        (match est
          ((left _) est)
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
     ,(lambda (count) (interact-context-count interact-substitute-full st 1)))
    (#\s "step"
     ,(lambda (count) (interact-context-count interact-step st count)))
    (#\c "complete"
     ,(lambda (count) (interact-context-count interact-complete st 1)))
    (#\x "toggle-syntax"
     ,(lambda (count) (interact-resp-next
                        (right-x (interact-view view-toggle st)))))
    (#\u "undo"
     ,(lambda (count) (match history
                        ('() (interact-resp-error "nothing to undo!"))
                        ((cons prev-state hist)
                         (interact-resp-next prev-state)))))
    (#\q "quit"
     ,(lambda (count) (interact-resp-done "quitting"))))
  common-commands)

(define interact-controller
  (gn yield (st)
    (letn loop (list keymap resp) =
               (list (void) (just (interact-resp-next st)))
      (match resp
        ((just (interact-resp-done final)) final)
        (_ (lets
          (list keymap output) =
          (match resp
            ((nothing) (list keymap (nothing)))
            ((just (interact-resp-error msg)) (list keymap (just (left msg))))
            ((just (interact-resp-next st))
             (lets commands = (state->commands st)
                   command-desc = (commands->desc commands)
                   keymap = (commands->keymap commands)
                   (list keymap (just (right (list command-desc st)))))))
          (event-keycount char count) = (yield output)
          (loop (list keymap (maybe-map (lambda (action) (action count))
                                        (dict-get keymap char))))))))))

(define interact-view-controller
  (gn yield (input)
    (letn loop (list (list msg cmd-desc st) input) =
               (list (list "" (void) (void)) input)
      result =
      (match input
        ((left msg)                 (list msg cmd-desc st))
        ((right (list cmd-desc st)) (list "" cmd-desc st)))
      (loop (list result (yield result))))))

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
    composite-commands =
    `((#\H "pane left"
       ,(lambda (layout focus-index count)
          (list layout (- focus-index count))))
      (#\L "pane right"
       ,(lambda (layout focus-index count)
          (list layout (+ focus-index count))))
      (#\R "pane reverse"
       ,(lambda (layout focus-index count)
          (list (list-range-reverse
                  layout focus-index
                  (min (length layout) (+ focus-index count 1)))
                focus-index))))
    composite-command-desc = (commands->desc composite-commands)
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
      command-desc = (append composite-command-desc command-desc)
      (list msg command-desc focus-index st-views))
    (gn yield (init-sts)
      focus-index = 0
      layout = (range (length init-sts))
      (gen-susp views composite) =
      (gen-fold (fn (a b) a) (hash) composite-controller
        (forl
          st <- init-sts
          key <- layout
          (gen-susp (just view) ctrl) = ((gen-compose
                                           (maybe-gen interact-view-controller)
                                           interact-controller) st)
          (composite-add key view ctrl)))
      composite-view = (views->composite-view layout focus-index views)
      event = (yield (just composite-view))
      (letn loop
          (list
            (list composite composite-view views layout focus-index) event) =
          (list
            (list composite composite-view views layout focus-index) event)
        (event-keycount char count) = event
        keymap = (commands->keymap composite-commands)
        (list composite mresult) =
        (match (dict-get keymap char)
          ((just action)
           (list composite
                 (just (list* views (action layout focus-index count)))))
          ((nothing)
           (lets
             (gen-susp result composite) =
             (match (list-get layout focus-index)
               ((nothing) (gen-susp (nothing) composite))
               ((just key) (composite (composite-send key event))))
             mresult =
             (match result
               ((nothing)
                ; TODO: try backup commands first
                (nothing))
               ((just (list final views))
                (lets
                  layout = (if (nothing? final) layout
                             (list-remove layout focus-index))
                  (just (list views layout focus-index)))))
             (list composite mresult))))
        mresult =
        (begin/with-monad maybe-monad
          (list views layout focus-index) <- mresult
          focus-index = (max 0 (min (- (length layout) 1) focus-index))
          composite-view = (views->composite-view layout focus-index views)
          (pure (list (views->composite-view layout focus-index views)
                      views layout focus-index)))
        result = (list* composite
                        (maybe-from
                          (list composite-view views layout focus-index)
                          mresult))
        (list _ _ _ layout _) = result
        new-composite-view =
        (maybe-map (fn ((list composite-view _ _ _)) composite-view) mresult)
        (if (empty? layout) "quitting: all interactions closed"
          (loop (list result (yield new-composite-view))))))))

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
      (letn loop (list command-desc focus-idx st-views input) =
                 (list (void) (void) (void) input)
        (list msg command-desc focus-idx st-views) =
        (match input
          ((left count)
           (list (number->string count) command-desc focus-idx st-views))
          ((right result)
           (match result
             ((nothing) (list "invalid choice" command-desc focus-idx st-views))
             ((just view) view))))
        display-str =
        (thunk (view->string
                 (tabular-view msg command-desc focus-idx st-views)))
        (loop (list command-desc focus-idx st-views (yield display-str))))))
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
