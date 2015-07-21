#lang racket
(provide
  interact-with
  ui-loop
  )

(require
  "database.rkt"
  "editor.rkt"
  "interaction-model.rkt"
  "presentation.rkt"
  "workspace-model.rkt"
  gregr-misc/cursor
  gregr-misc/dict
  gregr-misc/either
  gregr-misc/generator
  gregr-misc/list
  gregr-misc/maybe
  gregr-misc/monad
  gregr-misc/record
  gregr-misc/sugar
  gregr-misc/terminal
  gregr-misc/ui
  )

(module+ test
  (require
    rackunit
    ))

(define (commands->keymap commands)
  (make-immutable-hash
    (forl
      (list char desc cmd) <- commands
      (cons char cmd))))

(define (workspace->focus-commands ws-name ws db)
  (maybe-fold '() (fn ((interaction-widget name))
                      (interaction->commands ws-name name db))
              (workspace->focus-widget ws)))

(def (db->editor-commands ws-name db)
  ; TODO: specialized commands based on state
  cmd-table =
  `((#\n "new interaction" ,eci-interaction-new)
    (#\e "extract closed term" ,(curry eci-paste-subterm #f))
    (#\E "replacE closed term" ,(curry eci-paste-subterm #t)))
  (forl
    (list char desc instr) <- cmd-table
    (list char desc (compose1 (curry editor-command ws-name) instr))))

(def (db->workspace-commands-top name db)
  ; TODO: specialized commands based on workspace state
  ;ws = (:.* db 'workspaces name)
  cmd-table =
  `((#\q "pane close" ,wci-widget-close)
    (#\H "pane left" ,wci-widget-left)
    (#\L "pane right" ,wci-widget-right)
    (#\R "pane reverse" ,wci-widget-reverse))
  (forl
    (list char desc instr) <- cmd-table
    (list char desc (compose1 (curry workspace-command name) instr))))

(def (db->workspace-commands name db)
  ws = (:.* db 'workspaces name)
  editor-cmds = (db->editor-commands name db)
  ws-top-cmds = (db->workspace-commands-top name db)
  widget-cmds = (workspace->focus-commands name ws db)
  cmds->char-assoc = (lambda (cmds)
                       (forl
                         cmd <- cmds
                         (list char _ _) = cmd
                         (cons char cmd)))
  cmds-merge1 = (fn (cmds0 cmds1)
                  (list a0 a1) = (map cmds->char-assoc (list cmds0 cmds1))
                  a1 = (dict-subtract a1 a0)
                  (append* (map (curry map cdr) (list a0 a1))))
  (cmds-merge1 editor-cmds (cmds-merge1 ws-top-cmds widget-cmds)))

(def ((event->workspace-command ws-name) db (list char count))
  cmds = (db->workspace-commands ws-name db)
  keymap = (commands->keymap cmds)
  (begin/with-monad maybe-monad
    cmd-new <- (dict-get keymap char)
    (pure (cmd-new count))))

(define (interaction->commands ws-name name db)
  ; TODO: specialized commands based on interaction state
  (forl
    (list char desc instr) <-
    `((#\h "traverse left" ,ici-traverse-left)
      (#\j "traverse down" ,ici-traverse-down)
      (#\k "traverse up" ,ici-traverse-up)
      (#\l "traverse right" ,ici-traverse-right)
      (#\space "jump to binder" ,(lambda (_) (ici-traverse-binder)))
      (#\S "substitute completely" ,(lambda (_) (ici-substitute-complete)))
      (#\s "step" ,ici-step)
      (#\C "step completely" ,(lambda (_) (ici-step-complete)))
      (#\c "close over free vars" ,(lambda (_) (ici-edit (ici-edit-close))))
      (#\D "delete" ,(lambda (_) (ici-edit (ici-edit-delete))))
      (#\d "delete outermost" ,(compose1 ici-edit ici-edit-trim))
      (#\t "toggle" ,(compose1 ici-edit ici-edit-toggle))
      (#\T "toggle reverse" ,(compose1 ici-edit ici-edit-toggle -))
      (#\a "wrap lam" ,(compose ici-edit (curry ici-edit-wrap (ici-wrap-lam))))
      (#\A "wrap apply"
       ,(compose1 ici-edit (curry ici-edit-wrap (ici-wrap-apply))))
      (#\p "wrap pair"
       ,(compose1 ici-edit (curry ici-edit-wrap (ici-wrap-pair))))
      (#\P "wrap pair-access"
       ,(compose1 ici-edit (curry ici-edit-wrap (ici-wrap-pair-access))))
      (#\x "toggle-syntax" ,(lambda (_) (ici-toggle-syntax)))
      (#\u "undo" ,ici-undo))
    (list char desc
          (compose1 (curry interaction-command ws-name name) instr))))

(module+ test
  (require (submod "database.rkt" test-support))
  (define test-dbs (test-dbs-new interaction-widget))
  (define test-db-0 (list-ref test-dbs 0))
  (define test-db-1 (list-ref test-dbs 1))
  (check-equal?
    (list->string (map car (db->workspace-commands 'one test-db-1)))
    "neEqHLRhjkl SsCcDdtTaApPxu"
    ))

(module+ test
  (check-equal?
    (lets
      event->cmd = (event->workspace-command 'one)
      (list
        (event->cmd test-db-0 (event-keycount #\j 3))
        (event->cmd test-db-1 (event-keycount #\j 3))
        (event->cmd test-db-0 (event-keycount #\q 2))
        ))
    (list
      (nothing)
      (just (interaction-command 'one 1 (ici-traverse-down 3)))
      (just (workspace-command 'one (wci-widget-close 2)))
      )))

(define (commands->desc commands)
  (forl
    (list char desc action) <- commands
    (list (list->string (list char)) desc)))
(def (workspace-preview name db)
  ws = (:.* db 'workspaces name)
  (list widgets fidx msg) = (map (curry :.* ws)
                                 '(layout focus-index notification))
  cmds = (db->workspace-commands name db)
  cmd-desc = (commands->desc cmds)
  idocs =
  (forl
    (interaction-widget iname) <- widgets
    iaction = (:.* db 'interactions iname)
    (list stx nav) = (map (curry :.* iaction) '(syntax nav))
    nav->doc = (match stx
                 ((isyntax-raw) nav-term-lifted-old->doc)
                 ((isyntax-pretty) nav-term-lifted->doc))
    (delay (nav->doc nav)))
  (list msg cmd-desc fidx idocs))
(def (workspace-preview->str-thunk (list msg cmd-desc fidx idocs))
  (thunk (view->string (tabular-view msg cmd-desc fidx idocs))))

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

(define (ui-loop ws-name db)
  (define event-chan (make-channel))
  (define display-chan (make-channel))
  (define (check-quit db)
    (if (empty? (:.* db 'workspaces ws-name 'layout))
      (gen-result "quitting: all interactions closed")
      (gen-susp db check-quit)))
  (define event->cmd (event->workspace-command ws-name))
  (define handle-events
    (gn yield (event)
      (letn loop (values db (event-keypress char)) = (values db event)
        (values db result) =
        (:** db
          kpm-path = `(workspaces ,ws-name keypress-mode)
          :. kpmode kpm-path
          (values kpmode result) = (keypress-add kpmode char)
          := kpmode kpm-path
          := (match result ((left msg) msg) ((right _) ""))
            `(workspaces ,ws-name notification)
          result)
        db = (match result
               ((left _) db)
               ((right event)
                (match (event->cmd db event)
                  ((nothing)
                   (:=* db "invalid choice" 'workspaces ws-name 'notification))
                  ((just cmd) (editor-update cmd db)))))
        (loop db (yield db)))))
  (with-cursor-hidden (with-stty-direct
    (lets
      threads = (list (keypress-thread event-chan)
                      (display-view-thread 0.1 display-chan))
      result =
      (gen-loop
        (gen-compose*
          handle-events
          (fn->gen (lambda (_) (channel-get event-chan)))
          (fn->gen (curry channel-put display-chan))
          (fn->gen workspace-preview->str-thunk)
          (gn yield (db)
              (letn loop (values cur-preview db) =
                         (values (make-list 4 (void)) db)
                preview = (workspace-preview ws-name db)
                (loop preview (yield preview))))
          check-quit)
        db)
      _ = (for-each kill-thread threads)
      result))))

(def (interact-with terms)
  ws-name = 'terminal-ui:interact-with
  iactions = (map interaction-new terms)
  ws = (workspace-new (map interaction-widget (range (length terms))))
  db = (:** database-empty
         := ws `(workspaces ,ws-name)
         :~+ (list->index-dict iactions) '(interactions))
  (ui-loop ws-name db))
