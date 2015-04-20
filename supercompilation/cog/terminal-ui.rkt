#lang racket
(provide
  )

(require
  "database.rkt"
  "interaction-model.rkt"
  "workspace-model.rkt"
  gregr-misc/cursor
  gregr-misc/dict
  gregr-misc/list
  gregr-misc/maybe
  gregr-misc/monad
  gregr-misc/record
  gregr-misc/sugar
  gregr-misc/ui
  )

(module+ test
  (require
    "syntax-abstract.rkt"
    gregr-misc/navigator
    rackunit
    ))

(record interaction-widget name)

(define (commands->keymap commands)
  (make-immutable-hash
    (forl
      (list char desc cmd) <- commands
      (cons char cmd))))

(define (workspace->focus-commands ws-name ws db)
  (maybe-fold '() (fn ((interaction-widget name))
                      (interaction->commands ws-name name db))
              (workspace->focus-widget ws)))

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
  (cmds-merge1 ws-top-cmds widget-cmds))

(define (event->workspace-command ws-name)
  (fn (db (event-keycount char count))
    cmds = (db->workspace-commands ws-name db)
    keymap = (commands->keymap cmds)
    (begin/with-monad maybe-monad
      cmd-new <- (dict-get keymap char)
      (pure (cmd-new count)))))

(define (interaction->commands ws-name name db)
  ; TODO: specialized commands based on interaction state
  (forl
    (list char desc instr) <-
    `((#\h "traverse left" ,ici-traverse-left)
      (#\j "traverse down" ,ici-traverse-down)
      (#\k "traverse up" ,ici-traverse-up)
      (#\l "traverse right" ,ici-traverse-right)
      (#\S "substitute completely" ,(lambda (_) (ici-substitute-complete)))
      (#\s "step" ,ici-step)
      (#\c "step completely" ,(lambda (_) (ici-step-complete)))
      (#\x "toggle-syntax" ,(lambda (_) (ici-toggle-syntax)))
      (#\u "undo" ,ici-undo))
    (list char desc
          (compose1 (curry interaction-command ws-name name) instr))))

(module+ test
  (define test-term-0 (value (uno)))
  (define test-term-1 (lam-apply (value (lam (lattr-name 'v) (value (bvar 0))))
                                 (value (bit (b-1)))))
  (define test-iaction-0 (interaction-new test-term-0))
  (define test-iaction-1 (interaction-new test-term-1))
  (define test-db-0
    (:=* database-empty (hash 'one workspace-empty) 'workspaces))
  (define test-ws-range-len 7)
  (define test-ws-range-0 (range test-ws-range-len))
  (define test-ws-0
    (workspace-new (map interaction-widget test-ws-range-0) 1))
  (define test-db-1 (:=* (:=* test-db-0 test-ws-0 'workspaces 'one)
                         (:=* (make-immutable-hash
                                (forl idx <- test-ws-range-0
                                      (cons idx test-iaction-0)))
                              test-iaction-1 1)
                         'interactions)))

(module+ test
  (check-equal?
    (map list-init (db->workspace-commands 'one test-db-0))
    (map list-init (db->workspace-commands-top 'one test-db-0))
    )
  (check-equal?
    (list->string (map car (db->workspace-commands 'one test-db-1)))
    "qHLRhjklSscxu"
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

(module+ test
  (check-equal?
    ((database-update (workspace-command 'one (wci-widget-right 2))) test-db-0)
    test-db-0)
  (check-equal?
    (lets
      instrs = (list (wci-widget-left 10) (wci-widget-left 0)
                     (wci-widget-right 2) (wci-widget-right 10)
                     (wci-widget-reverse 4) (wci-widget-reverse 20)
                     (wci-widget-close 3) (wci-widget-close 20))
      updaters = (map (fn (instr) (database-update
                                    (workspace-command 'one instr))) instrs)
      dbs = (list* test-db-1 (map (fn (upd) (upd test-db-1)) updaters))
      (forl db <- dbs
            ws = (:.* db 'workspaces 'one)
            (list fidx layout widgets) =
            (map (curry :.* ws) '(focus-index layout widgets))
            (list fidx layout (sort (dict-keys widgets) <))))
    (lets
      layouts = (append (make-list 5 test-ws-range-0)
                        (list (append (list 0)
                                      (rest (reverse (rest test-ws-range-0)))
                                      (list 6))
                              (list* 0 (reverse (rest test-ws-range-0))))
                        '((0 4 5 6) (0)))
      (zip* (list 1 0 1 3 6 1 1 1 0) layouts (forl l <- layouts (sort l <))))
    )
  (check-equal?
    (lets
      cmds = (list (interaction-command 'one 1 (ici-step-complete))
                   (interaction-command 'one 2 (ici-traverse-down 1)))
      updaters = (map database-update cmds)
      dbs = (map (fn (upd) (upd test-db-1)) updaters)
      (forl db <- dbs
            (list (:.* db 'workspaces 'one 'notification)
                  (forl
                    name <- '(1 2)
                    iaction = (:.* db 'interactions name)
                    nav = (:.* iaction 'nav)
                    (navigator-focus nav)))))
    (list (list "" (list (value (bit (b-1))) test-term-0))
          (list "cannot traverse down" (list test-term-1 test-term-0)))
    ))
