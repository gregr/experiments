#lang racket
(provide
  )

(require
  gregr-misc/cursor
  gregr-misc/dict
  gregr-misc/list
  gregr-misc/maybe
  gregr-misc/monad
  gregr-misc/record
  gregr-misc/sugar
  )

(module+ test
  (require rackunit))

(record database workspaces interactions) ; {workspaces: {name => workspace}, interactions: {name => interaction}, ...}
(record workspace layout focus-index widgets notification) ; {layout: [name], focus-index: nat, widgets: {name => widget}, notification: string}
(record widget db->commands db->doc)
(define database-empty (database (hash) (hash)))
(define workspace-empty (workspace '() 0 (hash) ""))

;workspace->doc ; workspace -> db -> doc

(def (workspace->focus-widget ws)
  (list layout fidx widgets) =
  (map (curry :.* ws) '(layout focus-index widgets))
  (begin/with-monad maybe-monad
    name <- (list-get layout fidx)
    (dict-get widgets name)))
(define (workspace->focus-commands ws db)
  (maybe-fold '() (fn ((widget db->cmds _)) (db->cmds db))
              (workspace->focus-widget ws)))

(record workspace-command name instr)
(records workspace-instruction
  (wci-widget-left count)
  (wci-widget-right count)
  (wci-widget-reverse count))
(define (db->workspace-commands-top name db)
  ; TODO: specialized commands based on workspace state
  ;ws = (:.* db 'workspaces name)
  (define (cmd-new instr) (workspace-command name instr))
  `((#\H "pane left" ,(lambda (count) (cmd-new (wci-widget-left count))))
    (#\L "pane right" ,(lambda (count) (cmd-new (wci-widget-right count))))
    (#\R "pane reverse" ,(lambda (count)
                           (cmd-new (wci-widget-reverse count))))))
(def (db->workspace-commands name db)
  ws = (:.* db 'workspaces name)
  ws-top-cmds = (db->workspace-commands-top name db)
  widget-cmds = (workspace->focus-commands ws db)
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

(module+ test
  (lets
    test-db-0 = (:=* database-empty (hash 'one workspace-empty) 'workspaces)
    test-ws-0 = (workspace
                  (range 3) 1
                  (list->index-dict (map interaction-widget (range 3)))
                  "")
    test-db-1 = (:=* (:=* test-db-0 test-ws-0 'workspaces 'one)
                     (list->index-dict (range 3)) 'interactions)
    _ = (check-equal?
      (map list-init (db->workspace-commands 'one test-db-0))
      (map list-init (db->workspace-commands-top 'one test-db-0))
      )
    (check-equal?
      (list->string (map car (db->workspace-commands 'one test-db-1)))
      "HLRhjklSscxuq"
      )
    ))

(record interaction-command name instr)
(records interaction-instruction
  (ici-traverse-up count)
  (ici-traverse-down count)
  (ici-traverse-left count)
  (ici-traverse-right count)
  (ici-substitute-complete)
  (ici-step count)
  (ici-step-complete)
  (ici-toggle-syntax)
  (ici-undo count)
  (ici-quit))
(define ((interaction->commands name) interaction)
  ; TODO: specialized commands based on interaction state
  (define (cmd-new instr) (interaction-command name instr))
  `((#\h "traverse left"
     ,(lambda (count) (cmd-new (ici-traverse-left count))))
    (#\j "traverse down"
     ,(lambda (count) (cmd-new (ici-traverse-down count))))
    (#\k "traverse up"
     ,(lambda (count) (cmd-new (ici-traverse-up count))))
    (#\l "traverse right"
     ,(lambda (count) (cmd-new (ici-traverse-right count))))
    (#\S "substitute completely"
     ,(lambda (count) (cmd-new (ici-substitute-complete))))
    (#\s "step" ,(lambda (count) (cmd-new (ici-step count))))
    (#\c "step completely" ,(lambda (count) (cmd-new (ici-step-complete))))
    (#\x "toggle-syntax" ,(lambda (count) (cmd-new (ici-toggle-syntax))))
    (#\u "undo" ,(lambda (count) (cmd-new (ici-undo count))))
    (#\q "quit" ,(lambda (count) (cmd-new (ici-quit))))))

(define (interaction->doc interaction) (void))

(define (interaction-widget name)
  (define ((with-interaction f) db)
    (f (:.* db 'interactions name)))
  (widget (with-interaction (interaction->commands name))
          (with-interaction (interaction->doc name))))
