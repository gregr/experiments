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

(records workspace-command
  (workspace-widget-left name count)
  (workspace-widget-right name count)
  (workspace-widget-reverse name count))
(define (db->workspace-commands-top name db)
  ; TODO: specialized commands based on workspace state
  ;ws = (:.* db 'workspaces name)
  `((#\H "pane left" ,(lambda (count) (workspace-widget-left name count)))
    (#\L "pane right" ,(lambda (count) (workspace-widget-right name count)))
    (#\R "pane reverse" ,(lambda (count)
                           (workspace-widget-reverse name count)))))
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

(records interaction-command
  (interaction-traverse-up name count)
  (interaction-traverse-down name count)
  (interaction-traverse-left name count)
  (interaction-traverse-right name count)
  (interaction-substitute-complete name)
  (interaction-step name count)
  (interaction-step-complete name)
  (interaction-toggle-syntax name)
  (interaction-undo name count)
  (interaction-quit name))
(define ((interaction->commands name) interaction)
  ; TODO: specialized commands based on interaction state
  `((#\h "traverse left"
     ,(lambda (count) (interaction-traverse-left name count)))
    (#\j "traverse down"
     ,(lambda (count) (interaction-traverse-down name count)))
    (#\k "traverse up"
     ,(lambda (count) (interaction-traverse-up name count)))
    (#\l "traverse right"
     ,(lambda (count) (interaction-traverse-right name count)))
    (#\S "substitute completely"
     ,(lambda (count) (interaction-substitute-complete name)))
    (#\s "step" ,(lambda (count) (interaction-step name count)))
    (#\c "step completely" ,(lambda (count) (interaction-step-complete name)))
    (#\x "toggle-syntax" ,(lambda (count) (interaction-toggle-syntax name)))
    (#\u "undo" ,(lambda (count) (interaction-undo name count)))
    (#\q "quit" ,(lambda (count) (interaction-quit name)))))

(define (interaction->doc interaction) (void))

(define (interaction-widget name)
  (define ((with-interaction f) db)
    (f (:.* db 'interactions name)))
  (widget (with-interaction (interaction->commands name))
          (with-interaction (interaction->doc name))))
