#lang racket
(provide
  )

(require
  gregr-misc/record
  )

(record database workspaces interactions) ; {workspaces: {name => workspace}, interactions: {name => interaction}, ...}
(record workspace panel panes) ; {panel: panel, panes: {name => pane}}
(record panel layout focus-index notification) ; {layout: [name], focus-index: nat, notification: string}
(record pane db->commands db->doc)

;workspace->doc ; workspace -> db -> doc; uses panel-focus->commands
  ;panel-focus->commands ; panel -> panes -> db -> commands
;panel->commands ; panel -> commands

; TODO
(define (interaction->commands interaction) (void))
(define (interaction->doc interaction) (void))

(define (interaction-pane name)
  (define ((with-interaction f) db)
    (f (:.* db 'interactions name)))
  (pane (with-interaction interaction->commands)
        (with-interaction interaction->doc)))
