#lang racket
(provide
  )

(require
  gregr-misc/cursor
  gregr-misc/record
  )

(record database workspaces interactions) ; {workspaces: {name => workspace}, interactions: {name => interaction}, ...}
(record workspace layout focus-index panes notification) ; {layout: [name], focus-index: nat, panes: {name => pane}, notification: string}
(record pane db->commands db->doc)

;workspace->doc ; workspace -> db -> doc

; TODO
(define (interaction->commands interaction) (void))
(define (interaction->doc interaction) (void))

(define (interaction-pane name)
  (define ((with-interaction f) db)
    (f (:.* db 'interactions name)))
  (pane (with-interaction interaction->commands)
        (with-interaction interaction->doc)))
