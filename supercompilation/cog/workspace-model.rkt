#lang racket
(provide
  )

(require
  gregr-misc/cursor
  gregr-misc/record
  )

(record database workspaces interactions) ; {workspaces: {name => workspace}, interactions: {name => interaction}, ...}
(record workspace layout focus-index widgets notification) ; {layout: [name], focus-index: nat, widgets: {name => widget}, notification: string}
(record widget db->commands db->doc)

;workspace->doc ; workspace -> db -> doc

; TODO
(define (interaction->commands interaction) (void))
(define (interaction->doc interaction) (void))

(define (interaction-widget name)
  (define ((with-interaction f) db)
    (f (:.* db 'interactions name)))
  (widget (with-interaction interaction->commands)
          (with-interaction interaction->doc)))
