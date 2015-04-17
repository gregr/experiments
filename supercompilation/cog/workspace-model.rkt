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
