#lang racket
(provide
  database-empty
  database-update
  interaction-command
  workspace-command
  )

(require
  "interaction-model.rkt"
  "workspace-model.rkt"
  gregr-misc/cursor
  gregr-misc/record
  gregr-misc/sugar
  )

(record database workspaces interactions) ; {workspaces: {name => workspace}, interactions: {name => interaction}, ...}
(define database-empty (database (hash) (hash)))

(record workspace-command name instr)
(record interaction-command ws-name name instr)

(define ((database-update cmd) db)
  (match cmd
    ((workspace-command name instr)
     (:~* db (curry workspace-update instr) 'workspaces name))
    ((interaction-command ws-name name instr)
     (lets
       iaction = (:.* db 'interactions name)
       (list msg iaction) = (interaction-update instr iaction)
       db = (:=* db iaction 'interactions name)
       ws = (:=* (:.* db 'workspaces ws-name) msg 'notification)
       (:=* db ws 'workspaces ws-name)))))
