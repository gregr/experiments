#lang racket
(provide
  eci-interaction-new
  editor-command
  editor-update
  interaction-widget
  )

(require
  "database.rkt"
  "interaction-model.rkt"
  "workspace-model.rkt"
  gregr-misc/cursor
  gregr-misc/record
  )

(record interaction-widget name)

(record editor-command ws-name instr)

(records editor-instruction
  (eci-interaction-new offset)
  )

(define (editor-update cmd db)
  (match cmd
    ((editor-command ws-name instr)
     (match instr
       ((eci-interaction-new offset)
        (:** db
          :. ias                          '(interactions)
          name = (+ 1 (apply max -1 (dict-keys ias)))
          wci = (wci-widget-add (interaction-widget name) offset)
          := interaction-empty            `(interactions ,name)
          :~ (curry workspace-update wci) `(workspaces ,ws-name)))
       ))
    (_ (database-update cmd db))))
