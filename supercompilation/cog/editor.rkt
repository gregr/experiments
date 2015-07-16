#lang racket
(provide
  editor-command
  editor-update
  interaction-widget
  )

(require
  "database.rkt"
  "interaction-model.rkt"
  "workspace-model.rkt"
  gregr-misc/record
  )

(record interaction-widget name)

(record editor-command ws-name instr)

(define (editor-update cmd db)
  (match cmd
    (_ (database-update cmd db))))
