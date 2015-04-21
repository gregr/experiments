#lang racket
(provide
  database-empty
  database-update
  interaction-command
  workspace-command
  )

(module+ test-support
  (provide
    test-dbs-new
    ))

(require
  "interaction-model.rkt"
  "workspace-model.rkt"
  (submod "interaction-model.rkt" test-support)
  gregr-misc/cursor
  gregr-misc/list
  gregr-misc/record
  gregr-misc/sugar
  )

(module+ test
  (require rackunit))

(record database workspaces interactions) ; {workspaces: {name => workspace}, interactions: {name => interaction}, ...}
(define database-empty (database (hash) (hash)))

(record workspace-command name instr)
(record interaction-command ws-name name instr)

(define (database-update cmd db)
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

(define (test-dbs-new widget-new)
  (define widget-count 7)
  (define widgets (map widget-new (range widget-count)))
  (define ia-0 (list-ref test-iactions 0))
  (define ia-1 (list-ref test-iactions 1))
  (define db (:=* database-empty (hash 'one workspace-empty) 'workspaces))
  (list db
        (:=* (:=* db (workspace-new widgets 1) 'workspaces 'one)
             (:=* (list->index-dict (make-list widget-count ia-0)) ia-1 1)
             'interactions)
        ))

(module+ test
  (require (submod "workspace-model.rkt" test-support))
  (define test-dbs (test-dbs-new identity))
  (define test-db-0 (list-ref test-dbs 0))
  (define test-db-1 (list-ref test-dbs 1))
  (check-equal?
    (database-update (workspace-command 'one (wci-widget-right 2)) test-db-0)
    test-db-0)
  (void (forl
    ws <- test-workspaces
    path = (list 'workspaces 'one)
    db = (:= test-db-1 ws path)
    (forl
      instr <- test-instrs
      cmd = (workspace-command 'one instr)
      db = (database-update cmd db)
      (check-equal?
        (:. db path)
        (workspace-update instr ws)))))
  (void (forl
    ia <- test-iactions
    path = (list 'interactions 3)
    db = (:= test-db-1 ia path)
    (forl
      instr <- (list (ici-step-complete) (ici-traverse-down 1))
      cmd = (interaction-command 'one 3 instr)
      db = (database-update cmd db)
      (check-equal?
        (list (:.* db 'workspaces 'one 'notification) (:. db path))
        (interaction-update instr ia)))))
  )