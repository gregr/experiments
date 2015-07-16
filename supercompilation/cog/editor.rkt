#lang racket
(provide
  eci-interaction-new
  eci-paste-subterm
  editor-command
  editor-update
  interaction-widget
  )

(require
  "database.rkt"
  "interaction-model.rkt"
  "syntax-abstract.rkt"
  "workspace-model.rkt"
  gregr-misc/cursor
  gregr-misc/navigator
  gregr-misc/record
  )

(record interaction-widget name)

(record editor-command ws-name instr)

(records editor-instruction
  (eci-interaction-new offset)
  (eci-paste-subterm reverse? offset))

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
       ((eci-paste-subterm reverse? offset)
        (:** db
          :. layout `(workspaces ,ws-name layout)
          :. fidx `(workspaces ,ws-name focus-index)
          idx = (focus-index-valid layout (+ fidx offset))
          (interaction-widget src) = (list-ref layout idx)
          (interaction-widget tgt) = (list-ref layout fidx)
          (values src tgt) = (if reverse? (values src tgt) (values tgt src))
          ipath = `(interactions ,tgt)
          :. src-nav `(interactions ,src nav)
          :. ia ipath
          term = (navigator-focus src-nav)
          closed? = (set-empty? (term-frees-safe term))
          (list msg ia) = (if closed?
                            (interaction-update
                              (ici-edit (ici-edit-replace term)) ia)
                            (list "cannot paste subterm with free vars" ia))
          := ia ipath
          := msg `(workspaces ,ws-name notification)))))
    (_ (database-update cmd db))))
