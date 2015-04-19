#lang racket
(provide
  )

(require
  "interaction-model.rkt"
  gregr-misc/cursor
  gregr-misc/dict
  gregr-misc/list
  gregr-misc/maybe
  gregr-misc/monad
  gregr-misc/record
  gregr-misc/sugar
  gregr-misc/ui
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
(define (workspace->focus-commands ws-name ws db)
  (maybe-fold '() (fn ((interaction-widget name))
                      (interaction->commands ws-name name db))
              (workspace->focus-widget ws)))

(record workspace-command name instr)
(records workspace-instruction
  (wci-widget-left count)
  (wci-widget-right count)
  (wci-widget-reverse count)
  (wci-widget-close count))
(def (db->workspace-commands-top name db)
  ; TODO: specialized commands based on workspace state
  ;ws = (:.* db 'workspaces name)
  cmd-table =
  `((#\q "pane close" ,wci-widget-close)
    (#\H "pane left" ,wci-widget-left)
    (#\L "pane right" ,wci-widget-right)
    (#\R "pane reverse" ,wci-widget-reverse))
  (forl
    (list char desc instr) <- cmd-table
    (list char desc (compose1 (curry workspace-command name) instr))))
(def (db->workspace-commands name db)
  ws = (:.* db 'workspaces name)
  ws-top-cmds = (db->workspace-commands-top name db)
  widget-cmds = (workspace->focus-commands name ws db)
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
  (define test-db-0
    (:=* database-empty (hash 'one workspace-empty) 'workspaces))
  (define test-ws-range-0 (range 7))
  (define test-ws-0
    (workspace test-ws-range-0 1
               (list->index-dict (map interaction-widget test-ws-range-0)) ""))
  (define test-db-1 (:=* (:=* test-db-0 test-ws-0 'workspaces 'one)
                         (list->index-dict test-ws-range-0) 'interactions)))

(module+ test
  (check-equal?
    (map list-init (db->workspace-commands 'one test-db-0))
    (map list-init (db->workspace-commands-top 'one test-db-0))
    )
  (check-equal?
    (list->string (map car (db->workspace-commands 'one test-db-1)))
    "qHLRhjklSscxu"
    ))

(define (commands->keymap commands)
  (make-immutable-hash
    (forl
      (list char desc cmd) <- commands
      (cons char cmd))))

(define (event->workspace-command ws-name)
  (fn (db (event-keycount char count))
    cmds = (db->workspace-commands ws-name db)
    keymap = (commands->keymap cmds)
    (begin/with-monad maybe-monad
      cmd-new <- (dict-get keymap char)
      (pure (cmd-new count)))))

(module+ test
  (check-equal?
    (lets
      event->cmd = (event->workspace-command 'one)
      (list
        (event->cmd test-db-0 (event-keycount #\j 3))
        (event->cmd test-db-1 (event-keycount #\j 3))
        (event->cmd test-db-0 (event-keycount #\q 2))
        ))
    (list
      (nothing)
      (just (interaction-command 'one 1 (ici-traverse-down 3)))
      (just (workspace-command 'one (wci-widget-close 2)))
      )))

(record interaction-command ws-name name instr)
(define (interaction->commands ws-name name db)
  ; TODO: specialized commands based on interaction state
  (forl
    (list char desc instr) <-
    `((#\h "traverse left" ,ici-traverse-left)
      (#\j "traverse down" ,ici-traverse-down)
      (#\k "traverse up" ,ici-traverse-up)
      (#\l "traverse right" ,ici-traverse-right)
      (#\S "substitute completely" ,(lambda (_) (ici-substitute-complete)))
      (#\s "step" ,ici-step)
      (#\c "step completely" ,(lambda (_) (ici-step-complete)))
      (#\x "toggle-syntax" ,(lambda (_) (ici-toggle-syntax)))
      (#\u "undo" ,ici-undo))
    (list char desc
          (compose1 (curry interaction-command ws-name name) instr))))

(record interaction-widget name)

(define ((workspace-update cmd) ws)
  (define (focus-index-valid layout idx)
    (max 0 (min (- (length layout) 1) idx)))
  (define (end-index-valid layout idx) (max 0 (min (length layout) idx)))
  (def (workspace-valid ws)
    layout = (:.* ws 'layout)
    (:~* ws (curry focus-index-valid layout) 'focus-index))
  (lets
    (workspace layout fidx widgets _) = ws
    (workspace-valid
      (match cmd
        ((wci-widget-close count)
         (lets
           end = (end-index-valid layout (+ fidx count))
           closed = (list-range layout fidx end)
           widgets = (forf widgets = widgets
                           name <- closed
                           (dict-remove widgets name))
           ws = (:=* ws (list-range-remove layout fidx end) 'layout)
           (:=* ws widgets 'widgets)))
        ((wci-widget-left count) (:=* ws (- fidx count) 'focus-index))
        ((wci-widget-right count) (:=* ws (+ fidx count) 'focus-index))
        ((wci-widget-reverse count)
         (:=* ws (list-range-reverse
                   layout fidx (end-index-valid layout (+ fidx count 1)))
              'layout))))))

(define ((database-update cmd) db)
  (lets
    (list path trans) =
    (match cmd
      ((workspace-command name instr)
       (list (list 'workspaces name) (workspace-update instr))))
    (:~ db trans path)))

(module+ test
  (check-equal?
    ((database-update (workspace-command 'one (wci-widget-right 2))) test-db-0)
    test-db-0)
  (check-equal?
    (lets
      instrs = (list (wci-widget-left 10) (wci-widget-left 0)
                     (wci-widget-right 2) (wci-widget-right 10)
                     (wci-widget-reverse 4) (wci-widget-reverse 20)
                     (wci-widget-close 3) (wci-widget-close 20))
      updaters = (map (fn (instr) (database-update
                                    (workspace-command 'one instr))) instrs)
      dbs = (list* test-db-1 (map (fn (upd) (upd test-db-1)) updaters))
      (forl db <- dbs
            ws = (:.* db 'workspaces 'one)
            (list fidx layout widgets) =
            (map (curry :.* ws) '(focus-index layout widgets))
            (list fidx layout (sort (dict-keys widgets) <))))
    (lets
      layouts = (append (make-list 5 test-ws-range-0)
                        (list (append (list 0)
                                      (rest (reverse (rest test-ws-range-0)))
                                      (list 6))
                              (list* 0 (reverse (rest test-ws-range-0))))
                        '((0 4 5 6) (0)))
      (zip* (list 1 0 1 3 6 1 1 1 0) layouts (forl l <- layouts (sort l <))))
    ))
