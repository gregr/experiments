#lang racket
(provide
  )

(require
  gregr-misc/record
  )

(record database workspaces interactions) ; {workspaces: {name => workspace}, interactions: {name => interaction}, ...}
(record workspace panel panes) ; {panel: panel, panes: {name => pane}}
(record panel layout focus-index notification) ; {layout: [name], focus-index: nat, notification: string}
(record pane db->commands db->doc) ; interaction-panes close over an interaction-name

;workspace->doc ; workspace -> db -> doc; uses panel-focus->commands
  ;panel-focus->commands ; panel -> panes -> db -> commands
;panel->commands ; panel -> commands
