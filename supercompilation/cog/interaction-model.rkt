#lang racket
(provide
  ici-traverse-down
  ici-traverse-left
  ici-traverse-right
  ici-traverse-up
  ici-substitute-complete
  ici-step
  ici-step-complete
  ici-toggle-syntax
  ici-undo
  )

(require
  gregr-misc/record
  )

(records interaction-instruction
  (ici-traverse-down count)
  (ici-traverse-left count)
  (ici-traverse-right count)
  (ici-traverse-up count)
  (ici-substitute-complete)
  (ici-step count)
  (ici-step-complete)
  (ici-toggle-syntax)
  (ici-undo count))
