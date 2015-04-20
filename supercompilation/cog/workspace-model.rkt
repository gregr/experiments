#lang racket
(provide
  wci-widget-left
  wci-widget-right
  wci-widget-reverse
  wci-widget-close
  workspace
  workspace->focus-widget
  workspace-empty
  workspace-update
  )

(require
  gregr-misc/cursor
  gregr-misc/dict
  gregr-misc/list
  gregr-misc/maybe
  gregr-misc/monad
  gregr-misc/record
  gregr-misc/sugar
  )

(module+ test
  (require
    "syntax-abstract.rkt"
    gregr-misc/navigator
    rackunit
    ))

(record workspace layout focus-index widgets notification) ; {layout: [name], focus-index: nat, widgets: {name => widget}, notification: string}
(define workspace-empty (workspace '() 0 (hash) ""))

;workspace->doc ; workspace -> db -> doc

(def (workspace->focus-widget ws)
  (list layout fidx widgets) =
  (map (curry :.* ws) '(layout focus-index widgets))
  (begin/with-monad maybe-monad
    name <- (list-get layout fidx)
    (dict-get widgets name)))

(records workspace-instruction
  (wci-widget-left count)
  (wci-widget-right count)
  (wci-widget-reverse count)
  (wci-widget-close count))

(define ((workspace-update instr) ws)
  (define (focus-index-valid layout idx)
    (max 0 (min (- (length layout) 1) idx)))
  (define (end-index-valid layout idx) (max 0 (min (length layout) idx)))
  (def (workspace-valid ws)
    layout = (:.* ws 'layout)
    (:~* ws (curry focus-index-valid layout) 'focus-index))
  (lets
    (workspace layout fidx widgets _) = ws
    (workspace-valid
      (match instr
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
