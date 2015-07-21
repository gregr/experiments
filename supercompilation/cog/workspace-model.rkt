#lang racket
(provide
  focus-index-valid
  keypress-add
  keypress-cmd-mode
  wci-widget-add
  wci-widget-close
  wci-widget-left
  wci-widget-reverse
  wci-widget-right
  workspace->focus-widget
  workspace-empty
  workspace-new
  workspace-update
  )

(module+ test-support
  (provide
    test-instrs
    test-workspaces
    ))

(require
  gregr-misc/cursor
  gregr-misc/either
  gregr-misc/list
  gregr-misc/record
  gregr-misc/string
  gregr-misc/sugar
  )

(module+ test
  (require
    rackunit
    ))

(records keypress-mode
  (keypress-cmd-mode digits)
  )
(define keypress-mode-default (keypress-cmd-mode '()))

(define (keypress-add mode char)
  (define (digits->count digits)
    (if (empty? digits) 1 (string->number (list->string (reverse digits)))))
  (if (eq? #\u0003 char) (values keypress-mode-default (left "")) ; C-c
    (match mode
      ((keypress-cmd-mode digits)
       (if (char-numeric? char)
         (lets digits = (list* char digits)
               (values (keypress-cmd-mode digits)
                       (left (number->string (digits->count digits)))))
         (values (keypress-cmd-mode '())
                 (right (list char (digits->count digits))))))
)))

(record workspace layout focus-index notification keypress-mode) ; {layout: [widget], focus-index: nat, notification: string, keypress-mode: keypress-mode}
(define (workspace-new widgets (fidx 0) (msg ""))
  (workspace widgets fidx msg keypress-mode-default))
(define workspace-empty (workspace-new '() 0))

(define (focus-index-valid layout idx) (max 0 (min (- (length layout) 1) idx)))
(define (end-index-valid layout idx) (max 0 (min (length layout) idx)))
(define (workspace-valid ws)
  (:~* ws (curry focus-index-valid (:.* ws 'layout)) 'focus-index))

;workspace->doc ; workspace -> db -> doc

(def (workspace->focus-widget (workspace layout fidx _ _))
  (list-get layout fidx))

(records workspace-instruction
  (wci-widget-left count)
  (wci-widget-right count)
  (wci-widget-reverse count)
  (wci-widget-close count)
  (wci-widget-add widget offset))

(def (workspace-update instr ws)
  (workspace layout fidx _ _) = ws
  (workspace-valid
    (match instr
      ((wci-widget-add widget offset)
       (lets idx = (end-index-valid layout (+ fidx offset))
             (:=* ws (list-insert layout idx (list widget)) 'layout)))
      ((wci-widget-close count)
       (lets end = (end-index-valid layout (+ fidx count))
             (:=* ws (list-range-remove layout fidx end) 'layout)))
      ((wci-widget-left count) (:=* ws (- fidx count) 'focus-index))
      ((wci-widget-right count) (:=* ws (+ fidx count) 'focus-index))
      ((wci-widget-reverse count)
       (:=* ws (list-range-reverse
                 layout fidx (end-index-valid layout (+ fidx count 1)))
            'layout)))))

(define test-widgets (range 7))
(define test-workspaces (list workspace-empty (workspace-new test-widgets 2)))
(define test-instrs (list (wci-widget-left 10) (wci-widget-left 0)
                          (wci-widget-right 2) (wci-widget-right 10)
                          (wci-widget-reverse 4) (wci-widget-reverse 20)
                          (wci-widget-close 3) (wci-widget-close 20)))

(module+ test
  (void (forl
    instr <- test-instrs
    (check-equal?
      (workspace-update instr workspace-empty)
      workspace-empty)))
  (check-equal?
    (forl
      instr <- test-instrs
      ws = (workspace-update instr (workspace-new test-widgets 1))
      (map (curry :.* ws) '(focus-index layout)))
    (zip* (list 0 1 3 6 1 1 1 0)
          (append (make-list 4 test-widgets)
                  (list (append (list 0)
                                (rest (reverse (rest test-widgets)))
                                (list 6))
                        (list* 0 (reverse (rest test-widgets))))
                  '((0 4 5 6) (0)))))
  )
