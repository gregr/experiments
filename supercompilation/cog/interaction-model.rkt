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
  interaction-update
  )

(require
  "substitution.rkt"
  "semantics-operational.rkt"
  gregr-misc/cursor
  gregr-misc/either
  gregr-misc/monad
  gregr-misc/navigator
  gregr-misc/record
  gregr-misc/sugar
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

(record interaction syntax history nav)
(records interaction-syntax
  (isyntax-raw)
  (isyntax-pretty))

(define ((interaction-update instr) iaction)
  (def (trans f seed count)
    (list prev final) =
    (forf
      (list prev final) = (list (right seed) seed)
      _ <- (range count)
      #:break (left? prev)
      current = (right-x prev)
      (list (f current) current))
    (either-fold (fn (msg) (list msg final)) (fn (ia) (list "" ia)) prev))
  (define (mtrans msg f seed count)
    (define (g arg) (maybe->either msg (f arg)))
    (trans g seed count))
  (def (ttrans-nav tt f iaction count)
    (list msg nav) = (tt f (interaction-nav iaction) count)
    (list msg (:=* iaction nav 'nav)))
  (define trans-nav (curry ttrans-nav trans))
  (define (mtrans-nav msg f iaction count)
    (ttrans-nav (curry mtrans msg) f iaction count))
  (define ((trans-focus f) nav)
    (begin/with-monad either-monad
      new-focus <- (f (navigator-focus nav))
      (pure (navigator-focus-set nav new-focus))))
  (lets
    (interaction syntax history nav) = iaction
    iaction-current = iaction
    iaction = (:~* iaction (curry cons iaction) 'history)
    (match instr
      ((ici-traverse-down count)
       (mtrans-nav "cannot traverse down"
                   (fn (nav) (navigator-descend nav 0)) iaction count))
      ((ici-traverse-left count)
       (mtrans-nav "cannot traverse left"
                   (fn (nav) (navigator-shift nav (- 1))) iaction count))
      ((ici-traverse-right count)
       (mtrans-nav "cannot traverse right"
                   (fn (nav) (navigator-shift nav 1)) iaction count))
      ((ici-traverse-up count)
       (mtrans-nav "cannot traverse up" navigator-ascend iaction count))
      ((ici-substitute-complete)
       (trans-nav (trans-focus (compose1 right substitute-full)) iaction 1))
      ((ici-step count) (trans-nav (trans-focus step-safe) iaction count))
      ((ici-step-complete) (trans-nav (trans-focus step-complete) iaction 1))
      ((ici-toggle-syntax)
       (list "" (:=* iaction (match syntax
                               ((isyntax-raw) (isyntax-pretty))
                               ((isyntax-pretty) (isyntax-raw)))
                     'syntax)))
      ((ici-undo count)
       (match history
         ('() (list "nothing to undo" iaction-current))
         (_ (list "" (:=* iaction (drop history (min count (length history)))
                          'history))))))))