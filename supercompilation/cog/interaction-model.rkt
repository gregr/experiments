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
  isyntax-raw
  isyntax-pretty
  interaction-new
  interaction-update
  )

(module+ test-support
  (provide
    test-iactions
    ))

(require
  "substitution.rkt"
  "semantics-operational.rkt"
  "syntax.rkt"
  "syntax-abstract.rkt"
  gregr-misc/cursor
  gregr-misc/either
  gregr-misc/list
  gregr-misc/maybe
  gregr-misc/monad
  gregr-misc/navigator
  gregr-misc/record
  gregr-misc/sugar
  )

(module+ test
  (require
    gregr-misc/maybe
    rackunit
    ))

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

(def (subst-keys (substitution uses _))
  use-keys =
  (forl
    use-list-key <- (list-navigator-keys uses)
    (append '(s uses) use-list-key '(v)))
  (list* '(t) use-keys))
(def (app-keys app)
  count = (- (length (gather-applications app)) 2)
  rargs = (iterate (fn (path) (list* 'proc path)) '(arg) count)
  args = (reverse rargs)
  proc = (make-list (+ 1 count) 'proc)
  (list* proc args))
(define (hole-keys focus)
  (match focus
    ((lam-apply _ _) (app-keys focus))
    ((value v)       (map (fn (key) (list* 'v key)) (hole-keys v)))
    ((subst sub _)   (subst-keys sub))
    ((lam _ _)       '((body)))
    (_ (if (or (term? focus) (pair? focus))
         (map list (dict-keys focus)) '()))))
(define (navterm-new term) (navigator-new hole-keys term))

(def (navterm-binder-count nav)
  (list foci paths) =
  (zip-default '(() ())
               (forl (list focus mpath) <- (navigator-path nav)
                     (list focus (match mpath
                                   ((nothing) '())
                                   ((just (list _ path)) path)))))
  path = (append* paths)
  (list env) = (nav-paths->binders binders-empty (first foci) (list path))
  (length (binders-names env)))

(define test-terms
  (list (value (uno))
        (lam-apply (value (lam (lattr-name 'v) (value (bvar 0))))
                   (value (bit (b-1))))))
(define test-navterm-out (navterm-new (list-ref test-terms 1)))
(define test-navterm-in
  (lets
    nav = test-navterm-out
    (just nav) = (navigator-descend nav 0)
    (just nav) = (navigator-descend nav 0)
    nav))

(module+ test
  (check-equal?
    (navterm-binder-count test-navterm-in)
    1)
  (check-equal?
    (navterm-binder-count test-navterm-out)
    0)
  )

(define v-uno (uno))
(define t-uno (value v-uno))
(define v-0 (bit (b-0)))
(define t-0 (value v-0))
(define v-1 (bit (b-1)))
(define t-1 (value v-1))
(define v-uno-pair (pair v-uno v-uno))
(define t-uno-pair (value v-uno-pair))
(define v-uno-lam (lam lattr-void t-uno))
(define t-uno-lam (value v-uno-lam))
(define t-uno-apply (lam-apply t-uno-lam t-uno))
(define t-uno-pair-access (pair-access v-0 v-uno-pair))

(def (navterm-delete nav)
  focus = (navigator-focus nav)
  (if (term? focus) t-uno v-uno))

(module+ test
  (check-equal?
    (navterm-delete test-navterm-in)
    t-uno))

; toggle, wrap, trim
; extract/copy, paste/replace, rename
; jump to binder

(record interaction syntax history nav)
(records interaction-syntax
  (isyntax-raw)
  (isyntax-pretty))
(define (interaction-new term)
  (interaction (isyntax-pretty) '() (navterm-new term)))

(define (interaction-update instr iaction)
  (def (trans f seed count)
    (list prev final success?) =
    (forf
      (list prev final success?) = (list (right seed) seed #f)
      idx <- (range count)
      #:break (left? prev)
      current = (right-x prev)
      (list (f current) current (> idx 0)))
    success? = (or (right? prev) success?)
    (either-fold (fn (msg) (list success? msg final))
                 (fn (ia)  (list success? "" ia)) prev))
  (define (mtrans msg f seed count)
    (define (g arg) (maybe->either msg (f arg)))
    (trans g seed count))
  (def (ttrans-nav tt history f iaction count)
    (list success? msg nav) = (tt f (interaction-nav iaction) count)
    iaction = (:=* iaction nav 'nav)
    (list msg (if success? iaction (:=* iaction history 'history))))
  (define trans-nav (curry ttrans-nav trans))
  (define (mtrans-nav msg history f iaction count)
    (ttrans-nav (curry mtrans msg) history f iaction count))
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
       (mtrans-nav "cannot traverse down" history
                   (fn (nav) (navigator-descend nav 0)) iaction count))
      ((ici-traverse-left count)
       (mtrans-nav "cannot traverse left" history
                   (fn (nav) (navigator-shift nav (- 1))) iaction count))
      ((ici-traverse-right count)
       (mtrans-nav "cannot traverse right" history
                   (fn (nav) (navigator-shift nav 1)) iaction count))
      ((ici-traverse-up count)
       (mtrans-nav "cannot traverse up" history
                   navigator-ascend iaction count))
      ((ici-substitute-complete)
       (trans-nav history (trans-focus (compose1 right substitute-full))
                  iaction 1))
      ((ici-step count)
       (trans-nav history (trans-focus step-safe) iaction count))
      ((ici-step-complete)
       (trans-nav history (trans-focus step-complete-safe) iaction 1))
      ((ici-toggle-syntax)
       (list "" (:=* iaction (match syntax
                               ((isyntax-raw) (isyntax-pretty))
                               ((isyntax-pretty) (isyntax-raw)))
                     'syntax)))
      ((ici-undo count)
       (match history
         ('() (list "nothing to undo" iaction-current))
         (_ (list "" (car (drop history (- (min count (length history))
                                           1))))))))))

(define test-iactions (map interaction-new test-terms))

(module+ test
  (lets
    ia-term = (fn (ia) (navigator-focus (:.* ia 'nav)))
    (void (forl
      ia <- test-iactions
      (begin
        (check-equal?
          (ia-term (second (interaction-update (ici-step-complete) ia)))
          (step-complete (ia-term ia)))
        (check-equal?
          (lets
            (list msg ia) = (interaction-update (ici-traverse-down 1) ia)
            (if (equal? msg "") (just (ia-term ia)) (nothing)))
          (maybe-map navigator-focus (navigator-descend (:.* ia 'nav)))))))
    ))
