#lang racket
(provide
  ici-edit
  ici-edit-close
  ici-edit-delete
  ici-edit-replace
  ici-edit-toggle
  ici-edit-trim
  ici-edit-wrap
  ici-wrap-apply
  ici-wrap-lam
  ici-wrap-pair
  ici-wrap-pair-access
  ici-rename-binder
  ici-traverse-binder
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
  interaction-empty
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

(define (as-value tv) (if (value? tv) (value-v tv) tv))

; TODO: support substitutions
(define ((rename-binder name) focus)
  (match focus
    ((lam (lattr _ sa sl) body) (right (lam (lattr name sa sl) body)))
    ((value val) (either-map value ((rename-binder name) val)))
    (_ (left "cannot rename non-binder"))))

; TODO: this is broken within assigned values of substitution uses
(define (navterm-ascend-binder nav idx)
  (begin/with-monad maybe-monad
    nav <- (navigator-ascend nav)
    idx = (match (as-value (navigator-focus nav))
            ((lam _ _) (- idx 1))
            ((subst s _) (- idx (subst-scope-size s)))
            (_ idx))
    (if (< idx 0) (pure nav) (navterm-ascend-binder nav idx))))

(define (navterm-traverse-binder nav)
  (begin/with-monad either-monad
    focus = (navigator-focus nav)
    idx <- (match focus
             ((bvar idx) (right idx))
             ((value (bvar idx)) (right idx))
             (_ (left "cannot traverse non-variable")))
    (maybe->either "cannot traverse free variable"
                   (navterm-ascend-binder nav idx))))

; does not produce an equivalent term in value context (cannot wrap-apply)
; is this desirable?
(def (navterm-close-over-free nav)
  focus = (navigator-focus nav)
  frees = (term-frees-safe focus)
  max-binder = (+ 1 (apply max -1 (set->list frees)))
  (forf focus = (last (iterate (wrap-lam-trans identity) focus max-binder))
        index <- (reverse (range max-binder))
        (wrap-apply focus (value (bvar index)))))

(def (navterm-replace nav term)
  focus = (navigator-focus nav)
  (if (term? focus)
    (if (term-value? term) (value term) term)
    (if (term-value? term) term
      (match term
        ((value val) val)
        (_ focus)))))

(define (navterm-delete nav) (navterm-replace nav t-uno))

(module+ test
  (check-equal?
    (navterm-delete test-navterm-in)
    t-uno))

(def (value-shift-by val binder-count offset)
  targets = (append (list v-uno v-0 v-1) (map bvar (range binder-count)))
  pos =
  (match val
    ((uno) 0)
    ((bit (b-0)) 1)
    ((bit (b-1)) 2)
    ((bvar idx) (+ 3 idx)))
  (list-ref targets (modulo (+ pos offset) (length targets))))
(define (value-toggle val binder-count offset)
  (match val
    ((lam _ _) val)
    ((pair l r) (pair r l))
    (_ (value-shift-by val binder-count offset))))
(define (term-toggle term binder-count offset)
  (match term
    ((value val) (value (value-toggle val binder-count offset)))
    ((pair-access idx pr) (pair-access pr idx))
    ((lam-apply proc arg) (lam-apply arg proc))
    (_ term)))
(def (navterm-toggle nav offset)
  binder-count = (navterm-binder-count nav)
  focus = (navigator-focus nav)
  toggle = (if (term? focus) term-toggle value-toggle)
  (toggle focus binder-count offset))

(module+ test
  (check-equal?
    (term-toggle t-0 4 11)
    (value (bvar 2)))
  (check-equal?
    (navterm-toggle test-navterm-in 4)
    (value (bvar 0)))
  (check-equal?
    (navterm-toggle test-navterm-in 1)
    t-uno)
  )

(define (value-wrap-pair lhs) (pair lhs v-uno))
(define (wrap-pair lhs)
  (if (term-value? lhs) (value-wrap-pair lhs)
    (match lhs
      ((value lhs) (value (value-wrap-pair lhs)))
      (_ lhs))))
(define (wrap-pair-access idx)
  (match idx
    ((value idx) (pair-access idx v-uno-pair))
    (_ idx)))
(define (wrap-apply proc (arg t-uno))
  (if (term-value? proc) proc (lam-apply proc arg)))
(define (lift-by lift term) (subst (substitution '() lift) term))
(define ((wrap-lam-trans trans) tv)
  (define (wrap term) (lam lattr-void (trans term)))
  (if (term-value? tv) (wrap (value tv)) (value (wrap tv))))
(define wrap-lam (wrap-lam-trans (lambda (tm) (lift-by 1 tm))))

(module+ test
  (check-equal?
    (wrap-pair v-1)
    (pair v-1 v-uno))
  (check-equal?
    (wrap-pair t-1)
    (value (pair v-1 v-uno)))
  (check-equal?
    (wrap-pair (lam-apply t-uno t-uno))
    (lam-apply t-uno t-uno))
  (check-equal?
    (wrap-pair-access (lam-apply t-uno t-uno))
    (lam-apply t-uno t-uno))
  (check-equal?
    (wrap-pair-access v-uno)
    v-uno)
  (check-equal?
    (wrap-pair-access t-1)
    (pair-access v-1 v-uno-pair))
  (check-equal?
    (wrap-apply v-0)
    v-0)
  (check-equal?
    (wrap-apply t-0)
    (lam-apply t-0 t-uno))
  (check-equal?
    (wrap-lam t-1)
    (value (lam lattr-void (lift-by 1 t-1))))
  (check-equal?
    (wrap-lam v-1)
    (lam lattr-void (lift-by 1 t-1)))
  )

(define (trim-tv tv)
  (match tv
    ((value (lam attr body)) (substitute-lam-apply attr body v-uno))
    ((lam attr (value body))
     (substitute-value (substitution-lam-applied attr v-uno) body))
    ((pair l _) l)
    ((value v) (value (trim-tv v)))
    ((pair-access idx _) (value idx))
    ((lam-apply proc _) proc)
    (_ tv)))
(define (navterm-trim nav count)
  (last (iterate trim-tv (navigator-focus nav) count)))

(module+ test
  (check-equal?
    (trim-tv (pair v-1 v-0))
    v-1)
  (check-equal?
    (trim-tv (value (pair v-1 v-0)))
    t-1)
  (check-equal?
    (trim-tv (pair-access v-1 v-0))
    t-1)
  (check-equal?
    (trim-tv (lam-apply v-1 v-0))
    v-1)
  (check-equal?
    (trim-tv v-0)
    v-0)
  )

; extract/copy, paste/replace, rename
; jump to binder

(record interaction syntax history nav)
(records interaction-syntax
  (isyntax-raw)
  (isyntax-pretty))
(define (interaction-new term)
  (interaction (isyntax-pretty) '() (navterm-new term)))
(define interaction-empty (interaction-new t-uno))
(records interaction-instruction
  (ici-traverse-down count)
  (ici-traverse-left count)
  (ici-traverse-right count)
  (ici-traverse-up count)
  (ici-traverse-binder)
  (ici-rename-binder name)
  (ici-substitute-complete)
  (ici-step count)
  (ici-step-complete)
  (ici-edit method)
  (ici-toggle-syntax)
  (ici-undo count))
(records interaction-edit-method
  (ici-edit-replace term)
  (ici-edit-close)
  (ici-edit-delete)
  (ici-edit-toggle count)
  (ici-edit-trim count)
  (ici-edit-wrap wrap-type count))
(records interaction-wrap-type
  (ici-wrap-apply)
  (ici-wrap-lam)
  (ici-wrap-pair)
  (ici-wrap-pair-access))

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
      ((ici-traverse-binder)
       (trans-nav history navterm-traverse-binder iaction 1))
      ((ici-rename-binder name)
       (trans-nav history (trans-focus (rename-binder name)) iaction 1))
      ((ici-substitute-complete)
       (trans-nav history (trans-focus (compose1 right substitute-full))
                  iaction 1))
      ((ici-step count)
       (trans-nav history (trans-focus step-safe) iaction count))
      ((ici-step-complete)
       (trans-nav history (trans-focus step-complete-safe) iaction 1))
      ((ici-edit method)
       (lets
         focus =
         (match method
           ((ici-edit-replace term) (navterm-replace nav term))
           ((ici-edit-close) (navterm-close-over-free nav))
           ((ici-edit-delete) (navterm-delete nav))
           ((ici-edit-trim count) (navterm-trim nav count))
           ((ici-edit-toggle count) (navterm-toggle nav count))
           ((ici-edit-wrap type count)
            (lets wrap = (match type
                           ((ici-wrap-lam) wrap-lam)
                           ((ici-wrap-apply) wrap-apply)
                           ((ici-wrap-pair) wrap-pair)
                           ((ici-wrap-pair-access) wrap-pair-access))
                  (last (iterate wrap (navigator-focus nav) count)))))
         (list "" (:=* iaction (navigator-focus-set nav focus) 'nav))))
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
    )
  (void (forl
    wt <- (list (ici-wrap-pair) (ici-wrap-pair-access) (ici-wrap-apply))
    ia = (list-ref test-iactions 0)
    count = 4
    wcmd = (ici-edit (ici-edit-wrap wt count))
    tcmd = (ici-edit (ici-edit-trim count))
    ifocus = (fn (iaction) (navigator-focus (:.* iaction 'nav)))
    wrapped = (second (interaction-update wcmd ia))
    result = (second (interaction-update tcmd wrapped))
    (check-equal?
      (ifocus result)
      (ifocus ia))))
  )
