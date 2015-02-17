#lang racket
(provide
  interact-with
  )

(require
  "semantics-operational.rkt"
  "syntax-0-unparsing.rkt"
  "syntax-abstract.rkt"
  "util.rkt"
  gregr-misc/cursor
  gregr-misc/either
  gregr-misc/list
  gregr-misc/markout
  gregr-misc/match
  gregr-misc/maybe
  gregr-misc/monad
  gregr-misc/navigator
  gregr-misc/record
  gregr-misc/sugar
  gregr-misc/terminal
  )

(def (subst-keys (substitution uses _))
  use-keys =
  (forl
    use-list-key <- (list-navigator-keys uses)
    (append '(s uses) use-list-key '(v)))
  (list* '(t) use-keys))
(define (hole-keys focus)
  (match focus
    ((subst sub _) (subst-keys sub))
    ((lam _ _)     '((body)))
    (_ (if (or (term? focus) (pair? focus))
         (map list (dict-keys focus)) '()))))

(define (interact-ascend nav)
  (maybe->either "no hole to fill" (navigator-ascend nav)))
(define (interact-descend nav (idx 0))
  (maybe->either
    (format "cannot select subterm ~a of: ~v" idx (navigator-focus nav))
    (navigator-descend nav idx)))
(define ((interact-shift offset) nav)
  (maybe->either (format "cannot shift by ~a" offset)
                 (navigator-shift nav offset)))
(define interact-shift-left (interact-shift -1))
(define interact-shift-right (interact-shift 1))
(define (interact-with-focus f nav)
  (begin/with-monad either-monad
    new-focus <- (f (navigator-focus nav))
    (pure (navigator-focus-set nav new-focus))))
(define interact-step (curry interact-with-focus step-safe))
(define interact-complete (curry interact-with-focus step-complete-safe))
(define (interact-context-present nav)
  (forl
    (list focus hole-pos) <- (navigator-path nav)
    (match hole-pos
      ((nothing) focus)
      ((just (list _ key)) (:= focus (void) key)))))

(record void-closure is-value upenv)
(define (unparse-void-closure upe term)
  (if (void? term) (void-closure #f upe)
    (unparse-orec unparse-void-closure unparse-value-void-closure upe term)))
(define (unparse-value-void-closure upe val)
  (if (void? val) (void-closure #t upe)
    (unparse-value-orec unparse-void-closure unparse-value-void-closure
                        upe val)))
(define (void-closures term)
  (match term
    ((? void-closure?) (list term))
    ((? list?)         (foldr append '() (map void-closures term)))
    (_                 '())))
(define (chain-unparse chain)
  (define (unparse-vc-chain term-or-value parents)
    (match (car (void-closures (car parents)))
      ((void-closure is-value upe)
       (cons (if is-value
               (unparse-value-void-closure upe term-or-value)
               (unparse-void-closure upe term-or-value))
             parents))))
  (cdr (reverse
         (foldl unparse-vc-chain (list (void-closure #f upenv-empty)) chain))))
(define (holed-substitute hole? replacement term)
  (match term
    ((? hole?) replacement)
    ((? list?) (map (curry holed-substitute hole? replacement) term))
    (_ term)))
(define (chain-unparse-void chain)
  (map (curry holed-substitute void-closure? (void)) (chain-unparse chain)))

(define (chain-show chain)
  (string-join
    (list ""
          (string-join (map pretty-string chain) "\n----------------\n\n")
          "")
    "\n================================\n\n"))

; TODO: style marks for position tracking
(record style-palette
  default
  unit
  bit
  bvar
  lam
  lam-bracket
  pair-bracket
  pair-separator
  subst-lift
  subst-use
  subst-separator
  subst-bracket
  produce
  produce-bracket
  access-bracket
  apply-bracket
  )
(define style-palette-empty
  (style-palette style-empty style-empty style-empty style-empty style-empty
    style-empty style-empty style-empty style-empty style-empty style-empty
    style-empty style-empty style-empty style-empty style-empty))

(def (style-palette->doc-renderer
       render-other
       (style-palette
         style-default
         style-unit
         style-bit
         style-bvar
         style-lam
         style-lam-bracket
         style-pair-bracket
         style-pair-separator
         style-subst-lift
         style-subst-use
         style-subst-separator
         style-subst-bracket
         style-produce
         style-produce-bracket
         style-access-bracket
         style-apply-bracket
         ))
  pair-prefix = (doc-atom style-pair-bracket "{")
  pair-suffix = (doc-atom style-pair-bracket "}")
  pair-separator = (doc-atom style-pair-separator ",")
  lam-prefix = (doc-atom style-lam-bracket "(")
  lam-suffix = (doc-atom style-lam-bracket ")")
  lam-doc = (doc-atom style-lam "λ")
  unit-doc = (doc-atom style-unit "{}")
  b-0-doc = (doc-atom style-bit "0")
  b-1-doc = (doc-atom style-bit "1")
  subst-prefix = (doc-atom style-subst-bracket "[")
  subst-suffix = (doc-atom style-subst-bracket "]")
  subst-separator = (doc-atom style-subst-separator ",")
  produce-prefix = (doc-atom style-produce-bracket "(")
  produce-suffix = (doc-atom style-produce-bracket ")")
  produce-doc = (doc-atom style-produce "produce")
  access-prefix = (doc-atom style-access-bracket "[")
  access-suffix = (doc-atom style-access-bracket "]")
  apply-prefix = (doc-atom style-apply-bracket "(")
  apply-suffix = (doc-atom style-apply-bracket ")")

  (letrec ((render
    (fn (t/v)
      (match t/v
        ((uno)       unit-doc)
        ((bit (b-0)) b-0-doc)
        ((bit (b-1)) b-1-doc)
        ((pair l r)
         (lets
           items = (map render (list l r))
           items = (separated pair-separator style-default items)
           (bracketed-chain pair-prefix pair-suffix attr-loose-aligned
                            style-default style-default items)))
        ((bvar idx) (doc-atom style-bvar (format "$~a" idx)))
        ((lam attr body)
         (bracketed-chain lam-prefix lam-suffix attr-loose-aligned
                          style-default style-default
                          (list lam-doc (render body))))
        ((subst (substitution uses lift) t)
         (lets
           bindings = (map render (map substitution-use-v uses))
           lift = (doc-atom style-subst-lift (format "^~a" lift))
           bindings = (separated subst-separator style-default
                                 (list* lift bindings))
           sub = (bracketed-chain subst-prefix subst-suffix attr-loose-aligned
                                  style-default style-default bindings)
           body = (render t)
           (tight-pair style-default sub body)))
        ((value v) (render v))
        ((produce t)
         (bracketed-chain produce-prefix produce-suffix attr-loose-aligned
                          style-default style-default
                          (list produce-doc (render t))))
        ((pair-access index pair)
         (lets
           index =
           (bracketed-chain access-prefix access-suffix attr-loose-aligned
                            style-default style-default
                            (list (render index)))
           (tight-pair style-default (render pair) index)))
        ((lam-apply proc arg)
         (bracketed-chain apply-prefix apply-suffix attr-loose-aligned
                          style-default style-default
                          (map render (list proc arg))))
        (x (render-other x))))))
    render))
(define doc-render-empty
  (style-palette->doc-renderer (void) style-palette-empty))

(def (interact-context->doc nav)
  doc-empty = (doc-atom style-empty "")
  parts =
  (forl
    (list focus hole-pos) <- (navigator-path nav)
    (doc-render-empty focus))
  parts = (add-between parts doc-empty)
  (vertical-list style-empty parts))

(def (doc-show doc)
  (size width height) = (screen-size)
  ctx = (sizing-context-new-default)
  block = (doc->styled-block ctx style-empty (min 80 width) doc)
  block-str = (styled-block->string block)
  (string-append block-str "\n"))

(define view-syntax-doc
  (compose1 doc-show interact-context->doc))
(define view-syntax-raw
  (compose1 chain-show interact-context-present))
(define view-syntax-0
  (compose1 chain-show chain-unparse-void interact-context-present))
(define (view-toggle current-view)
  (right (if (eq? view-syntax-doc current-view)
           view-syntax-0 view-syntax-doc)))

(record interact-state view context history)
(def (interact-state-viewcontext (interact-state view ctx _)) (view ctx))

(define ((left-display-default default) result)
  (either-fold (lambda (msg) (display msg) default) identity result))

(define ((interact-safe path) trans st)
  (right (:~ st (compose1 (left-display-default (:. st path)) trans) path)))
(define interact-safe-context (interact-safe '(context)))
(define interact-safe-view (interact-safe '(view)))

(define (interact-loop state)
  (let loop ((st state))
    (printf "~a" (interact-state-viewcontext st))
    (display "[hjkl](movement),[s]tep(count),[c]omplete,toggle-synta[x],[u]ndo,[q]uit> ")
    (begin/with-monad either-monad
      prev-st = st
      input = (read-line)
      st <- (match input
              ("h" (interact-safe-context interact-shift-left st))
              ("l" (interact-safe-context interact-shift-right st))
              ("j" (interact-safe-context interact-descend st))
              ("k" (interact-safe-context interact-ascend st))
              ((pregexp #px"^\\s*s\\s*(\\d+)?\\s*$" (list _ count))
               (let ((count (if count (string->number count) 1)))
                 (last (iterate
                         (compose1 (curry interact-safe-context interact-step)
                                   right-x)
                         (right st) count))))
              ("c" (interact-safe-context interact-complete st))
              ("x" (interact-safe-view view-toggle st))
              ("u" (match (:.* st 'history)
                     ('() (displayln "nothing to undo!") (right st))
                     ((cons prev-state hist) (right prev-state))))
              ("q" (left "quitting"))
              (_ (displayln "invalid choice") (right st)))
      st = (if (equal? input "u") st
             (:~* st (curry cons prev-st) 'history))
      (loop st))))

(define (interact-with term)
  (interact-loop
    (interact-state view-syntax-0 (navigator-new hole-keys term) '())))
