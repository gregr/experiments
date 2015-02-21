#lang racket
(provide
  doc-show
  nav-term->doc
  )

(require
  "syntax-abstract.rkt"
  gregr-misc/cursor
  gregr-misc/markout
  gregr-misc/maybe
  gregr-misc/navigator
  gregr-misc/record
  gregr-misc/sugar
  gregr-misc/terminal
  )

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
(define palette-empty
  (style-palette style-empty style-empty style-empty style-empty style-empty
    style-empty style-empty style-empty style-empty style-empty style-empty
    style-empty style-empty style-empty style-empty style-empty))
(define palette-default
  (forf
    palette = palette-empty
    (list field fgc) <- '((unit red)
                          (bit red)
                          (bvar magenta)
                          (pair-bracket yellow)
                          (pair-separator yellow)
                          (lam yellow)
                          (lam-bracket green)
                          (subst-lift cyan)
                          (subst-use cyan)
                          (subst-separator cyan)
                          (subst-bracket cyan)
                          (produce blue)
                          (produce-bracket blue)
                          (access-bracket blue)
                          (apply-bracket white))
    (:=* palette fgc field 'color-fg)))
(define palette-selected-default
  (forf
    palette = palette-default
    key <- (dict-keys palette-default)
    (:=* palette #t key 'invert?)))

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
  subst-separator = (doc-atom style-subst-separator ";")
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
  (style-palette->doc-renderer (void) palette-empty))
(define doc-render-selected-default
  (style-palette->doc-renderer (void) palette-selected-default))
(record selected x)
(define doc-render-default
  (style-palette->doc-renderer
    (fn ((selected x)) (doc-render-selected-default x))
    palette-default))

(def (nav-term->doc nav)
  doc-empty = (doc-atom style-empty "")
  parts =
  (forl
    (list focus hole-pos) <- (navigator-path nav)
    focus =
    (match hole-pos
      ((nothing) focus)
      ((just (list _ key)) (:~ focus selected key)))
    (doc-render-default focus))
  parts = (add-between parts doc-empty)
  (vertical-list style-empty parts))

(def (doc-show doc)
  (size width height) = (screen-size)
  ctx = (sizing-context-new-default)
  block = (doc->styled-block ctx style-empty (min 80 width) doc)
  block-str = (styled-block->string block)
  (string-append block-str "\n"))
