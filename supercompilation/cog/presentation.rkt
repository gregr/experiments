#lang racket
(provide
  nav-term-flat->doc
  nav-term-lifted->doc
  string->doc
  tabular-view
  view->string
  )

(require
  "syntax.rkt"
  "syntax-abstract.rkt"
  gregr-misc/cursor
  gregr-misc/list
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
  lam-arg-prefix = (doc-atom style-lam "(")
  lam-arg-suffix = (doc-atom style-lam ")")
  lam-doc = (doc-atom style-lam "λ")
  unit-doc = (doc-atom style-unit "{}")
  b-0-doc = (doc-atom style-bit "0")
  b-1-doc = (doc-atom style-bit "1")
  subst-prefix = (doc-atom style-subst-bracket "[")
  subst-suffix = (doc-atom style-subst-bracket "]")
  subst-assignment = (doc-atom style-subst-use "=")
  subst-separator = (doc-atom style-subst-separator ";")
  produce-prefix = (doc-atom style-produce-bracket "(")
  produce-suffix = (doc-atom style-produce-bracket ")")
  produce-doc = (doc-atom style-produce "produce")
  access-prefix = (doc-atom style-access-bracket "[")
  access-suffix = (doc-atom style-access-bracket "]")
  apply-prefix = (doc-atom style-apply-bracket "(")
  apply-suffix = (doc-atom style-apply-bracket ")")

  (fnr (render env t/v)
    (match t/v
      ((uno)       unit-doc)
      ((bit (b-0)) b-0-doc)
      ((bit (b-1)) b-1-doc)
      ((pair l r)
       (lets
         items = (map (curry render env) (list l r))
         items = (separated pair-separator style-default items)
         (bracketed-chain pair-prefix pair-suffix attr-loose-aligned
                          style-default style-default items)))
      ((bvar idx)
       (lets
         name = (symbol->string (binders-get env idx))
         (doc-atom style-bvar name)))
      ((lam attr body)
       (lets
         (list env names body) = (gather-lams env (value t/v))
         names = (forl
                   name <- (map symbol->string names)
                   (doc-atom style-bvar name))
         names = (bracketed-chain
                   lam-arg-prefix lam-arg-suffix attr-loose-aligned
                   style-default style-default names)
         (bracketed-chain
           lam-prefix lam-suffix attr-loose-aligned style-default
           style-default (list (tight-pair style-lam lam-doc names)
                               (render env body)))))
      ((subst (substitution uses lift) t)
       (lets
         vals = (map (curry render env) (map substitution-use-v uses))
         (list names env) = (subst-binders env uses lift)
         names = (map symbol->string names)
         names = (map (curry doc-atom style-bvar) names)
         assignments =
         (forl
           name <- names
           val <- vals
           (doc-chain style-subst-use attr-loose-aligned
                      (list name subst-assignment val)))
         lift = (doc-atom style-subst-lift (format "^~a" lift))
         sub-inner = (separated subst-separator style-default
                                (list* lift assignments))
         sub = (bracketed-chain subst-prefix subst-suffix attr-loose-aligned
                                style-default style-default sub-inner)
         body = (render env t)
         (tight-pair style-default sub body)))
      ((value v) (render env v))
      ((produce t)
       (bracketed-chain produce-prefix produce-suffix attr-loose-aligned
                        style-default style-default
                        (list produce-doc (render env t))))
      ((pair-access index pair)
       (lets
         index =
         (bracketed-chain access-prefix access-suffix attr-loose-aligned
                          style-default style-default
                          (list (render env index)))
         (tight-pair style-default (render env pair) index)))
      ((lam-apply proc arg)
       (bracketed-chain apply-prefix apply-suffix attr-loose-aligned
                        style-default style-default
                        (map (curry render env) (gather-applications t/v))))
      (x (render-other env x)))))
(define doc-render-empty
  (style-palette->doc-renderer (void) palette-empty))
(define doc-render-selected-default
  (style-palette->doc-renderer (void) palette-selected-default))
(record selected x)
(record hole)
(define doc-render-default
  ((thunk
     (define hole-doc (doc-atom (:=* style-empty #t 'invert?) "[]"))
     (style-palette->doc-renderer
       (lambda (env x)
         (match x
           ((selected x) (doc-render-selected-default env x))
           ((hole) hole-doc)))
       palette-default))))

(define visible-context-levels-default 7)

(def (nav-term-flat->doc nav)
  doc-empty = (doc-atom style-empty "")
  (list foci paths) =
  (zip (forl
         (list focus hole-pos) <- (navigator-path nav)
         path = (match hole-pos
                  ((nothing) '())
                  ((just (list _ path)) path))
         (list focus path)))
  hidden-count = (max 0 (- (length foci) visible-context-levels-default))
  term = (first foci)
  focus = (first (drop foci hidden-count))
  selected-path = (append* (drop paths hidden-count))
  context-path = (append* (take paths hidden-count))
  env = (last (nav-paths->binders binders-empty term (list context-path)))
  focus = (if (empty? selected-path) focus (:~ focus selected selected-path))
  hidden = (doc-atom style-empty (format "~a levels hidden ..." hidden-count))
  focus-doc = (doc-render-default env focus)
  (vertical-list style-empty (list hidden doc-empty focus-doc)))

(def (nav-term-lifted->doc nav)
  hole-term = (hole)
  (list foci paths holed-foci) =
  (zip (forl
         (list focus hole-pos) <- (navigator-path nav)
         (list path holed-focus) =
         (match hole-pos
           ((nothing) (list '() focus))
           ((just (list _ path)) (list path (:= focus hole-term path))))
         (list focus path holed-focus)))
  envs = (nav-paths->binders binders-empty (first foci) paths)
  focus-docs =
  (forl
    focus <- holed-foci
    env <- envs
    (doc-render-default env focus))
  divider-doc = (doc-atom style-empty "----")
  docs = (add-between (reverse focus-docs) divider-doc)
  (vertical-list style-empty docs))

(define string->doc
  (compose1 doc-preformatted (curry string->styled-block style-empty #\space)))

(def (view->string view)
  sz = (screen-size)
  ctx = (sizing-context-new-default)
  block = (doc->styled-block ctx style-empty sz (view sz))
  (styled-block->string block))

(define ((tabular-view message commands focus-index d-inner-docs) sz)
  (define-values (inner-doc-list cpu-time real-time gc-time)
    (time-apply (thunk (map force d-inner-docs)) '()))
  (define border-style (:=* style-empty #t 'invert?))
  (define active-border-style (:=* border-style 'red 'color-fg))
  (define normal-footer (filler-h border-style #\space 1))
  (define active-footer (filler-h active-border-style #\space 1))
  (define inner-docs (first inner-doc-list))
  (define time-str (format "cpu time: ~a real time: ~a gc time: ~a"
                           cpu-time real-time gc-time))
  (define (doc-str str) (doc-atom style-empty str))
  (define time-doc (doc-str time-str))
  (define msg-doc (doc-str message))
  (define notification-doc
    (vertical-list style-empty (list msg-doc time-doc)))
  (lets
    (size full-width full-height) = sz
    div-w = 1
    div-h = 1
    div-size = (size div-w div-h)
    notification-height = 2
    content-height = (- full-height (+ notification-height div-h))
    fixed-size =
    (lambda (doc sz)
      (doc-frame style-empty (frame-fixed (rect (coord 0 0) sz)) doc))
    fixed-content =
    (lambda (doc)
      (doc-frame
        style-empty
        (frame-fixed-height (coord 0 0) content-height) doc))
    command-doc =
    (vertical-list style-empty
      (list
        (fixed-content
          (doc-expander expander-height
            (vertical-list style-empty
              (forl
                (list cmd desc) <- commands
                (doc-chain style-empty attr-loose-aligned
                           (list (doc-str cmd) (doc-str desc)))))))
        (filler-h border-style #\space div-h)))
    inner-docs =
    (forl
      idx <- (range (length inner-docs))
      doc <- inner-docs
      footer = (if (= idx focus-index) active-footer normal-footer)
      (vertical-list style-empty
        (list (fixed-content (doc-expander expander-both doc)) footer)))
    notification-doc = (fixed-size notification-doc
                                   (size full-width notification-height))
    div-table =
    (lambda (rows)
      (bordered-table
        style-empty border-style (size 0 0) div-size (make-list 15 #\space)
        rows))
    content-table = (div-table (list (list* command-doc inner-docs)))
    (vertical-list style-empty (list content-table notification-doc))))
