#lang racket
;(provide
  ;database-root
  ;db-url-add
  ;db-url-get
  ;db-url-remove
  ;db-url-trans
  ;db-interaction-add
  ;db-interaction-get
  ;db-revision-add
  ;db-revision-get
  ;revision-root

  ;url-attribute
  ;url-revision
  ;url-interaction

  ;;interaction
  ;;interaction-db-empty
  ;;present-workspace
  ;;tab
  ;;workspace
  ;;workspace-empty
  ;;workspace-view
  ;;workspace-view-empty
  ;)

(require
  ;"syntax-0-unparsing.rkt"
  ;"syntax-abstract.rkt"
  ;"util.rkt"
  gregr-misc/cursor
  gregr-misc/dict
  gregr-misc/either
  ;gregr-misc/list
  ;gregr-misc/match
  gregr-misc/record
  gregr-misc/sugar
  racket/control
  )

(module+ test
  (require rackunit))

(record url-tree data children)
(define (url-tree-new) url-tree-empty)
(define url-tree-empty
  (url-tree (void) (default-hash (lambda (_) (url-tree-new)) (hash))))
(define (url-path->cursor-path path)
  (append* (map (lambda (segment) (list 'children segment)) path)))
(define (url-tree-get tree path) (:. tree (url-path->cursor-path path)))
(def (url-tree-trans tree path trans)
  path = (url-path->cursor-path path)
  path = (append path '(data))
  (:~ tree trans path))
(def (url-tree-add tree path data)
  (url-tree-trans tree path (const data)))
(def (url-tree-remove tree path)
  path = (url-path->cursor-path path)
  (:= tree url-tree-empty path))

(module+ test
  (lets
    tree = url-tree-empty
    tree = (url-tree-add tree '(one two three) 3)
    tree = (url-tree-add tree '(one two) 2)
    _ = (check-equal?
          (:.* (url-tree-get tree '(one two)) 'data)
          2)
    _ = (check-equal?
          (:.* (url-tree-get tree '(one two three)) 'data)
          3)
    _ = (check-equal?
          (:.* (url-tree-get tree '(one)) 'data)
          (void))
    tree = (url-tree-remove tree '(one two))
    _ = (check-equal?
          (:.* (url-tree-get tree '(one two three)) 'data)
          (void))
    (void)))

;; metadata: misc junk, may include [tag], even under another tag
;; term: ...
;; tag (not the git tag kind): (uref metadata)
;; rev: root-rev | (uid parent-rev action (what was done to parent to produce this term) term)
;; rev-name (like a git tag): (uref rev metadata)
;; rev-branch (think of a better name): (uref rev metadata)
;; interaction: (uid nav-key rev forward-revs maybe(branch) metadata)
;; interaction-name (like git tag, except interactions themselves are mutable): (uref interaction metadata)
;; workspace: ([active] (ordered by viewport arrangement?) [stashed] (ordered by recency of stashing? stack?) interaction reflog)
;; git analogy
  ;;all revs have common root
  ;;branches and tags w/ preferred view info?
  ;;synch-able clones
  ;;shared editing w/ read/write capabilities
  ;;views are a separate concept
    ;;large granularity git commit analogy would be more like version control on views
    ;;projects/repos are indirect, only existing within hierarchical branch names for terms
      ;;ie. project-name/feature-branch-name/term-name
      ;;viewing a project is the same as filtering terms by project-name/

(record revision uid parent action content)
(record interaction uid nav-key rev forward-revs)

(record url-entry target metadata)
(define (url-entry-new target) (url-entry target (hash)))
(define (url-attribute uref) (list* "attribute" uref))
(define (url-revision uref) (list* "revision" uref))
(define (url-interaction uref) (list* "interaction" uref))

(record database url-tree uid-next revisions interactions)
(define dhash-empty (default-hash (const (void)) hash-empty))
(define database-empty (database url-tree-empty 0 dhash-empty dhash-empty))

(define (db-url-get-base db url)
  (:.* (url-tree-get (:.* db 'url-tree) url) 'data))
(def (db-url-get db url)
  data = (db-url-get-base db url)
  (if (void? data) (left "does not exist") (right data)))
(define (db-url-trans-base db url trans)
  (:~* db (lambda (tree) (url-tree-trans tree url trans)) 'url-tree))
(define (db-url-trans db url trans)
  (define tag (make-continuation-prompt-tag))
  (define (inner-trans entry)
    (if (void? entry)
      (shift-at tag _ (left "does not exist"))
      (trans entry)))
  (reset-at tag (right (db-url-trans-base db url inner-trans))))
(define (db-url-add db url target)
  (define tag (make-continuation-prompt-tag))
  (define (trans entry)
    (if (void? entry)
      (url-entry-new target)
      (shift-at tag _ (left "already exists"))))
  (reset-at tag (right (db-url-trans-base db url trans))))
(define (db-url-remove db url)
  (if (void? (db-url-get-base db url))
    (left "does not exist")
    (right (:~* db (lambda (tree) (url-tree-remove tree url)) 'url-tree))))

(def (db-get db type uid)
  result = (:.* db type uid)
  (if (void? result) (left "does not exist") (right result)))
(define (db-revision-get db uid) (db-get db 'revisions uid))
(define (db-interaction-get db uid) (db-get db 'interactions uid))
(define (db-add type new db . args)
  (lets
    uid = (:.* db 'uid-next)
    db = (:~* db (curry + 1) 'uid-next)
    resource = (apply new uid args)
    db = (:=* db resource type uid)
    (list db resource)))
(define db-revision-add (curry db-add 'revisions revision))
(define db-interaction-add (curry db-add 'interactions interaction))

(match-define (list database-root revision-root)
  (db-revision-add database-empty (void) (void) (void)))

(module+ test
  (lets
    url = (url-revision (list "one" "two"))
    db = database-root
    _ = (check-equal? (db-revision-get db 0) (right revision-root))
    _ = (check-equal? (db-revision-get db 1) (left "does not exist"))
    _ = (check-equal?
          (db-url-get db url)
          (left "does not exist"))
    _ = (check-equal?
          (db-url-remove db url)
          (left "does not exist"))
    db = (right-x (db-url-add db url 2))
    _ = (check-equal?
          (db-url-get db url)
          (right (url-entry 2 (hash))))
    _ = (check-equal?
          (db-url-add db url 2)
          (left "already exists"))
    _ = (check-equal? (right? (db-url-remove db url)) #t)
    (void)))

;; step 1: DB and revision manipulation (mini diffs/replacements, other primitive ops)
;; step 2: simple ui term display/navigation (or, should this be step 1 for faster feedback?)
;; step 3: ui interactions using step 1

;; merge revision
  ;; fast-forward
  ;; rebase
  ;; out-of-order integration
  ;; conflict resolution?

;; extract current contextually-closed-subterm as a new revision
;; replace current subterm with revision, but context must be matchable

;; mini diffs
  ;; complex operations as a sequence of replacements (as a single revision action?)
  ;; replacement: (nav-key, (term | (rev, nav-key)))
  ;; sequence of positioned term replacements, with replacement-sources relative to original term (not to intermediate terms)
    ;; example: wrapping a subterm
      ;; replace subterm with wrapping shell
        ;; doesn't matter what destination position in shell contains; will be replaced by thing-to-be-wrapped
      ;; replace destination position with original subterm (original subterm position given is relative to original term)
        ;; appropriate naked lift substs need to be formed around the destination, if the wrapping shell includes binders
  ;; all below wrapping/embedding introductions work this way?

;;erase,drop,prune,trim?

;;unit (maybe root revision should include this?)
  ;;introduce: (completely erase an existing term)

;;bit
  ;;introduce: 0 or 1
  ;;erase to unit
  ;;toggle

;;bvar
  ;;introduce: select from available names
  ;;erase to unit
  ;;toggle/cycle

;;pair
  ;;introduce: embed subvalue left/right (other side gets unit)
  ;;erase left/right
  ;;swap
  ;;rotate left/right
    ;;tuple rotates are compound operations involving normal rotates and swaps

;;lam
  ;;introduce: wrap subterm after wrapping with naked lift-subst (should it be completed immediately?)
  ;;float/raise: cross a lambda? leave behind name-swap-subst; otherwise leave behind lifts as appropriate
    ;;cannot be floated when embedded in pairs, or through a subst (push the subst down through the lambda instead)
  ;;factor-out (aka lambda lifting):
    ;;requires creating a contextually-closed term, moving it upward into internal global context
    ;;(introduce new lambda with globalified arg, lift-substs, replacing original uses with bvar)
  ;;apply to an immediate value (value | (nav-key, rev))
    ;;cannot be applied when embedded in pairs
  ;;specify arg name, comments
  ;;[un]thunkify
  ;;[un]contextify/globalify
    ;;actually, is this necessary? only for extending the set of "globals", not minor context
      ;;global args directly refer to a rev?
    ;;context can be determined implicitly during replacement (unselected lams)
    ;;selected lams will be given fresh arg tags when copying/replacing

;;subst
  ;;finish minor (naked lifts, renamings, ...)
  ;;finish completely

;;produce
  ;;introduce
  ;;erase to subterm

;;pair-access
  ;;introduce: embed subvalue left/right
    ;;pair-left, pair-right, if-0/1?
  ;;erase left/right
  ;;swap

;;lam-apply
  ;;introduce: embed subterm left/right
    ;;let? seq? abstract/generalize as in learnable-prog?
  ;;erase left/right
  ;;swap
  ;;rotate left/right
    ;;flat-arg rotates are compound operations involving normal rotates and swaps

;;global args
  ;;global arg: (tag/uid, revision uref)
    ;;can themselves be named by uref
  ;;globalified lams name the revision (preferrably a rebindable module-like uref) they desire to be applied to
    ;;sort of like an import statement
    ;;a convenience, not an enforced constraint (would be unsafe when dealing with foreign programs)
    ;;similarly-pointed args may have distinct arg tags (in the future, they may point in different directions)
    ;;similarly-tagged args may have distinct names across different revisions
  ;;interaction
    ;;add/remove/re-introduce global args from shared pool
      ;;re-introduce is like adding, but without a fresh tag (refer to a currently-unshared global used by a revision)
    ;;add/remove global shared pool args within an interaction
    ;;run/debug/optimize/etc. a program with global args
      ;;resolve revision dependencies
      ;;construct complete program term
      ;;manipulate in the usual way

;;subterm traversal
  ;;descend, ascend, left, right
  ;;refocus/jump to [upward/downward] continuation from subterm
  ;;assign and jump to nav-key bookmarks

;;other subterm manipulation
  ;;replace
  ;;small step
  ;;big step
  ;;finish substitutions (minor, all non-closure, all)

;(records workspace-event
  ;(event-ws-close )
  ;;(event-ws-new ) ; new interaction starting at a given revision (root by default)
  ;;(event-ws-watch ) ; track an existing interaction
  ;;introduce new + replace
  ;;(event-ws-introduce )
  ;;(event-ws-copy )
  ;(event-ws-shift count)  ; left/right?
  ;(event-ws-rotate count)
  ;(event-ws-swap)
  ;; help, command/search
  ;)

;(records term-interactor-event
  ;;replace (basic, existing, name/branch)
  ;;(basic, existing (such as from another tab; not a copy), name/branch  )
  ;;(event-ti-replace )
  ;;(event-ti-name-revision )
  ;;(event-ti-name-interaction )
  ;(event-ti-descend count)
  ;(event-ti-ascend count)
  ;(event-ti-shift count)
  ;(event-ti-shift count)
  ;;(event-ti-step-small count)
  ;;(event-ti-step-big)
  ;; help, command/search
  ;)

;(record workspace motd-doc interactors)
;(record term-interactor nav interaction)

;(define workspace-keymap
  ;(hash
    ;#\q         (fn (_)     (list (event-terminate (void))))
    ;#\page      (fn (count) (list (event-ws-shift count)))
    ;#\backspace (fn (count) (list (event-ws-shift (- count))))
    ;#\L         (fn (count) (list (event-ws-rotate count)))
    ;#\H         (fn (count) (list (event-ws-rotate (- count))))
    ;#\W         (fn (_)     (list (event-ws-swap)))
    ;))

;(define term-interactor-keymap
  ;(hash
    ;#\j (fn (count) (list (event-ti-descend count)))
    ;#\k (fn (count) (list (event-ti-ascend count)))
    ;#\l (fn (count) (list (event-ti-shift count)))
    ;#\h (fn (count) (list (event-ti-shift (- count))))
    ;))

;(define (nterm->doc nterm)
  ;)

;(define (idocs->doc motd-doc idocs)
  ;(define table-style
    ;(apply table-style-basic-bordered
           ;1 1
           ;(append (list #\= #\= #\-
                         ;#\# #\# #\|
                         ;#\^ #\> #\< #\v
                         ;#\+ #\+ #\+ #\+ #\+)
                   ;(make-list 15 (style 'default 'default #f #f #f #t)))))
  ;(define itable (doc-table style-empty table-style (list idocs)))
  ;(if (empty? idocs) motd-doc itable))


;(define (workspace-ctrl ws)  ; TODO: this is only the initial ctrler; define a new ctrl transition
  ;(match-define (workspace motd-doc interactors) ws)
  ;(match-define ictrls (map term-interactor-ctrl interactors))
  ;; TODO: actual interactor docs
  ;(define nterms (map ::0 interactors))  ; TODO: actual interactors
  ;(define idocs (map nterm->doc nterms))
  ;(define initial-doc (idocs->doc motd-doc idocs))
  ;(def (handle-note note)
    ;(match note
      ;((note-terminated result) (left result))
      ;((note-view next-doc) (right next-doc))
      ;(_ (right doc))))
  ;(def (handle-notes notes)
    ;(begin/with-monad either-monad
      ;next-doc <- (monad-foldl either-monad handle-note doc notes)
      ;_ = (unless (eq? doc next-doc) (display-doc next-doc))
      ;(pure (markout-reactor next-doc)))

  ;(def (self event)
    ;(match event
      ;;((event-ws-shift count) )
      ;;(event-ws-rotate count)
      ;;(event-ws-swap)
      ;(_
        ;(match ictrls
          ;('()
           ;(match event
             ;((event-terminate _) (list self (list (note-terminated (void)))))
             ;(_ (list self '()))))
          ;((cons focus unfocused)
           ;(lets
             ;(list focus notes) = (focus event)
             ;ictrls = (cons focus unfocused)
             ;(:=* ws ictrls 'interactors)))))))
  ;self)

;(define (term-interactor-ctrl interactor)
  ;(define (note-nav-view nav)
    ;(list self (list (note-view ))))
  ;(def (self event)
    ;(term-interactor nav _) = interactor
    ;(match event
      ;((event-terminate _) (list self (list (note-terminated (void)))))
      ;((event-ti-descend count) (note-nav-view ))
      ;((event-ti-ascend count)
       ;)
      ;((event-ti-shift count)
       ;)
      ;((event-ti-shift count)
       ;)

      ;(_ (list self '()))
      ;))
  ;self)

;;(module+ main
  ;;(define (doc-str str) (doc-atom style-empty str))
  ;;(define (doc-append . docs) (vertical-list style-empty docs))
  ;;(define ((ctrl doc) event)
    ;;(def (note-doc-append doc-tail)
      ;;new-doc = (doc-append doc doc-tail)
      ;;(list (ctrl new-doc) (list (note-view new-doc))))
    ;;(match event
      ;;((event-terminate _) (list (ctrl doc) (list (note-terminated (void)))))
      ;;((event-tick dt)
       ;;(note-doc-append (doc-str (format "time-delta: ~ams" dt))))
      ;;((event-keycount char count)
       ;;(note-doc-append (doc-str (format "keycount: ~v,~a" char count))))
      ;;(_ (list (ctrl doc) '()))))

  ;;(lets
    ;;sty = (style 'yellow 'blue #f #f #f #f)
    ;;doc = (doc-preformatted (styled-block-fill sty #\x (size 10 20)))
    ;;doc = (doc-append doc (doc-str "Press 'q' to quit this test."))
    ;;ctrl = (keymap-controller keymap (ctrl doc))
    ;;(markout-dispatch-react-loop doc (keycount-controller ctrl))
    ;;))


;;; old

;;(record workspace-view tab-index ic-indices)
;;(record workspace tabs interaction-db)
;;(record tab layout interaction-uids)

;;(record interaction pretty current history)
;;(record interaction-db uid->interaction name<->uid active-uids discarded-uids preserved)

;;(define (interaction-new term)
  ;;(interaction (curry unparse upenv-empty) (::0 term) '()))

;;(define interaction-db-empty (interaction-db (hash) (hash) (set) (set) (hash)))
;;(define/destruct (interaction-db-add
                   ;;(interaction-db uid->i name<->uid active discarded preserved)
                   ;;interaction)
  ;;(let* ((uid (+ 1 (apply max (cons -1 (hash-keys uid->i)))))
         ;;(uid->i (hash-set uid->i uid interaction))
         ;;(name<->uid (hash-set* name<->uid uid (set) (~a uid) uid))
         ;;(active (set-add active uid)))
    ;;(list (interaction-db uid->i name<->uid active discarded preserved)
          ;;uid)))

;;(define tab-empty (tab (void) '()))
;;(define/destruct (tab-add (tab layout iuids) ic-index iuid)
  ;;(tab layout (:~ iuids (curry cons iuid) (list-path ic-index))))
;;(define workspace-empty (workspace (list tab-empty) interaction-db-empty))
;;(define workspace-view-empty (workspace-view -1 '()))

;;;; presentation
;;(define major-divider "================================\n")
;;(define minor-divider "----------------\n")
;;(define select-divider "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n")

;;(define ((bracket left right) str) (string-append left str right))
;;(define bracket-ic-view (bracket select-divider select-divider))
;;(define bracket-tab (bracket "[" "]"))

;;(define (present-cterm pretty cterm)
  ;;(string-join (list
                  ;;(pretty-string (cursor-trail cterm))
                  ;;(pretty-string (pretty (::.* cterm))))
                ;;minor-divider))

;;(define/destruct (present-interaction (interaction pretty current history))
  ;;(string-join (list
                 ;;(present-cterm pretty current)
                 ;;(format "history length: ~a\n" (length history)))
               ;;minor-divider))

;;(define/destruct (present-tab uid->interaction ic-index (tab layout iuids))
  ;;(let* ((interactions (map (curry dict-ref uid->interaction) iuids))
         ;;(presented-interactions (map present-interaction interactions))
         ;;(ic-views (map string-append
                     ;;(map (curry format (string-append "< ~a >\n" minor-divider))
                          ;;(range (length interactions)))
                     ;;presented-interactions))
         ;;(ic-views (:~ ic-views bracket-ic-view (list-path ic-index 'first))))
    ;;(string-join ic-views major-divider)))

;;(define/destruct (present-workspace (workspace-view tab-index ic-indices)
                                    ;;(workspace tabs idb))
  ;;(if (< tab-index 0) "tabs: ..."
    ;;(let* ((uid->interaction (interaction-db-uid->interaction idb))
          ;;(ic-index (list-ref ic-indices tab-index))
          ;;(tab (list-ref tabs tab-index))
          ;;(tab-names (map ~a (range (length tabs))))
          ;;(tab-names (:~ tab-names bracket-tab (list-path tab-index 'first)))
          ;;(tab-view (present-tab uid->interaction ic-index tab)))
      ;;(string-join
        ;;(list tab-view
              ;;(string-append "tabs: " (string-join tab-names " ") "\n"))
        ;;major-divider))))
