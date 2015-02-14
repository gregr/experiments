#lang racket
(provide
  database-root
  db-url-add
  db-url-get
  db-url-remove
  db-url-trans
  db-interaction-add
  db-interaction-get
  db-revision-add
  db-revision-get
  revision-root

  url-attribute
  url-branch
  url-revision
  url-interaction

  ;interaction
  ;interaction-db-empty
  ;present-workspace
  ;tab
  ;workspace
  ;workspace-empty
  ;workspace-view
  ;workspace-view-empty
  )

(require
  ;"syntax-0-unparsing.rkt"
  ;"syntax-abstract.rkt"
  ;"util.rkt"
  gregr-misc/cursor
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
(define url-tree-empty (url-tree (void) (hash)))
(define (new-url-tree . _) url-tree-empty)
(define (url-path->cursor-path path)
  (append* (map (lambda (segment) (list 'children segment)) path)))
(define (url-tree-get tree path)
  (:%. new-url-tree tree (url-path->cursor-path path)))
(def (url-tree-trans tree path trans)
  path = (url-path->cursor-path path)
  path = (append path '(data))
  (:%~ new-url-tree tree trans path))
(def (url-tree-add tree path data)
  (url-tree-trans tree path (const data)))
(def (url-tree-remove tree path)
  path = (url-path->cursor-path path)
  (:%= new-url-tree tree url-tree-empty path))

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

; metadata: misc junk, may include [tag], even under another tag
; term: ...
; tag (not the git tag kind): (uref metadata)
; rev: root-rev | (uid parent-rev action (what was done to parent to produce this term) term)
; rev-name (like a git tag): (uref rev metadata)
; rev-branch (think of a better name): (uref rev metadata)
; interaction: (uid nav-key rev forward-revs maybe(branch) metadata)
; interaction-name (like git tag, except interactions themselves are mutable): (uref interaction metadata)
; workspace: ([active] (ordered by viewport arrangement?) [stashed] (ordered by recency of stashing? stack?) interaction reflog)
; git analogy
  ;all revs have common root
  ;branches and tags w/ preferred view info?
  ;synch-able clones
  ;shared editing w/ read/write capabilities
  ;views are a separate concept
    ;large granularity git commit analogy would be more like version control on views
    ;projects/repos are indirect, only existing within hierarchical branch names for terms
      ;ie. project-name/feature-branch-name/term-name
      ;viewing a project is the same as filtering terms by project-name/

(record revision uid parent action content)
(record interaction uid nav-key rev forward-revs branch metadata)

(record url-entry target metadata)
(define (url-entry-new target) (url-entry target (hash)))
(define (url-attribute uref) (list* "attribute" uref))
(define (url-revision uref) (list* "revision" uref))
(define (url-interaction uref) (list* "interaction" uref))
(define (url-branch uref) (list* "branch" uref))

(record database url-tree uid-next revisions interactions)
(define database-empty (database url-tree-empty 0 (hash) (hash)))

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
  result = (:%.* (const (void)) db type uid)
  (if (void? result) (left "does not exist") (right result)))
(define (db-revision-get db uid) (db-get db 'revisions uid))
(define (db-interaction-get db uid) (db-get db 'interactions uid))
(define (db-add type new db . args)
  (lets
    uid = (:.* db 'uid-next)
    db = (:~* db (curry + 1) 'uid-next)
    resource = (apply new uid args)
    db = (:%=* (const (void)) db resource type uid)
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

;(record workspace motd-doc cviews)
;(record viewer model-doc model)


;; old

;(record workspace-view tab-index ic-indices)
;(record workspace tabs interaction-db)
;(record tab layout interaction-uids)

;(record interaction pretty current history)
;(record interaction-db uid->interaction name<->uid active-uids discarded-uids preserved)

;(define (interaction-new term)
  ;(interaction (curry unparse upenv-empty) (::0 term) '()))

;(define interaction-db-empty (interaction-db (hash) (hash) (set) (set) (hash)))
;(define/destruct (interaction-db-add
                   ;(interaction-db uid->i name<->uid active discarded preserved)
                   ;interaction)
  ;(let* ((uid (+ 1 (apply max (cons -1 (hash-keys uid->i)))))
         ;(uid->i (hash-set uid->i uid interaction))
         ;(name<->uid (hash-set* name<->uid uid (set) (~a uid) uid))
         ;(active (set-add active uid)))
    ;(list (interaction-db uid->i name<->uid active discarded preserved)
          ;uid)))

;(define tab-empty (tab (void) '()))
;(define/destruct (tab-add (tab layout iuids) ic-index iuid)
  ;(tab layout (:~ iuids (curry cons iuid) (list-path ic-index))))
;(define workspace-empty (workspace (list tab-empty) interaction-db-empty))
;(define workspace-view-empty (workspace-view -1 '()))

;;; presentation
;(define major-divider "================================\n")
;(define minor-divider "----------------\n")
;(define select-divider "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n")

;(define ((bracket left right) str) (string-append left str right))
;(define bracket-ic-view (bracket select-divider select-divider))
;(define bracket-tab (bracket "[" "]"))

;(define (present-cterm pretty cterm)
  ;(string-join (list
                  ;(pretty-string (cursor-trail cterm))
                  ;(pretty-string (pretty (::.* cterm))))
                ;minor-divider))

;(define/destruct (present-interaction (interaction pretty current history))
  ;(string-join (list
                 ;(present-cterm pretty current)
                 ;(format "history length: ~a\n" (length history)))
               ;minor-divider))

;(define/destruct (present-tab uid->interaction ic-index (tab layout iuids))
  ;(let* ((interactions (map (curry dict-ref uid->interaction) iuids))
         ;(presented-interactions (map present-interaction interactions))
         ;(ic-views (map string-append
                     ;(map (curry format (string-append "< ~a >\n" minor-divider))
                          ;(range (length interactions)))
                     ;presented-interactions))
         ;(ic-views (:~ ic-views bracket-ic-view (list-path ic-index 'first))))
    ;(string-join ic-views major-divider)))

;(define/destruct (present-workspace (workspace-view tab-index ic-indices)
                                    ;(workspace tabs idb))
  ;(if (< tab-index 0) "tabs: ..."
    ;(let* ((uid->interaction (interaction-db-uid->interaction idb))
          ;(ic-index (list-ref ic-indices tab-index))
          ;(tab (list-ref tabs tab-index))
          ;(tab-names (map ~a (range (length tabs))))
          ;(tab-names (:~ tab-names bracket-tab (list-path tab-index 'first)))
          ;(tab-view (present-tab uid->interaction ic-index tab)))
      ;(string-join
        ;(list tab-view
              ;(string-append "tabs: " (string-join tab-names " ") "\n"))
        ;major-divider))))
