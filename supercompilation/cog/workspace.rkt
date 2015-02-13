#lang racket
(provide
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
  ;gregr-misc/list
  ;gregr-misc/match
  gregr-misc/record
  )

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
