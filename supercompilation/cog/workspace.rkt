#lang racket
(provide (all-defined-out))

(require
  "syntax-0-unparsing.rkt"
  "syntax-abstract.rkt"
  "util.rkt"
  )

(record workspace tab-cursor interaction-db)
(record tab layout interaction-uid-cursor)

(record interaction pretty current history)
(record interaction-db uid->interaction name<->uid active-uids discarded-uids preserved)

(define (interaction-new term)
  (interaction (curry unparse upenv-empty) (::0 term) '()))
(define interaction-minimal (interaction-new (value (uno))))

(define interaction-db-empty (interaction-db (hash) (hash) (set) (set) (hash)))
(define/destruct (interaction-db-add
                   (interaction-db uid->i name<->uid active discarded preserved)
                   interaction)
  (let* ((uid (+ 1 (apply max (cons -1 (hash-keys uid->i)))))
         (uid->i (hash-set uid->i uid interaction))
         (name<->uid (hash-set* name<->uid uid (set) (~a uid) uid))
         (active (set-add active uid)))
    (list (interaction-db uid->i name<->uid active discarded preserved)
          uid)))

(define tab-empty (tab (void) (::0 '())))
(define/destruct (tab-add (tab layout ic) iuid)
  (tab layout (::~ ic (curry cons iuid))))
(define (tab-minimal idb)
  (match-let (((list idb uid) (interaction-db-add idb interaction-minimal)))
    (list idb (tab-add tab-empty uid))))

(define/destruct (workspace-normalize (workspace ctab idb))
  (if (< 0 (length (::^*. ctab)))
    (workspace ctab idb)
    (match-let (((list idb new-tab) (tab-minimal idb)))
      (workspace (::0 (list new-tab)) idb))))
(define workspace-empty (workspace (::0 '()) interaction-db-empty))
(define workspace-minimal (workspace-normalize workspace-empty))

;; presentation
(define major-divider "================================\n")
(define minor-divider "----------------\n")
(define select-divider "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n")

(define ((bracket left right) str) (string-append left str right))
(define bracket-view (bracket select-divider select-divider))
(define bracket-tab (bracket "[" "]"))

(define (present-cterm pretty cterm)
  (string-join (list
                  (pretty-string (cursor-trail cterm))
                  (pretty-string (pretty (::. cterm))))
                minor-divider))

(define/destruct (present-interaction (interaction pretty current history))
  (string-join (list
                 (present-cterm pretty current)
                 (format "history length: ~a\n" (length history)))
               minor-divider))

(define/destruct (present-tab uid->interaction (tab layout ic))
  (let* ((interactions
           (map (curry dict-ref uid->interaction) (::^*. ic)))
         (presented-interactions (map present-interaction interactions))
         (views (map string-append
                     (map (curry format (string-append "< ~a >\n" minor-divider))
                          (range (length interactions)))
                     presented-interactions))
         (index (length (cursor-trail ic)))
         (views (:~ views bracket-view (list-path index 'first))))
    (string-join views major-divider)))

(define/destruct (present-workspace (workspace ctab idb))
  (let* ((index (length (cursor-trail ctab)))
         (tabs (::^*. ctab))
         (tab (list-ref tabs index))
         (tab-names (:~ (map ~a (range (length tabs)))
                        bracket-tab (list-path index 'first)))
         (uid->interaction (interaction-db-uid->interaction idb)))
    (string-join (list
                   (present-tab uid->interaction tab)
                   (string-append "tabs: " (string-join tab-names " ") "\n"))
                 major-divider)))
