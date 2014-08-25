#lang racket
(provide (all-defined-out))

(require
  "util.rkt"
  )

(record workspace tab-cursor interaction-db)
(record tab layout interaction-uid-cursor)

(record interaction pretty current history)
(record interaction-db uid->interaction name<->uid active-uids discarded-uids preserved)

(define interaction-db-empty (interaction-db (hash) (hash) (set) (set) (hash)))

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
         (views (:~ views bracket-view (list-path index))))
    (string-join views major-divider)))

(define/destruct (present-workspace (workspace ctab idb))
  (let* ((index (length (cursor-trail ctab)))
         (tabs (::^*. ctab))
         (tab (list-ref tabs index))
         (tab-names (:~ (map ~a (range (length tabs)))
                        bracket-tab (list-path index)))
         (uid->interaction (interaction-db-uid->interaction idb)))
    (string-join (list
                   (present-tab uid->interaction tab)
                   (string-append "tabs: " (string-join tab-names " ") "\n"))
                 major-divider)))
