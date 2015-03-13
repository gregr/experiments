#lang racket
(provide
  )

(require
  "syntax-abstract.rkt"
  gregr-misc/navigator
  )

(module+ test
  (require rackunit))

; inner (D) vs. outer (d, it's more common?) delete

; registers?
; yank (y) (or copy (c)?)
; paste (p)
; delete (d, D)
; cut (x) = yank, delete
; introduce (i) (only works when uno is selected?)
  ; inward vs. outward introduction?
  ; (i) inward, going after all unos in order, followed by outer
  ; (I) skip straight to outer
; replace (r) (or change (c)?) = delete, introduce
; w,b : tree traversal jumps rightward and leftward (pair and apply) ?

; TODO: introductions from some kind of menu

(define v-uno (uno))
(define v-0 (bit (b-0)))
(define v-1 (bit (b-1)))
(define v-uno-pair (pair v-uno v-uno))
(define value-introductions
  (list
    (lambda (_) v-uno)
    (lambda (_) v-0)
    (lambda (_) v-1)
    (lambda (l) (pair l v-uno))
    (lambda (r) (pair v-uno r))
    (lambda (v) (lam lattr-void (value v)))
    (lambda (idx) (pair-access idx v-uno-pair))
    (lambda (payload) (pair-access v-0 payload))
    (lambda (payload) (pair-access v-1 payload))
    ; TODO: available bvars
    ))

(define tv-uno (value v-uno))
(define tv-0 (value v-0))
(define tv-1 (value v-1))
(define tv-id (value (lam lattr-void (bvar 0))))
(define term-introductions
  (list
    (lambda (_) tv-uno)
    (lambda (_) tv-0)
    (lambda (_) tv-1)
    (lambda (t) (value (lam lattr-void t)))
    (lambda (proc) (lam-apply proc tv-uno))
    (lambda (arg) (lam-apply tv-id arg))))

;;pair-access
  ;;introduce: embed subvalue left/right
    ;;pair-left, pair-right, if-0/1?
  ;;erase left/right
  ;;swap

;;lam-apply
  ;;introduce: embed subterm left/right
    ;;let? seq? abstract/generalize as in learnable-prog?

;;erase,drop,prune,trim uses this; so does introduce?
(define nterm-replace navigator-focus-set)


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
