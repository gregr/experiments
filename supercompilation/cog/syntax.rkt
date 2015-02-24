#lang racket
(provide
  binders-get
  binders-empty
  gather-applications
  gather-lams
  nav-path-binders
  subst-binders
  )

(require
  "syntax-abstract.rkt"
  gregr-misc/cursor
  gregr-misc/record
  gregr-misc/sugar
  )

(record binders names)
(define binders-empty (binders '()))
(define (binders-next-name names)
  (string->symbol (format "$x~a" (length names))))
(define (binders-free-name env idx)
  (string->symbol (format "$free~a" (- idx (length (binders-names env))))))
(def (binders-add (binders names) (lattr name _ _))
  next-name = (if (equal? (void) name) (binders-next-name names) name)
  (list next-name (binders (cons next-name names))))
(def (binders-extend (binders names) attrs lift)
  env = (binders (drop names (min (length names) lift)))
  (forf
    (list names env) = (list '() env)
    attr <- (reverse attrs)
    (list next-name env) = (binders-add env attr)
    (list (list* next-name names) env)))
(def (binders-get env idx)
  names = (binders-names env)
  (if (< idx (length names)) (list-ref names idx) (binders-free-name env idx)))

(def (subst-binders env uses lift)
  attrs = (forl (substitution-use attr _) <- uses
                attr)
  (binders-extend env attrs lift))
(define (gather-lams env t/v)
  (match t/v
    ((value (lam attr body))
     (lets
       (list new-name new-env) = (binders-add env attr)
       (list env names body) = (gather-lams new-env body)
       (list env (list* new-name names) body)))
    (body (list env '() body))))
(define (gather-applications t/v)
  (letsn loop (proc = t/v args = '())
    (match proc
      ((lam-apply proc arg) (loop proc (list* arg args)))
      (proc (list* proc args)))))

(define (nav-path-binders env focus path)
  (match path
    ('() env)
    ((cons key path)
     (lets
       new-env =
       (match focus
         ((lam attr body) (second (binders-add env attr)))
         ((subst (substitution uses lift) t)
          (second (subst-binders env uses lift)))
         (_ env))
       (nav-path-binders new-env (:.* focus key) path)))))
