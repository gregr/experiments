#lang racket
(provide
  binders-empty
  binders-get
  binders-names
  gather-applications
  gather-lams
  nav-paths->binders
  subst-binders
  )

(require
  "syntax-abstract.rkt"
  gregr-misc/cursor
  gregr-misc/generator
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
  (letn loop (values proc args) = (values t/v '())
    (match proc
      ((lam-apply proc arg) (loop proc (list* arg args)))
      (proc (list* proc args)))))

(define (nav-paths->binders env focus paths)
  (gen->list
    (gn yield (_)
      (forf
        (list focus env) = (list focus env)
        path <- paths
        (list focus env) =
        (forf
          (list focus env) = (list focus env)
          key <- path
          new-env =
          (match focus
            ((lam attr body) (second (binders-add env attr)))
            ((subst (substitution uses lift) t)
             (second (subst-binders env uses lift)))
            (_ env))
          (list (:.* focus key) new-env))
        _ = (yield env)
        (list focus env)))))
