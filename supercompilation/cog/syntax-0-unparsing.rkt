#lang racket
(provide
  unparse
  unparse-orec
  unparse-value-orec
  upenv-empty
  )

(require
  "syntax-0-parsing.rkt"
  "syntax-abstract.rkt"
  gregr-misc/list
  gregr-misc/match
  gregr-misc/record
  gregr-misc/sugar
  )

(record upenv vars)
(define upenv-empty (upenv '()))
; TODO: use more of the alphabet
(define (upenv-next-name vars)
  (string->symbol (format "$x~a" (length vars))))
(define (upenv-free-name upe idx)
  (string->symbol (format "$free~a" (- idx (length (upenv-vars upe))))))
(define/destruct (upenv-vars-add (upenv vars) (lattr name _ _))
  (let ((next-name (if (equal? (void) name) (upenv-next-name vars) name)))
    (cons next-name (upenv (cons next-name vars)))))
(define/destruct (upenv-vars-extend (upenv vars) names lift)
  (upenv (append names (drop vars (min (length vars) lift)))))
(define (upenv-vars-get upe idx)
  (let ((vars (upenv-vars upe)))
    (if (< idx (length vars)) (list-ref vars idx) (upenv-free-name upe idx))))

(define (unparse upe term)
  (unparse-orec unparse unparse-value upe term))
(define (unparse-value upe term)
  (unparse-value-orec unparse unparse-value upe term))
(define (unparse-orec unparse unparse-value upe term)
  (define (unparse-thunk upe thunk)
    (match (unparse-value upe thunk)
      (`(lam (,_)            ,body) body)
      (`(lam ,(cons x names) ,body) `(lam ,names ,body))))
  (match term
    ((subst sub tm)
     (match-let (((list uses names lift) (unparse-subst unparse-value upe sub)))
       `(subst ,uses ,lift ,(unparse (upenv-vars-extend upe names lift) tm))))
    ((value v) (unparse-value upe v))
    ((produce tm) `(produce ,(unparse upe tm)))
    ((pair-access idx pr) (list* 'pair-access
                                 (map* (curry unparse-value upe) idx pr)))
    ((lam-apply tproc targ)
     (unparse-application unparse upe tproc (list targ)))))
(define (unparse-application unparse upe tproc targs)
  (match tproc
    (_ (map (curry unparse upe) (cons tproc targs)))))
(define (unparse-value-orec unparse unparse-value upe val)
  (match val
    ((bit b)    (unparse-value-bit b))
    ((uno)      '())
    ((pair l r)
     (let ((fl (unparse-value upe l)))
      (match (unparse-value upe r)
        ('()               `(tuple ,fl))
        (`(tuple . ,elems) `(tuple . ,(cons fl elems)))
        (fr                `(pair ,fl ,fr)))))
    ((bvar idx) (upenv-vars-get upe idx))
    ((lam attr body)
     (match-let (((cons new-name new-upe) (upenv-vars-add upe attr)))
       (match (unparse new-upe body)
         (`(lam ,names ,body) (list 'lam (cons new-name names) body))
         (body                (list 'lam (list new-name) body)))))))
(def (unparse-subst unparse-value upe (substitution uses lift))
  unparsed-uses =
  (forl
    (substitution-use (lattr name _ _) val) <- uses  ; TODO: use entire attr
    (list (list name '= (unparse-value upe val)) name))
  (list assignments names) = (zip-default unparsed-uses '(() ()))
  (list assignments names lift))

(define (unparse-value-bit vb)
  (match vb
    ((b-0) 0)
    ((b-1) 1)))
