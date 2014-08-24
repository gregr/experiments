#lang racket
(provide (all-defined-out))

(require
  "syntax-0-parsing.rkt"
  "syntax-abstract.rkt"
  "util.rkt"
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
  (define (thunk-form? val)
    (define (mentions-bvar-value? idx val)
      (match val
        ((bvar index) (equal? index idx))
        ((pair v0 v1)
         (or (mentions-bvar-value? idx v0) (mentions-bvar-value? idx v1)))
        ((lam _ body) (mentions-bvar? (+ idx 1) body))
        (_ #f)))
    (define (mentions-bvar-subst? idx sub)
      (match sub
        ((bvar-lift _)      #f)
        ((bvar-use _ val sub) (or (mentions-bvar-value? idx val)
                                  (mentions-bvar-subst? idx sub)))))
    (define (mentions-bvar? idx term)
      (match term
        ((value val) (mentions-bvar-value? idx val))
        ((produce tm) (mentions-bvar? idx tm))
        ((subst sub tm) (or (mentions-bvar-subst? idx sub)
                            (mentions-bvar? idx tm)))
        ((action-2 _ t0 t1)
         (or (mentions-bvar? idx t0) (mentions-bvar? idx t1)))))
    (match val
      ((lam _ body) (not (mentions-bvar? 0 body)))
      (_ #f)))
  (define (unparse-thunk upe thunk)
    (match (unparse-value upe thunk)
      (`(lam (,_)            ,body) body)
      (`(lam ,(cons x names) ,body) `(lam ,names ,body))))
  (match term
    ((value v) (unparse-value upe v))
    ((produce tm) `(produce ,(unparse upe tm)))
    ((subst sub tm)
     (match-let (((list uses names lift) (unparse-subst unparse-value upe sub)))
       `(subst ,uses ,lift ,(unparse (upenv-vars-extend upe names lift) tm))))
    ((action-2 (lam-apply)
      (action-2 (pair-access) tcnd
        (value (pair (? thunk-form? alt-0) (? thunk-form? alt-1))))
      (value (uno)))
     `(if-0 ,(unparse upe tcnd)
            ,(unparse-thunk upe alt-0) ,(unparse-thunk upe alt-1)))
    ((action-2 (lam-apply) tproc targ)
     (unparse-application unparse upe tproc (list targ)))
    ((action-2 (pair-access) (value (bit b)) tpair)
     (list (match b ((b-0) 'pair-l) ((b-1) 'pair-r)) (unparse upe tpair)))
    ((action-2 act t0 t1)
     (unparse-action-2 act (unparse upe t0) (unparse upe t1)))))
(define (unparse-application unparse upe tproc targs)
  (match tproc
    (_ (map (curry unparse upe) (cons tproc targs)))))
(define (unparse-action-2 act f0 f1)
  (match act
    ((pair-access) `(pair-access ,f0 ,f1))))
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
(define (unparse-subst unparse-value upe sub)
  (let loop ((upe upe) (sub sub) (uses '()) (names '()))
    (match sub
      ((bvar-lift k) (list (reverse uses) (reverse names) k))
      ((bvar-use (lattr name _ _) val sub)  ; TODO: use entire attr
       (loop upe sub (cons (list name '= (unparse-value upe val)) uses) (cons name names))))))

(define (unparse-value-bit vb)
  (match vb
    ((b-0) 0)
    ((b-1) 1)))
