#lang racket

; normal order
w x = x x

y f = u u
  where u x = f (x x)

; applicative order
y f = u u
  where u x arg = f (x x) arg

; typing
y f = (u u):b
  where u x = (f (x x):b):c
x: a. a -> b
f: b -> c
u: x
c: b
y: (b -> b) -> b

fact0 fact n = ...
fact0 : (n -> n) -> (n -> n)
y fact0 : n -> n

w x = (x x):b
x: a. a -> b
w: (a.(a -> b)) -> b

;  expr ::= var:t
;         | (expr:(a->b) expr:a):b
;         | (x:a -> expr:b):(a->b)
;         | (expr:{v:t} #@ expr:label_v):t
;         | (_construct expr:tag_n expr0 ... exprn):tagged_n
;         | (_destruct expr:tagged_n expr:((arg0 ... argn)->r)):r
;         | array? tuple? simple-hash? (what a record really is?) / ephemeral/extensible varieties? (linear?)
;         | (get-tag expr:tagged_t):tag
;         | get-type/get-region/etc.
;         | metafy vs. reify (metafy lifts values to type level / reify lowers types to value level) -- for dependent typing
;         | concretize vs. abstract (abstract creates abstract values of a type rather than types as values -- concretize builds refined types that refer only to specific values) -- for abstract interpretation
;
;data representation:
;  arrays (like a homogeneous ordered dense struct)
;  (ordered or unordered) dense structs
;  (ordered (trees) or unordered (hashes) sparse structs
;  strings? buffers? other low-level or unsafe data
;
;ordered reprs implicitly support unlabeled constructors/destructors
;unordered reprs require field-labeled constructors/destructors
;each repr has both a tag for construction as well as destruction so that capabilities can be separated
;all constructor/destructor forms require the proper tags to be present
;
;case-analysis/record-selection yields intersection type
;
;public vs. private tags
;plain array/struct tags public (globally indexable via data representation description)
;named data tags private (explicitly exported to users as appropriate)
;
;value-level tagged data
;Nil -- get-tag yields Nil-tag
;Cons x y -- get-tag yields Cons-tag
;
;type-level tagged data (after reification)
;List a -- get-tag yields List-tag

; Man, racket sucks for defining sum types
; not sure this will be relevant for the interpreter
(struct repr-ref () #:transparent) ; reference to non-local sub-structure
(struct repr-scalar (desc) #:transparent) ; atomic data: symbol, int, float, string, file descriptor, etc.
(struct repr-array (length elem) #:transparent)
(struct repr-struct (dense? ordered? elems) #:transparent) ; unordered: elems will map fields to elements
(struct repr-sum (tags->reprs) #:transparent) ; tagged union of reprs; only 2 or more make sense (1 redundant, 0 illegally implies void/bottom)
(struct repr-sum-any () #:transparent) ; the full open sum, like an infinite sum of repr-refs (so it contains a full-sized tag plus a ref)

(struct val-undefined () #:transparent) ; void type
(struct val-indirect (val) #:mutable #:transparent)
(struct val-tagged (tag val) #:transparent)
(define undefined (val-undefined))

(define (env-new) (make-immutable-hasheq '()))
(define (env-extend env sym val) (hash-set env sym val))
(define (env-getd env sym default)
  (match (hash-ref env sym default)
    ((val-indirect val) val)
    (val val)))
(define (env-get env sym) (env-getd env sym undefined))
(define (env-get-checked env sym)
  (let ((val (env-get env sym)))
    (if (eq? val undefined)
      (error 'env-get-checked "referenced unbound variable: ~s" sym) val)))

; should defer data org until after expressions can be typed

(struct typed-term (term type env frees) #:transparent)

; todo: mu tagging for tagging recursive types and unfold locations?
; todo: array/struct types other than record?
(struct type-void () #:transparent)
(struct type-any () #:transparent)
(struct type-constr (tag elemss) #:transparent) ; implicit intersection, so elemss is a list of lists, implied by double plural
;todo: records as predicates instead? one per label required (struct type-record (labels->elems) #:transparent)
(struct type-intersect (elems) #:transparent) ; for vars: they can be on the left of multiple <: constraints
;todo: are these necessary? (struct type-union (constrs intersects vars) #:transparent) ; TODO: should types be \/ or /\ major?
(struct type-var (sym) #:transparent) ; todo: var chain folding optimization in env
(define ty-void (type-void))
(define ty-any (type-any))

(define type-constr-variances (make-hasheq '()))
(define (type-constr-variance-set tag vars) ; list of 'covar|'contravar|'invar
  (hash-set! type-constr-variances tag vars))
(define (type-constr-variance-get tag) (hash-ref type-constr-variances tag))
(define tag-proc '->)
(type-constr-variance-set tag-proc (list 'contravar 'covar))

(define (type-meet-constr tag as bs env) 'todo)
(define (type-join-constr tag as bs env) 'todo)
(define (type-meet ta tb tenv)
  (match* (ta tb)
    (((type-void) _) ty-void)
    ((_ (type-void)) ty-void)
    (((type-any) ty) ty)
    ((ty (type-any)) ty)
    (((type-constr tag args-a) (type-constr tag args-b)) (type-meet-constr tag args-a args-b tenv))
    (((type-constr _ _) (type-constr _ _)) ty-void)
    ((_ _) (error 'type-meet "incompatible types: ~s and ~s" ta tb))))
(define (type-join ta tb tenv)
  (match* (ta tb)
    (((type-void) ty) ty)
    ((ty (type-void)) ty)
    (((type-any) _) ty-any)
    ((_ (type-any)) ty-any)
    (((type-constr tag args-a) (type-constr tag args-b)) (type-join-constr tag args-a args-b tenv))
    (((type-constr tag-a args-a) (type-constr tag-b args-b)) 'todo)
    ((_ _) (error 'type-join "incompatible types: ~s and ~s" ta tb))))

(struct term-var (sym) #:transparent)
(struct term-app (proc arg) #:transparent)
(struct term-proc (param body) #:transparent)
(struct term-letrec (param arg body) #:transparent)
(struct term-if0 (condition consequent alternative) #:transparent)
(struct term-lit (data) #:transparent) ; include native procedures
;(struct term-destr (tag proc tval) #:transparent)
;(struct term-tagof (tval) #:transparent)
;(struct term-table (default-val lab-val-pairs) #:transparent)
;(struct term-table-get (tab lab) #:transparent)



(struct val-proc (binder body env) #:transparent)
;(struct val-table (default labs->vals) #:transparent)

;(define (table-build dval lvs) (val-table dval (make-hash lvs)))
;(define (table-get tab lab)
;  (match tab
;    ((val-table dval lvs) (hash-ref lvs lab dval))
;    (_ (error 'table-get
;         "attempted to get labeled value from non-table ~s" tab))))
;(define (tagged-tagof tval)
;  (match tval
;    ((val-tagged tag args) tag)
;    (_ tval)))
;(define (tagged-destruct tag proc tval)
;  (match tval
;    ((val-tagged tag0 args)
;     (if (equal? tag0 tag) (apply-proc proc args)
;       (error 'tagged-destruct
;         "attempting to destruct tag ~s when expecting ~s" tag0 tag)))
;    (_ (error 'tagged-destruct
;         "attempted to destruct non-tagged-val ~s" tval))))
;
(define (apply-proc proc arg)
  (match proc
    ((val-proc binder body env)
      (eval-term body (env-extend env binder arg)))
    (_ (proc arg)))) ; native proc

(define (eval-term term env)
  (match term
    ((term-var sym) (env-get-checked env sym))
    ((term-app proc arg)
     (let ((proc (eval-term proc env)) (arg (eval-term arg env)))
       (apply-proc proc arg)))
    ((term-proc param body) (val-proc param body env))
    ((term-letrec param arg body)
     (let* ((ind (val-indirect undefined)) (env (env-extend env param ind)) (arg (eval-term arg env)))
       (set-val-indirect-val! ind arg)
       (eval-term body env)))
    ((term-if0 cnd cns alt)
     (if (equal? 0 (eval-term cnd env)) (eval-term cns env) (eval-term alt env)))
    ((term-lit data) data)))
;    ((term-destr tag proc tval)
;     (let ((tag (eval-term tag env)) (proc (eval-term proc env)) (tval (eval-term tval env)))
;       (tagged-destruct tag proc tval)))
;    ((term-tagof tval) (let ((tval (eval-term tval env))) (tagged-tagof tval)))
;    ((term-table dval lvs)
;     (let ((dval (eval-term dval env))
;           (lvs (map (lambda (lv) (cons (eval-term (car lv) env)
;                                        (eval-term (cdr lv) env))) lvs)))
;       (table-build dval lvs)))
;    ((term-table-get tab lab)
;     (let ((tab (eval-term tab env)) (lab (eval-term lab env)))
;       (table-get tab lab)))))


(define (flip2 fn) (lambda (x y) (fn y x)))
(define (curry2 fn) (lambda (x) (lambda (y) (fn x y))))

(define (build-term data)
  (match data
    ((? symbol?) (term-var data))
    (`(proc ,param ,body) (term-proc param (build-term body)))
    (`(letrec (,param ,arg) ,body) (term-letrec param (build-term arg) (build-term body)))
    (`(if0 ,cnd ,cns ,alt) (term-if0 (build-term cnd) (build-term cns) (build-term alt)))
    ;(`(destr ,tag ,proc ,tval) (term-destr (build-term tag) (build-term proc) (build-term tval)))
    ((list single) (build-term single))
    ((? list?) (foldl (flip2 term-app) (build-term (car data)) (map build-term (cdr data))))
    (_ (term-lit data))))


(define ex-env (foldl (lambda (symval env) (env-extend env (car symval) (cadr symval))) (env-new)
                      `((+ ,(curry2 +)) (- ,(curry2 -)) (* ,(curry2 *)))))
;(define ex-term (build-term '(+ 1 2)))
(define ex-term (build-term
  '(letrec (factorial
            (proc acc (proc n (if0 n acc (factorial (* acc n) (- n 1))))))
     (factorial 1 7))
))

;(define (type-=-constraints ca cb tenv))
;(define (type-=-vars sa sb tenv))

;(define (type-<=-vars sa sb tenv)) ; constraint ub acyclic chaining?

(define (type-unify ta tb tenv)
  (match* (ta tb)
    (((type-var sym-a) (type-var sym-b))
      (let ((val-a (env-get sym-a)) (val-b (env-get sym-b)))
        (type-meet val-a val-b)
        ))))

;(define (type-meet ta tb))
;(define (type-join ta tb))
;(define (type-unify ta tb))
;(define (type-semi-unify ta tb)) ; ta <= tb

; todo: env dependency graphs: merging heads/chains?
;(define (env-meet ea eb))

(struct expr-var (sym) #:transparent) ; de bruijn indices or actual symbols?
(struct expr-app (proc arg) #:transparent)
(struct expr-proc (param body) #:transparent)
(struct expr-consr (tag args) #:transparent)
(struct expr-destr (tag tval recv) #:transparent)
(struct expr-get-tag (tval) #:transparent)
(struct expr-field-get (rec lab) #:transparent)

;(define (expr->type expr)
;  (match expr
;    ((expr-var sym) (list (type-var sym) (env-new)))
;    ((expr-app proc arg)
;     (match-let (((list tproc tproc-env) (expr->type proc)) ((list targ targ-env) (expr->type arg)) (tret (gensym)))
;       (match-let (((type-constr '-> '(targp tretp)) tproc))
;         (list tret (env-unify (env-unify tproc-env targ-env) (env-extend (env-extend (env-new) targp targ) tretp tret))))))
;    ((expr-proc param body)
;     (match-let (((list tbody tbody-env) (expr->type body)))
;       (list (type-constr ('-> (list (type-var param) tbody))) tbody-env)))))
