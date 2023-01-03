#lang racket/base
(provide (all-defined-out))
(require racket/list racket/match racket/set)

;;; microVerse
;;; based on: https://simon.peytonjones.org/assets/pdfs/verse-conf.pdf

;; TODO: IO operations?
;; - immediate read/write on a channel: sort of like unsafePerformIO
;; - scoped IO: an IO handler that receives and services IO request descriptions, sort of like the IO monad
;; - transactional IO: a (mozart/oz-style) single-assignment stream of IO requests is gradually assigned to by concurrent processes

(define env.empty '())

(define (env-extend  env name  id)  (cons (cons name id) env))
(define (env-extend* env name* id*) (append (map cons name* id*) env))

(define (env-ref env name)
  (let ((kv (assoc name env)))
    (if kv (cdr kv) (error "unbound variable" name))))

(define (env-rename env id->id)
  (map cons (map car env) (map id->id (map cdr env))))

;; TODO: subst garbage collection
(struct subst (local* x=>v) #:prefab)
(define S-local* subst-local*)
(define S-x=>v   subst-x=>v)

(define S.empty (subst (set) '()))

(define (S-extend S x v)
  (subst (S-local* S) (cons (cons (var-id x) v) (S-x=>v S))))

(define (S-ref S x k.found k.not-found)
  (let ((kv (assv (var-id x) (S-x=>v S))))
    (if kv (k.found (cdr kv)) (k.not-found))))

(define (S-domain S) (list->set (map car (S-x=>v S))))

(define (S-local? S x) (set-member? (S-local* S) (var-id x)))

(define (S-clear-local* S) (subst (set) (S-x=>v S)))

(define (S-exist S id*)
  (subst (foldl (lambda (id id*) (set-add id* id)) (S-local* S) id*) (S-x=>v S)))

(define (S-diff S S.parent)
  (let ((x=>v.parent (S-x=>v S.parent)))
    (subst (set-subtract (S-local* S) (S-local* S.parent))
           (let loop ((x=>v (S-x=>v S)))
             (cond
               ((eq? x=>v x=>v.parent) '())
               (else                   (cons (car x=>v) (loop (cdr x=>v)))))))))

(define (S-rename S id->id)
  (subst (list->set (map id->id (set->list (S-local* S))))
         (let ((x=>v (S-x=>v S)))
           (map cons
                (map id->id (map car x=>v))
                (map (lambda (v) (value-rename v id->id)) (map cdr x=>v))))))

(define (value-rename v id->id)
  (let loop ((v v))
    (cond
      ((var?    v) (var (id->id (var-id v))))
      ((pair?   v) (cons (loop (car v)) (loop (cdr v))))
      ((vector? v) (list->vector (map loop (vector->list v))))
      ((clo?    v) (clo (clo-param v) (clo-body v) (env-rename (clo-env v) id->id)))
      (else        v))))

(define (S-reify S E.body)
  (let ((id* (sort (set->list (S-local* S)) <))
        (b*  (seq* (map (lambda (xv) (== (value (var (car xv))) (value (cdr xv))))
                        (sort (S-x=>v S) (lambda (kv.a kv.b) (< (car kv.a) (car kv.b))))))))
    (define (build E) (nexist id* (nseq b* E)))
    (match E.body
      (`(alt ,a ,b) (alt (build a) (build b)))
      (_            (build E.body)))))

(define (walk S v)
  (if (var? v)
      (S-ref S v
             (lambda (w) (walk S w))
             (lambda () v))
      v))

;; This implements path compression without leaking references to large values.
(define (walk-var S v)
  (if (var? v)
      (S-ref S v
             (lambda (w)
               (if (var? w)
                   (walk-var S w)
                   v))
             (lambda () v))
      v))

(define (walk* S v)
  (let loop ((v v))
    (let ((v (walk S v)))
      (cond
        ((pair?   v) (cons (loop (car v)) (loop (cdr v))))
        ((vector? v) (list->vector (loop (vector->list v))))
        ;; TODO: for clo, store portion of S reachable from rhs of clo-env
        (else        v)))))

;; Acyclic unify
(define (unify S u v)
  (let ((u (walk S u)) (v (walk S v)))
    (cond
      ((eqv? u v)  S)
      ((var? u)    (if (and (var? v) (var=? u v)) S (assign S u v)))
      ((var? v)    (assign S v u))
      ((pair? u)   (and (pair? v) (let ((S (unify S (car u) (car v))))
                                    (and S (unify S (cdr u) (cdr v))))))
      ((vector? u) (and (vector? v)
                        (= (vector-length u) (vector-length v))
                        (unify S (vector->list u) (vector->list v))))
      ((string? u) (and (string? v) (string=? u v) S))
      (else        #f))))

(define (occurs? S x v)
  (let loop ((v (walk S v)))
    (cond
      ((eqv? x v)  #t)
      ((pair? v)   (or (loop (car v)) (loop (cdr v))))
      ((vector? v) (loop (vector->list v)))
      (else        #f))))

;; NOTE: if we use a fancy evaluation strategy, this assignment may want to schedule
;; work that depends on x.
(define (assign S x v)
  (and (not (occurs? S x v))
       (if (and (not (S-local? S x)) (var? v) (S-local? S v))
           (S-extend S v x)  ; just in case x is rigid
           (S-extend S x v))))

;; NOTE: Special behavior for heap values is not necessary as long as we use
;; eqv? to compare all values by quasi-identity.  This will even have the
;; paper's behavior for closures, even though the paper disequates all closures.
;; NOTE: however, because we allow cyclic equalities (no occurs check), we
;; still need special support to detect loops in some cases:
;; e.g.,
;;   (== x (vector 1 y)) ;
;;   (== y (vector 1 x)) ;
;;   (== x y)
;; should succeed, and
;;   (== x (vector 1 x)) ;
;;   (== y (vector 1 y)) ;
;;   (== x y)
;; should also succeed.  We can achieve this by remembering all unifications in
;; progress, and immediately succeeding if we revisit one.
;;
;; Cyclic unification (I think we don't need this.  Let's try acyclic first.)
;(define (unify S u v)
;  (define (cyclic=? seen u v)
;    (let loop ((seen seen))
;      (match seen
;        ((cons (cons j k) seen) (or (and (eq? j u) (eq? k v)) (loop seen)))
;        (_                      #f))))
;  (let loop ((seen '()) (S S) (u u) (v v))
;    (let ((u (walk S u)) (v (walk S v)))
;      (cond
;        ((eqv? u v)  S)
;        ((var? u)    (if (and (var? v) (var=? u v)) S (assign S u v)))
;        ((var? v)    (assign S v u))
;        ((pair? u)   (and (pair? v)
;                          (if (cyclic=? seen u v)
;                              S
;                              (let* ((seen (cons (cons u v) seen))
;                                     (S    (loop seen) S (car u) (car v)))
;                                (and S (loop seen S (cdr u) (cdr v)))))))
;        ((vector? u) (and (vector? v)
;                          (= (vector-length u) (vector-length v))
;                          (loop seen S (vector->list u) (vector->list v))))
;        (else        #f)))))

(struct var (id)             #:prefab)
(struct clo (param body env) #:prefab)

(define (var=? u v) (eqv? (var-id u) (var-id v)))

(define (ref name)       `(ref ,name))
(define (lam param body) `(lam ,param ,body))

(define (value? E)      (eq? (car E) 'value))
(define (value-value E) (match E (`(value ,val) val)))
(define (var-value? E)  (and (value? E) (var? (value-value E))))

(define (value v)      `(value ,v))
(define (op name e*)   `(op ,name . ,e*))
(define (== a b)       `(== ,a ,b))
(define (app a b)      `(app ,a ,b))
(define (seq a b)      `(seq ,a ,b))
(define (alt a b)      `(alt ,a ,b))
(define (one e)        `(one ,e))
(define (all rdone* e) `(all ,rdone* ,e))
(define (exist id* e)  `(exist ,id* ,e))

(define (nexist id* e) (if (null? id*) e `(exist ,id* ,e)))
(define (nseq   a b)   (if (value? a) b (seq a b)))

(define (rseq* rE*)
  (match rE*
    ('()          (value '()))
    ((cons E rE*) (foldl (lambda (e e.last) (nseq e e.last)) E rE*))))

(define (seq* E*) (rseq* (reverse E*)))

(define (ralt* rE*)
  (match rE*
    ('()          (== (value #t) (value #f)))
    ((cons E rE*) (foldl (lambda (e e.last) (alt e e.last)) E rE*))))

(define (alt* E*) (ralt* (reverse E*)))

(define (choice-free? E)
  (match E
    (`(==  ,a ,b)        (and (choice-free? a) (choice-free? b)))
    (`(seq ,a ,b)        (and (choice-free? a) (choice-free? b)))
    (`(op ,name . ,arg*) (and (not (eq? name 'vector-ref))
                            (andmap choice-free? arg*)))
    (`(app ,_ ,_)        #f)
    (`(alt ,_ ,_)        #f)
    (_                   #t)))

(define (start env uid E)
  (match E
    (`(value ,val)         (values uid E))
    (`(ref ,name)          (values uid (value (var (env-ref env name)))))
    ;; TODO: limit closure env to variables captured by body
    (`(lam ,param ,body)   (values uid (value (clo param body env))))
    (`(exist ,name* ,body) (let* ((uid.next (+ uid (length name*)))
                                  (uid*     (range uid uid.next))
                                  (env      (env-extend* env name* uid*)))
                             (let-values (((uid body) (start env uid.next body)))
                               (values uid (nexist uid* body)))))
    (`(op ,name . ,arg*)   (let loop ((uid uid) (arg* arg*) (rarg* '()))
                             (cond
                               ((null? arg*)
                                (let* ((arg*     (reverse rarg*))
                                       (uid.next (+ uid (length arg*)))
                                       (uid*     (range uid uid.next))
                                       (var*     (map value (map var uid*))))
                                  (values uid.next (nexist uid* (nseq (seq* (map == var* arg*))
                                                                      (op name var*))))))
                               (else (let-values (((uid arg) (start env uid (car arg*))))
                                       (loop uid (cdr arg*) (cons arg rarg*)))))))
    (`(app ,a ,b)          (let*-values (((uid a) (start env uid a))
                                         ((uid b) (start env uid b)))
                             (if (var-value? a)
                                 (if (var-value? b)
                                     (values uid (app a b))
                                     (let ((lhs.b (value (var uid))))
                                       (values (+ uid 1) (exist (list uid)
                                                           (seq (== lhs.b b) (app a lhs.b))))))
                                 (let ((lhs.a (value (var uid))))
                                   (if (var-value? b)
                                       (values (+ uid 1) (exist (list uid)
                                                           (seq (== lhs.a a) (app lhs.a b))))
                                       (let* ((uid.b (+ uid 1))
                                              (lhs.b (value (var uid.b))))
                                         (values (+ uid 2) (exist (list uid uid.b)
                                                             (seq (seq (== lhs.a a)
                                                                       (== lhs.b b))
                                                                  (app lhs.a lhs.b))))))))))
    (`(== ,a ,b)           (let*-values (((uid a) (start env uid a))
                                         ((uid b) (start env uid b)))
                             (cond
                               ((var-value? a) (values uid (== a b)))
                               ((var-value? b) (values uid (== b a)))
                               (else           (let ((lhs (value (var uid))))
                                                 (values (+ uid 1)
                                                         (exist (list uid)
                                                           (seq (== lhs a) (== lhs b)))))))))
    (`(seq ,a ,b)          (let*-values (((uid a) (start env uid a))
                                         ((uid b) (start env uid b)))
                             (values uid (nseq a b))))
    (`(alt ,a ,b)          (let*-values (((uid a) (start env uid a))
                                         ((uid b) (start env uid b)))
                             (values uid (alt a b))))
    (`(one ,e)             (let*-values (((uid e) (start env uid e)))
                             (values uid (one e))))
    (`(all ,e)             (let*-values (((uid e) (start env uid e)))
                             (values uid (all '() e))))))

(struct state (uid S E) #:prefab)

(define (initial-state env E)
  (let-values (((uid E) (start env 0 E)))
    (state uid S.empty E)))

;; TODO: garbage collect first, and use S-reify on this result to include unground variables.
(define (state-reify st) (reify (state-S st) (state-E st)))

(define (reify S E)
  (let loop ((E E))
    (match E
      (`(value ,v)         (value (walk* S v)))
      (`(exist ,id* ,body) (exist id* (loop body)))
      (`(op ,name . ,arg*) (op name (map loop arg*)))
      (`(app ,a ,b)        (app (loop a) (loop b)))
      (`(== ,a ,b)         (==  (loop a) (loop b)))
      (`(seq ,a ,b)        (seq (loop a) (loop b)))
      (`(alt ,a ,b)        (alt (loop a) (loop b)))
      (`(one ,e)           (one (loop e)))
      (`(all ,rd* ,e)      (all (map (lambda (v) (walk* S v)) rd*) (loop e))))))

(define (state-step st) (step #t (state-uid st) (state-S st) (state-E st)))

(define (step choice? uid S E)
  (match E
    (`(value ,_) (state uid S E))

    (`(op ,name . ,arg*)
      (let ((arg* (map value-value arg*)))
        ;; TODO: check arg count statically instead
        (define (argcount?! k) (unless (= (length arg*) k) (error "incorrect number of arguments" E)))
        (define (rewrite expr) (state uid S expr))
        (define (suspend)      (rewrite (op name (map (lambda (arg) (value (walk-var S arg))) arg*))))
        (define (return v)     (rewrite (value v)))
        (define (typepred ?)
          (argcount?! 1)
          (let ((x (walk S (car arg*))))
            (cond
              ((?    x) (return #t))
              ((var? x) (suspend))
              (else     #f))))
        (define (numpred2 ?)
          (argcount?! 2)
          (let ((a (walk S (car  arg*)))
                (b (walk S (cadr arg*))))
            (cond
              ((number? a)                     (cond
                                                 ((number? b)    (and (? a b) (return #t)))
                                                 ((not (var? b)) #f)
                                                 (else           (suspend))))
              ((not (var? a))                  #f)
              ((not (or (number? b) (var? b))) #f)
              (else                            (suspend)))))
        (cond
          ((eq? name 'cons) (argcount?! 2) (return (cons (walk-var S (car arg*))
                                                         (walk-var S (cadr arg*)))))
          (else (case name
                  ((number?)    (typepred number?))
                  ((symbol?)    (typepred symbol?))
                  ((string?)    (typepred string?))
                  ((vector?)    (typepred vector?))
                  ((procedure?) (typepred clo?))
                  ((vector-lengtho)
                   (argcount?! 2)
                   (let ((vec (walk S (car  arg*)))
                         (len (walk S (cadr arg*))))
                     (cond
                       ((vector? vec)        (rewrite (== (value len) (value (vector-length vec)))))
                       ((not (var? vec))     #f)
                       ((exact-integer? len) (let* ((uid.next (+ uid len))
                                                    (uid*     (range uid uid.next))
                                                    (var*     (map var uid*)))
                                               (state uid.next S
                                                      (exist uid* (== (value vec) (value (list->vector var*)))))))
                       ((not (var? len))     #f)
                       (else                 (suspend)))))
                  ((vector-refo)
                   (argcount?! 3)
                   (let ((vec  (walk     S (car   arg*)))
                         (idx  (walk     S (cadr  arg*)))
                         (elem (walk-var S (caddr arg*))))
                     (cond
                       ((vector? vec)
                        (cond
                          ((exact-integer? idx) (and (<= 0 idx)
                                                     (< idx (vector-length vec))
                                                     (rewrite (== (value elem) (value (vector-ref vec idx))))))
                          ((not (var? idx))     #f)
                          (else                 (rewrite (alt* (map (lambda (i x)
                                                                      (seq (== (value idx) (value i))
                                                                           (== (value elem) (value x))))
                                                                    (range (vector-length vec))
                                                                    (vector->list vec)))))))
                       ((not (var? vec))                           #f)
                       ((not (or (exact-integer? idx) (var? idx))) #f)
                       (else                                       (suspend)))))
                  ((+o)
                   (argcount?! 3)
                   (let ((a   (walk S (car   arg*)))
                         (b   (walk S (cadr  arg*)))
                         (out (walk S (caddr arg*))))
                     (cond
                       ((and (number? a) (number? b))       (rewrite (== (value out) (value (+ a b)))))
                       ((and (number? a) (number? out))     (rewrite (== (value b) (value (- out a)))))
                       ((and (number? b) (number? out))     (rewrite (== (value a) (value (- out b)))))
                       ((not (or (number? a)   (var? a)))   #f)
                       ((not (or (number? b)   (var? b)))   #f)
                       ((not (or (number? out) (var? out))) #f)
                       (else                                (suspend)))))
                  ((*o)
                   (argcount?! 3)
                   (let ((a   (walk S (car   arg*)))
                         (b   (walk S (cadr  arg*)))
                         (out (walk S (caddr arg*))))
                     (cond
                       ((and (number? a) (number? b))       (rewrite (== (value out) (value (* a b)))))
                       ((eqv? a 0)                          (rewrite (seq (op 'number? (list (value b)))
                                                                          (== (value out) (value 0)))))
                       ((eqv? b 0)                          (rewrite (seq (op 'number? (list (value a)))
                                                                          (== (value out) (value 0)))))
                       ((and (eqv? out 0) (number? a))      (rewrite (== (value b) (value 0))))
                       ((and (eqv? out 0) (number? b))      (rewrite (== (value a) (value 0))))
                       ((and (number? a) (number? out))     (rewrite (== (value b) (value (/ out a)))))
                       ((and (number? b) (number? out))     (rewrite (== (value a) (value (/ out b)))))
                       ((not (or (number? a)   (var? a)))   #f)
                       ((not (or (number? b)   (var? b)))   #f)
                       ((not (or (number? out) (var? out))) #f)
                       (else                                (suspend)))))
                  ((<)  (numpred2 <))
                  ((<=) (numpred2 <=))
                  (else (error "unknown operator" E)))))))

    (`(app ,f ,arg)        (match (walk S (value-value f))
                             ((clo param body env) (let* ((arg (walk-var S (value-value arg)))
                                                          (env (env-extend env param (var-id arg))))
                                                     (let-values (((uid body) (start env uid body)))
                                                       (state uid S body))))
                             ((? var? f)           (state uid S (app (value f) arg)))
                             (else                 #f)))

    (`(== ,a (seq ,b ,c))  (step choice? uid S (seq b (== a c))))

    (`(== ,a ,b)           (match (step choice? uid S b)
                             (#f #f)
                             ((state uid S b)
                              (if (value? b)
                                  (let ((S (unify S (value-value a) (value-value b))))
                                    (and S (state uid S a)))
                                  (let ((default (lambda () (state uid S (== a b)))))
                                    (if choice?
                                        (match b
                                          (`(alt ,b ,c) (state uid S (alt (== a b) (== a c))))
                                          (_ (default)))
                                        (default)))))))

    (`(seq (seq ,a ,b) ,c) (step choice? uid S (seq a (seq b c))))

    (`(seq ,a ,b)          (match (step choice? uid S a)
                             (#f              #f)
                             ((state uid S a) (match (step choice? uid S b)
                                                (#f #f)
                                                ((state uid S b)
                                                 (let* ((E       (nseq a b))
                                                        (default (lambda () (state uid S E))))
                                                   (if choice?
                                                       (match E
                                                         (`(seq (alt ,a ,b) ,c)
                                                           (state uid S (alt (seq a c) (seq b c))))
                                                         (`(seq ,a (alt ,b ,c))
                                                           (if (choice-free? a)
                                                               (state uid S (alt (seq a b) (seq a c)))
                                                               (default)))
                                                         (_ (default)))
                                                       (default))))))))

    (`(alt ,a ,b)          (state uid S E))

    (`(one ,E)             (let loop ((E E))
                             (match E
                               (`(alt (alt ,a ,b) ,c) (loop (alt a (alt b c))))
                               (`(alt ,a ,b)
                                 (aggregate-step
                                   uid S a
                                   (lambda ()      (state uid S (one b)))
                                   (lambda (uid a) (state uid S a))
                                   (lambda (uid a) (state uid S (one (alt a b))))))
                               (_ (aggregate-step
                                    uid S E
                                    (lambda ()      #f)
                                    (lambda (uid E) (state uid S E))
                                    (lambda (uid E) (state uid S (one E))))))))

    (`(all ,rdone* ,E)     (define (finish uid S rdone*)
                             (let* ((arg*     (reverse rdone*))
                                    (uid.next (+ uid (length arg*)))
                                    (uid*     (range uid uid.next))
                                    (var*     (map var uid*)))
                               (state uid.next S
                                      (nexist uid* (nseq (seq* (map == (map value var*) arg*))
                                                         (value (list->vector var*)))))))
                           (define (push rdone* E) (cons E rdone*))
                           (let loop ((E E))
                             (match E
                               (`(alt (alt ,a ,b) ,c) (loop (alt a (alt b c))))
                               (`(alt ,a ,b)
                                 (aggregate-step
                                   uid S a
                                   (lambda ()      (state uid S (all rdone*          b)))
                                   (lambda (uid a) (state uid S (all (push rdone* a) b)))
                                   (lambda (uid a) (state uid S (all rdone* (alt a b))))))
                               (_ (aggregate-step
                                    uid S E
                                    (lambda ()      (finish uid S rdone*))
                                    (lambda (uid E) (finish uid S (push rdone* E)))
                                    (lambda (uid E) (state uid S (all rdone* E))))))))

    (`(exist ,id* ,e)      (step choice? uid (S-exist S id*) e))))

(define (aggregate-step uid S E k.fail k.result k.incomplete)
  (let ((S (S-clear-local* S)))
    (match (step #t uid S E)
      (#f (k.fail))
      ((state uid S.inner E)
       (let ((S.diff (S-diff S.inner S)))
         (if (and (value? E) (let ((added* (S-domain S.diff))
                                   (exist* (S-local* S.diff)))
                               (set-empty? (set-subtract added* exist*))))
             (let* ((id*      (S-local* S.diff))
                    (uid.next (+ uid (set-count id*)))
                    (uid*     (range uid uid.next))
                    (id=>id   (make-immutable-hash (map cons (set->list id*) uid*)))
                    (id->id   (lambda (id) (hash-ref id=>id id id))))
               (k.result uid.next (S-reify (S-rename S.diff id->id)
                                           (value (value-rename (value-value E) id->id)))))
             (k.incomplete uid (S-reify S.diff E))))))))

;; NOTE: we may want to define a variant of "all" that only operates for
;; effect, and does not produce an aggregate result.  This will avoid space
;; leaks.  Alternatively, we can define "all" to incrementally build a list
;; result instead of a vector, allowing incremental garbage collection of
;; an unused result.  Yet another alternative would be to incrementally
;; build a stream, so that aggregation could be suspended and resumed based
;; on a consumer's demand.  The stream would be a sequence of pairs whose
;; cdrs are thunks that resume the rest of the aggregation.  These thunks
;; would be call-by-name.  If we also want memoization for call-by-need, the
;; memo operation can be derived through the use of "one" and
;; single-assignment control variales.
