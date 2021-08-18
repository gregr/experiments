#lang racket/base
(provide (all-defined-out))
(require racket/list racket/match racket/set racket/struct)

;; To switch search strategies, swap the strategy section [un]commentings.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables and their attributes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define uid (let ((next -1))
              (lambda ()
                (set! next (+ next 1))
                next)))

(struct var   (name id)
        #:methods gen:custom-write
        ((define write-proc
           (make-constructor-style-printer
             (lambda (v) 'var)
             (lambda (v) (list (var-name v) (var-id v)))))))
(struct vattr (fm-ids) #:prefab)

(define var.initial (var 'var.initial #f))
(define vattr.empty (vattr (set)))

(define (vattr-fm-ids-add va id*) (vattr (set-union (vattr-fm-ids va) id*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct state (log v=>vattr id=>fm pending-fm-ids) #:prefab)

(define state.empty (state #f (hash) (hash) (set)))

(define (state-log-start     s)       (state '()                    (state-v=>vattr s) (state-id=>fm s) (state-pending-fm-ids s)))
(define (state-log-add       s f)   (if (state-log s)
                                      (state (cons f (state-log s)) (state-v=>vattr s) (state-id=>fm s) (state-pending-fm-ids s))
                                      s))
(define (state-vattr-set     s v t) (let ((s (if (vattr? t)
                                               s
                                               (state-log-add s `(== ,v ,t)))))
                                      (state (state-log s) (hash-set (state-v=>vattr s) v t)           (state-id=>fm s)                  (state-pending-fm-ids s))))
(define (state-id=>fm-add    s id f)  (state (state-log s)           (state-v=>vattr s)      (hash-set (state-id=>fm s) id f)            (state-pending-fm-ids s)))
(define (state-id=>fm-remove s id*) (let ((id=>fm (foldl (lambda (id id=>fm) (hash-remove id=>fm id))
                                                         (state-id=>fm s)
                                                         id*)))
                                      (state (state-log s)           (state-v=>vattr s)                id=>fm                            (state-pending-fm-ids s))))
(define (state-pending-add   s id*)   (state (state-log s)           (state-v=>vattr s)                (state-id=>fm s)       (set-union (state-pending-fm-ids s) id*)))
(define (state-pending-clear s)       (state (state-log s)           (state-v=>vattr s)                (state-id=>fm s)       (set)))

(define (state-vattr   s v)  (hash-ref (state-v=>vattr s) v  vattr.empty))
(define (state-formula s id) (hash-ref (state-id=>fm   s) id (conj)))

(define (walk s t)
  (if (var? t)
    (let ((v=>a (state-v=>vattr s)))
      (let loop ((v t))
        (let ((t (hash-ref v=>a v vattr.empty)))
          (cond ((vattr? t)     v)
                ((not (var? t)) t)
                (else           (loop t))))))
    t))

(define (walk* s t)
  (let ((t (walk s t)))
    (if (pair? t)
      (cons (walk* s (car t))
            (walk* s (cdr t)))
      t)))

(define (occurs? s v t)
  (let ((t (walk s t)))
    (or (eqv? v t)
        (and (pair? t)
             (or (occurs? s v (car t))
                 (occurs? s v (cdr t)))))))

(define (assign s v t)
  (if (var? t)
    (state-vattr-set (state-vattr-set s v t) t
                     (vattr-fm-ids-add (state-vattr s t)
                                       (vattr-fm-ids (state-vattr s v))))
    (and (not (occurs? s v t))
         (state-pending-add (state-vattr-set s v t)
                            (vattr-fm-ids (state-vattr s v))))))

(define (unify s t1 t2)
  (let ((t1 (walk s t1))
        (t2 (walk s t2)))
    (cond ((eqv? t1 t2) s)
          ((var? t1)    (assign s t1 t2))
          ((var? t2)    (assign s t2 t1))
          ((pair? t1)   (and (pair? t2)
                             (let ((s (unify s (car t1) (car t2))))
                               (and s (unify s (cdr t1) (cdr t2))))))
          (else         (and (equal? t1 t2) s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reify s t)
  (let ((v=>id (hash)))
    (let loop ((t (walk* s t)))
      (cond
        ((var?  t) `#s(var ,(or (hash-ref v=>id t #f)
                                (let ((id (hash-count v=>id)))
                                  (set! v=>id (hash-set v=>id t id))
                                  id))))
        ((pair? t) (cons (loop (car t)) (loop (cdr t))))
        (else      t)))))

(define (reify/initial s) (reify s var.initial))

(define (pretty-state s)
  (cons (filter-not not (hash-map (state-v=>vattr s)
                                  (lambda (v t)
                                    (and (not (vattr? t))
                                         `(== ,v ,(walk* s t))))))
        (hash->list (state-id=>fm s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Streams
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (s-map f ss)
  (cond ((procedure? ss) (lambda () (s-map f (ss))))
        ((null? ss)      '())
        (else            (cons (f (car ss)) (s-map f (cdr ss))))))

(define (s-force ss) (if (procedure? ss) (s-force (ss)) ss))

(define (s-take n ss)
  (if (and n (<= n 0))
    '()
    (let ((ss (s-force ss)))
      (if (null? ss)
        '()
        (cons (car ss) (s-take (and n (- n 1)) (cdr ss)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interleaving search operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fail   s) '())
(define (return s) (if s (list s) '()))

(define (bind ss g)
  (cond ((null? ss) '())
        ((pair? ss) (mplus (lambda () (g (car ss)))
                           (lambda () (bind (cdr ss) g))))
        (else       (lambda () (bind (ss) g)))))

(define (mplus ss1 ss2)
  (let ((ss1 (if (procedure? ss1) (ss1) ss1)))
    (cond ((null? ss1) ss2)
          ((pair? ss1) (cons (car ss1) (lambda () (mplus ss2 (cdr ss1)))))
          (else                        (lambda () (mplus ss2      ss1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; miniKanren Syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (== u v)    `(== ,u ,v))
(define (conj . fs) `(conj . ,fs))
(define (disj . fs) `(disj . ,fs))

(define-syntax-rule (fresh (x ...) f.0 f ...)
  (let ((x (var 'x (uid))) ...)
    (conj f.0 f ...)))

(define-syntax-rule (conde (f.0 fs.0 ...) (f fs ...) ...)
  (disj (conj f.0 fs.0 ...)
        (conj f   fs   ...) ...))

(define-syntax-rule (query (q ...) f ...)
  (fresh (q ...) (== var.initial (list q ...)) f ...))

(define-syntax-rule (define-relation (name param ...) f ...)
  (define (name param ...)
    `(relate ,(lambda () (fresh () f ...)) ,name name ,param ...)))

(define-syntax-rule (run^  body ...) (s-map reify/initial (start state.empty (query body ...))))
(define-syntax-rule (run n body ...) (s-take n (run^   body ...)))
(define-syntax-rule (run*  body ...)           (run #f body ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typical miniKanren search strategy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(define (formula->goal f)
  (match f
    (`(conj)             return)
    (`(conj ,f.0 . ,fs)  (foldl (lambda (f g.0)
                                  (let ((g (formula->goal f)))
                                    (lambda (s) (bind (g.0 s) g))))
                                (formula->goal f.0)
                                fs))
    (`(disj)             fail)
    (`(disj . ,fs)       (let ((fs (reverse fs)))
                           (foldl (lambda (f g.1)
                                    (let ((g.0 (formula->goal f)))
                                      (lambda (s) (mplus (g.0 s)
                                                         (lambda () (g.1 s))))))
                                  (formula->goal (car fs))
                                  (cdr fs))))
    (`(relate ,thk ,_ ,r . ,_) (lambda (s) (lambda () ((formula->goal (thk)) s))))
    (`(==     ,u ,v)     (lambda (s) (return (unify s u v))))))

(define (start s f) ((formula->goal f) state.empty))
;|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dKanren-like search strategy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;#|
(define (foldl/and f acc xs . yss)
  (let loop ((acc acc) (xs xs) (yss yss))
    (if (null? xs)
      acc
      (and acc (loop (apply f (car xs) (append (map car yss) (list acc)))
                     (cdr xs)
                     (map cdr yss))))))

(define (apply-formula depth s f)
  (match f
    (`(conj . ,fs)              (foldl/and (lambda (f s) (apply-formula depth s f))
                                           s
                                           fs))
    (`(disj . ,fs)              (let loop ((fs fs) (f.0 #f))
                                  (match fs
                                    ('()                     (and f.0 (apply-formula depth s f.0)))
                                    ((cons `(disj . ,ds) fs) (loop (append ds fs) f.0))
                                    ((cons f             fs)
                                     (let ((s.new (apply-formula 0 (state-log-start s) f)))
                                       (cond ((not s.new)               (loop fs f.0))
                                             ((null? (state-log s.new)) s) ; unconstrained success
                                             (else (let ((f.new `(conj . ,(reverse (state-log s.new)))))
                                                     (cond ((not f.0)   (loop fs f.new))
                                                           ((= depth 0) (state-log-add s `(disj ,f.0 ,f.new . ,fs)))
                                                           (else (let ((id (uid)) (f `(disj ,f.0 ,f.new . ,fs)))
                                                                   (foldl (lambda (v s)
                                                                            (state-vattr-set
                                                                              s v (vattr-fm-ids-add (state-vattr s v) (set id))))
                                                                          (state-id=>fm-add s id f)
                                                                          (set->list (formula-dependencies f))))))))))))))
    (`(relate ,thk . ,metadata) (if (= depth 0)
                                  (state-log-add s f)
                                  (let ((id (uid)) (f (thk)))
                                    (state-pending-add (state-id=>fm-add s id f) (set id)))))
    (`(==     ,u ,v)            (unify s u v))))

(define (formula-dependencies f)
  (let loop ((f f) (vs (set)))
    (match f
      (`(conj . ,fs)          (foldl loop vs fs))
      (`(disj ,f.0 ,f.1 . ,_) (foldl loop vs (list f.0 f.1)))
      (`(relate . ,_)         vs)
      ;; Assume constraints of the form: (== var term)
      (`(==     ,u ,_)        (set-add vs u)))))

(define (start s f)
  (let ((s (apply-formula 1 s f)))
    (if s
      (lambda () (step s))
      '())))

(define (step s)
  (let* ((ids    (set->list (state-pending-fm-ids s)))
         (id=>fm (state-id=>fm s))
         (fs     (map (lambda (id) (hash-ref id=>fm id '(conj))) ids))
         (s      (state-id=>fm-remove s ids))
         (s      (state-pending-clear s)))
    (cond ((not (null? fs))     (start s `(conj . ,fs)))
          ((hash-empty? id=>fm) (list s))
          (else                 (guess s)))))

(define (guess s)
  (let* ((id=>fm (state-id=>fm s))
         (idfs   (hash->list id=>fm)))
    (let loop ((idfs (cdr idfs)) (id (caar idfs)) (f (cdar idfs)))
      (match idfs
        ((cons (cons id.next f.next) idfs) (if (< id.next id)
                                             (loop idfs id.next f.next)
                                             (loop idfs id      f)))
        ('() (match-let ((s             (state-id=>fm-remove s (list id)))
                         (`(disj . ,fs) f))
               (let loop ((fs fs))
                 (match fs
                   ((list f)    (start s f))
                   ((cons f fs) (mplus (start s f) (lambda () (loop fs))))))))))))

;; A failed guessing attempt
;(define (formula-weight s f)
;  (match f
;    (`(conj . ,fs)              (foldl * 1 (map (lambda (f) (formula-weight s f)) fs)))
;    (`(disj ,f.0 ,f.1 . ,fs)    (- 1 (* (- 1 (formula-weight s f.0))
;                                        (- 1 (formula-weight s f.1))
;                                        (expt 1/2 (length fs)))))
;    (`(relate ,_ ,_ ,_ . ,args) (/ 1 (+ 1 (term-size s args))))
;    (`(== ,u ,v)                (/ 1 (+ (term-size s u) (term-size s v))))))
;
;(define (term-size s t)
;  (let ((t (walk s t)))
;    (cond ((var? t)  1)
;          ((pair? t) (+ (term-size s (car t))
;                        (term-size s (cdr t))))
;          (else      2))))
;
;(define (guess s)
;  (let* ((id=>fm (state-id=>fm s))
;         (idfs   (hash->list id=>fm)))
;    (let loop ((idfs (cdr idfs)) (id (caar idfs)) (f (cdar idfs)) (weight (formula-weight s (cdar idfs))))
;      (match idfs
;        ((cons (cons id.next f.next) idfs) (let ((w (formula-weight s f.next)))
;                                             (if (< w weight)
;                                               (loop idfs id.next f.next w)
;                                               (loop idfs id      f      weight))))
;        ('() (match-let ((s             (state-id=>fm-remove s (list id)))
;                         (`(disj . ,fs) f))
;               (let loop ((fs fs))
;                 (match fs
;                   ((list f)    (start s f))
;                   ((cons f fs) (mplus (start s f) (lambda () (loop fs))))))))))))

;|#
