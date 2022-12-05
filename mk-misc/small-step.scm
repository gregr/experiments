(define (lookupo env x v)
  (symbolo x)
  (conde
    ((== env '()) (== v `(ref ,x)))
    ((fresh (y a rest)
       (== env `((,y . ,a) . ,rest))
       (conde
         ((== x y) (== v a))
         ((=/= x y) (lookupo rest x v)))))))

(define (kstepo k u v)
  (conde
    ((== k '()) (== u v))
    ((fresh (x j w)
       (== k (cons x j))
       (stepo u w)
       (kstepo j w v)))))

(define (kdiffstepo k u v)
  (conde
    ((== k '()) (== u v))
    ((fresh (x j w)
       (== k (cons x j))
       (=/= u w)
       (stepo u w)
       (kdiffstepo j w v)))))

(define (step+o u w)
  (fresh (v)
    (stepo u v)
    (step*o v w)))

(define (step*o u v)
  (conde
    ((== u v))
    ((step+o u v))))


;;;;;;;;;;;;;;;;;;;;;
;;; call-by-value ;;;
;;;;;;;;;;;;;;;;;;;;;

(define (stucko e)
  (fresh (tag rest)
    (== e `(,tag . ,rest))
    (=/= tag 'app)))

(define (stepo u v)
  (fresh (f a)
    (== u `(app ,f ,a))
    (step-appo f a v)))

(define (step-appo f a v)
  (conde
    ((fresh (x body)
       (== f `(lambda (,x) ,body))
       (stucko a)  ; call-by-value
       (appo x a body v)))

    ((fresh (b)
       (== `(app ,f ,b) v)
       (stucko f)
       (stepo a b)))

    ((fresh (g)
       (== v `(app ,g ,a))
       (stepo f g)))))

(define (appo x a body v)
  (conde
    ((fresh (env rest)
       (== body `(closure-environment ,env ,rest))
       (substituteo (cons (cons x a) env) rest v)))
    ((fresh (tag rest)
       (== body `(,tag . ,rest))
       (=/= tag 'closure-environment)
       (substituteo (list (cons x a)) body v)))))

(define (substituteo env e v)
  (conde
    ((fresh (name)
       (== e `(ref ,name))
       (lookupo env name v)))

    ;; Unrolled lookupo for improved search priority?
    ;((fresh (name rest)
       ;(== e `(ref ,name))
       ;(== env `((,name . ,v) . ,rest))
       ;(symbolo name)))
    ;((fresh (name)
       ;(== e `(ref ,name))
       ;(== env '())
       ;(== e v)))
    ;((fresh (name y a rest)
       ;(== e `(ref ,name))
       ;(== env `((,y . ,a) . ,rest))
       ;(=/= name y)
       ;(lookupo rest name v)))

    ((fresh (x body)
       (== e `(lambda ,x ,body))
       (== v `(lambda ,x (closure-environment ,env ,body)))))

    ((fresh (f a g b)
       (== e `(app ,f ,a))
       (== v `(app ,g ,b))
       ;; Option: only allow normal-form lambda bodies
       (fresh (tag body)
         (== f `(,tag . ,body))
         (=/= tag 'lambda))
       (substituteo env f g)
       (substituteo env a b)))))


;;;;;;;;;;;;;;;;;;;;
;;; call-by-name ;;;
;;;;;;;;;;;;;;;;;;;;

;(define (stucko e)
;  (fresh (name)
;    (== e `(ref ,name))))
;
;(define (stepo u v)
;  (conde
;    ((fresh (f a)
;       (== u `(app ,f ,a))
;       (step-appo f a v)))
;    ((fresh (env e)
;       (== u `(closure-environment ,env ,e))
;       (step-substituteo env e v)))))
;
;(define (step-substituteo env e v)
;  (conde
;    ((fresh (name)
;       (== e `(ref ,name))
;       (lookupo env name v)))
;
;    ((fresh (x body)
;       (== e `(lambda ,x ,body))
;       (== v `(lambda ,x (closure-environment ,env ,body)))))
;
;    ((fresh (f a)
;       (== e `(app ,f ,a))
;       (== v `(app (closure-environment ,env ,f) (closure-environment ,env ,a)))))))
;
;(define (step-appo f a v)
;  (conde
;    ((fresh (x body)
;       (== f `(lambda (,x) ,body))
;       (appo x a body v)))
;
;    ((fresh (g)
;       (== v `(app ,g ,a))
;       (stepo f g)))
;
;    ((fresh (b)
;       (== `(app ,f ,b) v)
;       (stucko f)
;       (stepo a b)))))
;
;(define (appo x a body v)
;  (conde
;    ((fresh (env rest)
;       (== body `(closure-environment ,env                   ,rest))
;       (== v    `(closure-environment ,(cons (cons x a) env) ,rest))))
;    ((fresh (tag rest)
;       (== body `(,tag . ,rest))
;       (=/= tag 'closure-environment)
;       (== v    `(closure-environment ,(list (cons x a)) ,body))))))


;;;;;;;;;;;;;;;;
;;; Examples ;;;
;;;;;;;;;;;;;;;;

;(run 2 (a b) (stepo a b))

;(run 3 (a b)
;  (absento 'closure-environment a)
;  (stepo a b))

(time (run 1 (e) (stepo e e)))
;(time (run 1 ...))
;    no collections
;    0.000184000s elapsed cpu time
;    0.000181000s elapsed real time
;    114560 bytes allocated
;(((app (lambda (_.0) (app (ref _.0) (ref _.0)))
;       (lambda (_.0) (app (ref _.0) (ref _.0))))))

;(time (run 1 (e) (step+o e e)))

;(time (run 2 (e) (stepo e e)))
;(time (run 2 (e) (step+o e e)))

;(time (run 20 (e) (stepo e e)))
;(time (run 20 (e) (step+o e e)))

;(time (run 1 (e)
;        (let ((t '(app (lambda (_.1) (app (ref _.1) (ref _.1)))
;                       (lambda (_.1) (app (ref _.1) (ref _.1))))))
;          (step+o t t))))

;(time (run 1 (Y) (step*o            `(app ,Y (ref F)) `(app (ref F) (app ,Y (ref F))))))

(time (run 1 (Y) (kstepo     '(1 1) `(app ,Y (ref F)) `(app (ref F) (app ,Y (ref F))))))
;(time (run 1 ...))
;    64300 collections
;    1004.438716000s elapsed cpu time, including 167.569024000s collecting
;    1013.246964000s elapsed real time, including 169.502996000s collecting
;    541760207664 bytes allocated, including 541672082416 bytes reclaimed
;(((app (lambda (_.0)
;         (lambda (_.1)
;           (app (ref _.1) (app (app (ref _.0) (ref _.0)) (ref _.1)))))
;       (lambda (_.0)
;         (lambda (_.1)
;           (app (ref _.1) (app (app (ref _.0) (ref _.0)) (ref _.1))))))
;   (=/= ((_.0 _.1)))))

;(time (run 1 (Y) (kdiffstepo '(1 1) `(app ,Y (ref F)) `(app (ref F) (app ,Y (ref F))))))

;(let* ((template '(lambda (x)
;                    (lambda (f)
;                      (app (ref f) (app (app (ref x) (ref x)) (ref f))))))
;       (Y        `(app ,template ,template)))
;  (time (run 1 (k q) (kstepo k `(app ,Y (ref F)) `(app (ref F) (app ,Y (ref F)))))))
;
;(time (run 1 (pre-Y Y)
;        (stepo pre-Y Y)
;        (kstepo '(1 1) `(app ,Y (ref F)) `(app (ref F) (app ,Y (ref F))))))
;
;(let ((pre-Y '(app (lambda (d) (app (ref d) (ref d)))
;                   (lambda (x)
;                     (lambda (f)
;                       (app (ref f) (app (app (ref x) (ref x)) (ref f))))))))
;  (time (run 1 (k q)
;          (fresh (Y)
;            (stepo pre-Y Y)
;            (kstepo k `(app ,Y (ref F)) `(app (ref F) (app ,Y (ref F))))))))

