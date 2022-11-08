;;;;;;;;;;;;;;;;;
;;; Utilities ;;;
;;;;;;;;;;;;;;;;;

(define (error x) (`(error: ,x)))

(define (atom? x) (or (null? x) (boolean? x) (number? x) (symbol? x)))

(define (not x) (if x #f #t))

(define (foldl f acc x*)
  (match x*
    ('()         acc)
    ((cons x x*) (foldl f (f x acc) x*))))

(define (foldr f acc x*)
  (match x*
    ('()         acc)
    ((cons x x*) (f x (foldr f acc x*)))))

(define (andmap ? x*)
  (match x*
    ('()         #t)
    ((list x)    (? x))
    ((cons x x*) (and (? x) (andmap ? x*)))))

(define (ormap ? x*)
  (match x*
    ('()         #f)
    ((list x)    (? x))
    ((cons x x*) (or (? x) (ormap ? x*)))))

(define (append  x* y) (foldr cons y x*))
(define (reverse x*)   (foldl cons '() x*))

(define (map f x*)
  (let loop ((x* x*))
    (match x*
      ('()         '())
      ((cons x x*) (cons (f x) (loop x*))))))

(define (filter ? x*)
  (let loop ((x* x*))
    (match x*
      ('()         '())
      ((cons x x*) (if (? x)
                       (cons x (loop x*))
                       (loop x*))))))

(define (remv1 x x*)
  (let loop ((x* x*))
    (match x*
      ('()         '())
      ((cons y y*) (if (atom=? x y)
                       y*
                       (cons y (loop y*)))))))

(define (assoc/? ? kv*)
  (let loop ((kv* kv*))
    (match kv*
      ('()                            #f)
      ((cons (and (cons k v) kv) kv*) (if (? k) kv (loop kv*))))))

;;;;;;;;;;;;;;;;;;;
;;; microKanren ;;;
;;;;;;;;;;;;;;;;;;;

(define (var   id)  (vector id))
(define (var?  x)   (vector? x))
(define (var=? a b) (atom=? (vector-ref a 0) (vector-ref b 0)))

(define sub.empty   '())

(define (walk t sub)
  (let ((xt (and (var? t) (assoc/? (lambda (x) (var=? t x)) sub))))
    (if xt (walk (cdr xt) sub) t)))

(define (occurs? x t sub)
  (let loop ((t t))
    (cond ((pair? t) (or (loop (walk (car t) sub))
                         (loop (walk (cdr t) sub))))
          ((var?  t) (var=? x t))
          (else      #f))))

(define (extend-sub x t sub)
  (and (not (occurs? x t sub)) `((,x . ,t) . ,sub)))

(define (make-state sub id.next)   (cons sub id.next))
(define (state-sub         st)     (car st))
(define (state-next-id     st)     (cdr st))
(define (set-state-next-id st id)  (make-state (state-sub st) id))
(define (set-state-sub     st sub) (make-state sub (state-next-id st)))

(define state.empty '(() 0))

(define (call/fresh var->k)
  (lambda (st)
    (let ((id (state-next-id st)))
      ((var->k (var id)) (set-state-next-id st (+ id 1))))))

(define (assign x t st)
  (let ((sub.new (extend-sub x t (state-sub st))))
    (and sub.new (set-state-sub st sub.new))))

(define (unify u v st)
  (let* ((sub (state-sub st))
         (u   (walk u sub))
         (v   (walk v sub)))
    ;; TODO: it might also be useful to allow any procedure values to unify.
    (cond ((and (var? u) (var? v) (var=? u v)) st)
          ((pair? u) (and (pair? v) (let ((st (unify (car u) (car v) st)))
                                      (and st (unify (cdr u) (cdr v) st)))))
          (else      (and (atom? u) (atom? v) (atom=? u v) st)))))

(define (state->stream st) (and st (cons st #f)))

(define (mature? s) (or (not s) (pair? s)))
(define (mature  s) (if (mature? s) s (mature (s))))
(define (step    s) (if (mature? s) s (s)))

(define (s-take n s)
  (cond ((atom=? 0 n) '())
        (else (let ((s (mature s)))
                (cond ((pair? s) (cons (car s) (s-take (and n (+ n -1))
                                                       (cdr s))))
                      (else      '()))))))

(define (pause st g) (lambda () (g st)))

(define (mplus s1 s2)
  (let ((s1 (step s1)))
    (cond ((not   s1) s2)
          ((pair? s1) (cons (car s1) (lambda () (mplus s2 (cdr s1)))))
          (else       (lambda () (mplus s2 s1))))))

(define (bind s g)
  (let ((s (step s)))
    (cond ((not   s) #f)
          ((pair? s) (mplus (pause (car s) g)
                            (lambda () (bind (cdr s) g))))
          (else      (lambda () (bind s g))))))

(define (disj   g1 g2)   (lambda (st) (mplus (pause st g1) (pause st g2))))
(define (conj   g1 g2)   (lambda (st) (bind (pause st g1) g2)))
(define (relate thunk _) (lambda (st) (pause st (thunk))))
(define (==     u v)     (lambda (st) (state->stream (unify u v st))))

;; TODO: desugaring of miniKanren into microKanren
