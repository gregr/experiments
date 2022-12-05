(define (rename x count)
  (string->symbol (string-append (symbol->string x) "." (number->string count))))

;; TODO: replace naive alpha-renaming with GHC-style Rapier to reduce name explosion
(define (normalize term)
  (let loop ((term term) (S '()) (count 0))
    (match term
      ((? symbol?)
       (let ((kv (assoc term S)))
         (if kv (cdr kv) term)))
      (`(lambda (,x) ,body)
        (let ((x.new (rename x count)))
          `(lambda (,x.new) ,(loop body (cons (cons x x.new) S) (+ count 1)))))
      (`(,f ,arg)
        (let ((arg (loop arg S count)))
          (match (loop f S count)
            (`(lambda (,x) ,body) (loop body (cons (cons x arg) S) count))
            (f                    `(,f ,arg))))))))

(define zero   '(lambda (f) (lambda (Z) Z)))
;(define add1   '(lambda (n) (lambda (f) (lambda (Z) ((n f) (f Z))))))
;; variant of add1
(define add1   '(lambda (n) (lambda (f) (lambda (Z) (f ((n f) Z))))))

(define true   '(lambda (t) (lambda (f) t)))
(define false  '(lambda (t) (lambda (f) f)))
(define not    `(lambda (b) ((b ,false) ,true)))

;(define double '(lambda (n) (lambda (f) (lambda (Z) ((n f) ((n f) Z))))))
;; variant of double
(define double '(lambda (n) (lambda (f) (lambda (Z) ((n (lambda (Z) (f (f Z)))) Z)))))

(define even?  `(lambda (n) ((n ,not) ,true)))

;; Theorem we want to prove for natural numbers n
(define double-even?        `(lambda (n) (,even? (,double n))))

;; Induction cases
(define double-even?.zero  `((lambda (n) (,even? (,double n))) ,zero))

(define double-even?.k+1 `(lambda (k)
                            ((lambda (n) (,even? (,double n))) (,add1 k))))

(define double-even?.normalized-without-substituting
  `(lambda (n) ((n ,not) ((n ,not) ,true))))

(define double-even?.normalized
  '(lambda (n)
     ((n (lambda (b)
           ((b (lambda (t) (lambda (f) f)))
            (lambda (t) (lambda (f) t)))))
      ((n (lambda (b)
            ((b (lambda (t) (lambda (f) f)))
             (lambda (t) (lambda (f) t)))))
       (lambda (t) (lambda (f) t))))))

(define double-even?.zero.normalized
  '(lambda (t) (lambda (f) t))) ; == true

(define double-even?.k+1.normalized
  '(lambda (k)
     ((k (lambda (b)
           ((b (lambda (t) (lambda (f) f)))
            (lambda (t) (lambda (f) t)))))
      ((((k (lambda (b)
              ((b (lambda (t) (lambda (f) f)))
               (lambda (t) (lambda (f) t)))))
         (lambda (t) (lambda (f) f)))
        (lambda (t) (lambda (f) f)))
       (lambda (t) (lambda (f) t))))))

(define (alpha=? u v)
  (let loop ((u u) (v v) (S.u '()) (S.v '()) (count 0))
    (match* (u v)
      ((`(lambda (,x) ,u) `(lambda (,y) ,v))
       (loop u v (cons (cons x count) S.u) (cons (cons y count) S.v) (+ count 1)))
      ((`(lambda . ,_) _) #f)
      ((_ `(lambda . ,_)) #f)
      ((`(,u ,arg.u) `(,v ,arg.v))
       (and (loop u     v     S.u S.v count)
            (loop arg.u arg.v S.u S.v count)))
      (((? symbol?) (? symbol?))
       (let ((ku (assoc u S.u))
             (kv (assoc v S.v)))
         (if ku
             (and kv (eqv? (cdr ku) (cdr kv)))
             (and (eq? kv #f) (eq? ku kv)))))
      ((_ _) #f))))

(define (assoc/alpha=? k alist)
  (let loop ((alist alist))
    (and (eq? (null? alist) #f)
         (if (alpha=? k (caar alist))
             (car alist)
             (loop (cdr alist))))))

(define (substitute term S)
  ;; Assume all right-hand-sides of S are closed terms.
  ;; Otherwise we need to alpha-rename under lambda.
  (let ((kv (assoc/alpha=? term S)))
    (if kv
        (cdr kv)
        (match term
          (`(lambda (,x) ,body) `(lambda (,x) ,(substitute body S)))
          (`(,f ,arg)           `(,(substitute f S) ,(substitute arg S)))
          (_                    term)))))

(define (==true? term) (alpha=? true term))

(define (nat-theorem? nat-p?)
  (andmap ==true? (list
                    ;; (P 0)
                    (normalize `(,nat-p? ,zero))
                    ;; forall k:nat. (P k) -> (P (+ k 1))
                    (normalize (substitute (normalize `(,nat-p? (,add1 k)))
                                           (list (cons (normalize `(,nat-p? k)) true)))))))

(nat-theorem? double-even?) ; ==> #t


;;; Looking at the theorem components in more detail:

(define de.k+1
  ;; (normalize `(,double-even? (,add1 k)))
  ;; ==>
  '((((;; Notice that this part is alpha=? to (noramlize `(,double-even? k)).
       ;; We can replace it with true since it is our inductive hypothesis.
       ((k
          (lambda (Z.4.3.2.1.0.0)
            ((((Z.4.3.2.1.0.0
                 (lambda (t.3.2.3.2.1.1) (lambda (f.4.3.4.3.2.2) f.4.3.4.3.2.2)))
               (lambda (t.3.2.3.2.1.1) (lambda (f.4.3.4.3.2.2) t.3.2.3.2.1.1)))
              (lambda (t.3.2.3.2.1.1) (lambda (f.4.3.4.3.2.2) f.4.3.4.3.2.2)))
             (lambda (t.3.2.3.2.1.1) (lambda (f.4.3.4.3.2.2) t.3.2.3.2.1.1)))))
        (lambda (t.2.1.0) (lambda (f.3.2.1) t.2.1.0)))

       (lambda (t.3.2.3.2.1.1.0) (lambda (f.4.3.4.3.2.2.1) f.4.3.4.3.2.2.1)))
      (lambda (t.3.2.3.2.1.1.0) (lambda (f.4.3.4.3.2.2.1) t.3.2.3.2.1.1.0)))
     (lambda (t.3.2.3.2.1.1.0) (lambda (f.4.3.4.3.2.2.1) f.4.3.4.3.2.2.1)))
    (lambda (t.3.2.3.2.1.1.0) (lambda (f.4.3.4.3.2.2.1) t.3.2.3.2.1.1.0))))

(define de.k
  ;; (normalize `(,double-even? k))
  ;; ==>
  '((k
      (lambda (Z.4.3.2.1.0)
        ((((Z.4.3.2.1.0
             (lambda (t.3.2.3.2.1) (lambda (f.4.3.4.3.2) f.4.3.4.3.2)))
           (lambda (t.3.2.3.2.1) (lambda (f.4.3.4.3.2) t.3.2.3.2.1)))
          (lambda (t.3.2.3.2.1) (lambda (f.4.3.4.3.2) f.4.3.4.3.2)))
         (lambda (t.3.2.3.2.1) (lambda (f.4.3.4.3.2) t.3.2.3.2.1)))))
    (lambda (t.2.1.0) (lambda (f.3.2.1) t.2.1.0))))
