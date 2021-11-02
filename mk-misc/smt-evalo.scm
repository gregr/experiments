(declare-datatypes () ((Symbol quote var lambda pair app)))
(declare-datatypes () ((S null (qint (unqint Int)) (qsym (unqsym Symbol)) (cons (car S) (cdr S)))))

(declare-fun x () S)

;(declare-fun lookupo   (S S S) Bool)
;(declare-fun eval-expo (S S S) Bool)

;(define-fun eval-expo ((exp S) (env S) (val S)) Bool
  ;(ite (and (= exp (cons (qsym quote) (cons (qsym lambda) null)))
            ;(= val (qsym lambda)))
       ;true
       ;(= exp (cons (qsym quote) (cons val null)))))

(define-fun eval-expo ((exp S) (env S) (val S)) Bool
  (or (and (= (car exp) (qsym quote))
           (= val       (car (cdr exp))))
      (and (= (car exp) (qsym var))
           (let ((name (car (cdr exp))))
             (lookupo name env val)))))

(define-fun lookupo ((x S) (env S) (t S)) Bool
  (let ((bp (car env)))
    (or (and (= x (car bp))       (= t (cdr bp)))
        (and (not (= x (car bp))) (lookupo x (cdr env) t)))))

;(assert
  ;(forall ((exp S) (env S) (val S))
    ;(iff (or (exists ((v S))
               ;(and (= exp (cons (qsym quote) (cons v null)))
                    ;(= val v))))
         ;(eval-expo exp env val))))

(assert
  (forall ((exp S) (env S) (val S))
    (iff (or (= exp (cons (qsym quote) (cons val null)))
             (exists ((name S))
               (and (= exp (cons (qsym var) (cons name null)))
                    (lookupo name env val))))
         (eval-expo exp env val))))

(assert
  (forall ((x S) (env S) (t S))
    (iff (exists ((rest S) (y S) (v S))
           (and (= env (cons (cons y v) rest))
                (or (and (= y x) (= v t))
                    (and (not (= y x)) (lookupo x rest t)))))
         (lookupo x env t))))


(assert (eval-expo (cons (qsym quote)
                         (cons (qsym lambda)
                               null))
                   null
                   (qsym x)))

(check-sat)
(get-model)

;(declare-fun x () Int)
;(declare-fun edge (Int Int) Bool)
;(declare-fun path (Int Int) Bool)

;(declare-datatypes ()
  ;((Symbol quote lambda cons)))

;(declare-datatypes ()
  ;((S null
      ;(qint (unqint Int))
      ;(qsym (unqsym Symbol))
      ;(cons (car S) (cdr S)))))

;(declare-fun x () Symbol)

;(assert (not (or (= x quote))))

;;(assert (not (or (= x null) (exists ((n Int)) (= x (qint n))))))

;;(declare-datatype S (par () ((null))))

;;(quote (unq Int))
;;(cons (car S) (cdr S))))

;;; edge
;;(assert
;;  (forall ((a Int) (b Int))
;;    (iff
;;      (or
;;        (and (= a 1) (= b 2))
;;        (and (= a 2) (= b 3))
;;        (and (= a 2) (= b 4)))
;;      (edge a b))))

;;(assert
;;  (forall ((a Int) (b Int))
;;    (iff
;;      (or
;;        (edge a b)
;;        (exists ((mid Int))
;;          (and (edge a mid) (path mid b))))
;;      (path a b))))

;;(assert (path 1 x))
;;(assert (not (or (= x 4) (= x 3) (= x 2))))

;; Solve

;(check-sat)
;(get-model)
