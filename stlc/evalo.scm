(load "mk/mk.scm")

(define (:o expr type) (:expro '() expr type))

(define (:expro env expr type)
  (conde ((symbolo expr) (lookupo env expr type))
         ((fresh (x body a b env^)
            (== expr `(lambda (,x) ,body))
            (== type `(,a -> ,b))
            (== env^ `((,x . ,a) . ,env))
            (symbolo x)
            (:expro env^ body b)))
         ((fresh (rator rand a->type a)
            (== expr `(,rator ,rand))
            (== a->type `(,a -> ,type))
            (:expro env rand a)
            (:expro env rator a->type)))))

(define (lookupo env x t)
  (fresh (env^ y u)
    (conde ((== env `((,x . ,t) . ,env^)))
           ((== env `((,y . ,u) . ,env^))
            (=/= x y)
            (lookupo env^ x t)))))


;; type checking/inference/inhabitation for normalized terms
(define (:normalo nexpr type) (:normal-expro '() nexpr type))

(define (:normal-expro env nexpr type)
  (conde ((:elimo env nexpr type))
         ((fresh (x body a b env^)
            (== nexpr `(lambda (,x) ,body))
            (== type  `(,a -> ,b))
            (== env^  `((,x . ,a) . ,env))
            (symbolo x)
            (:normal-expro env^ body b)))))

(define (:elimo env nexpr type)
  (fresh (x t)
    (lookupo env x t)
    (:resulto env x t nexpr nexpr type)))

(define (:resulto env expr type e-rator e-result t-result)
  (conde ((== type t-result) (== expr e-result))
         ((fresh (ea ta tb e1 e2)
            (== type `(,ta -> ,tb))
            (== e-rator `(,e1 ,e2))  ;; refutational completeness guard
            (:resulto env `(,expr ,ea) tb e1 e-result t-result)
            (:normal-expro env ea ta)))))


;; type checking/inference/inhabitation for acyclic normalized terms
;; Acyclic terms do not reuse variales that have appeared in [nested] operator
;; position in a parent term.
;; e.g., (((x A) B) C) is acyclic only if subterms A, B, C do not reuse x
(define (:acyclico aexpr type) (:acyclic-expro '() '() aexpr type))

(define (:acyclic-expro env used nexpr type)
  (conde ((:acyclic-elimo env used nexpr type))
         ((fresh (x body a b env^)
            (== nexpr `(lambda (,x) ,body))
            (== type  `(,a -> ,b))
            (== env^  `((,x . ,a) . ,env))
            (symbolo x)
            (:acyclic-expro env^ used body b)))))

(define (:acyclic-elimo env used nexpr type)
  (fresh (x t)
    (not-membero x used)
    (lookupo env x t)
    (:acyclic-resulto env `(,x . ,used) x t nexpr nexpr type)))

(define (:acyclic-resulto env used expr type e-rator e-result t-result)
  (conde ((== type t-result) (== expr e-result))
         ((fresh (ea ta tb e1 e2)
            (== type `(,ta -> ,tb))
            (== e-rator `(,e1 ,e2))  ;; refutational completeness guard
            (:acyclic-resulto env used `(,expr ,ea) tb e1 e-result t-result)
            (:acyclic-expro env used ea ta)))))

(define (not-membero x xs)
  (conde ((== xs '()))
         ((fresh (y ys)
            (== xs `(,y . ,ys))
            (=/= x y)
            (not-membero x ys)))))


;; :o and :normalo are not refutationally complete
;; :normalo behaves better than :o, but will fail to notice hopeless cycles
;; :acyclico should be refutationally complete

;; decidable type inhabitation

;; these fail to terminate with :normalo
;> (run* (x) (:acyclico x '((a -> a) -> a))))
;()
;> (run* (x) (:acyclico x '((a -> b) ((b -> a) -> a))))
;()

;> (run* (x) (:normalo x '((a -> b) -> b)))
;()
;> (run* (x) (:acyclico x '((a -> b) -> a))))
;()
;> (run* (x) (:normalo '(lambda (y) (lambda (x) (x x))) x)))
;()
;> (run* (x) (:acyclico '(lambda (y) (lambda (x) (x x))) x)))
;()
;> (run* (x) (:normalo '(lambda (x) (x x)) x)))
;()
;> (run* (x) (:acyclico '(lambda (x) (x x)) x)))
;()

;; neither (~~p -> p) nor ~(~~p -> p) is a theorem
;> (run* (x) (:normalo x '(((a -> false) -> false) -> a)))
;()
;> (run* (x) (:acyclico x '(((a -> false) -> false) -> a)))
;()
;> (run* (x) (:normalo x '((((a -> false) -> false) -> a) -> false)))
;()
;> (run* (x) (:acyclico x '((((a -> false) -> false) -> a) -> false)))
;()

;; but (~~~p -> ~p) is a theorem
;> (run 3 (x) (:normalo x '((((a -> false) -> false) -> false) -> (a -> false))))
;(((lambda (_.0)
;    (lambda (_.1) (_.0 (lambda (_.2) (_.2 _.1)))))
;   (=/= ((_.0 _.1)) ((_.1 _.2)))
;   (sym _.0 _.1 _.2))
;  ((lambda (_.0)
;     (lambda (_.1)
;       (_.0 (lambda (_.2) (_.0 (lambda (_.3) (_.3 _.1)))))))
;    (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.1 _.2)) ((_.1 _.3)))
;    (sym _.0 _.1 _.2 _.3))
;  ((lambda (_.0)
;     (lambda (_.1)
;       (_.0 (lambda (_.2) (_.0 (lambda (_.3) (_.2 _.1)))))))
;    (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.1 _.2)) ((_.1 _.3))
;         ((_.2 _.3)))
;    (sym _.0 _.1 _.2 _.3)))
;> (run* (x) (:acyclico x '((((a -> false) -> false) -> false) -> (a -> false))))
;(((lambda (_.0)
;    (lambda (_.1) (_.0 (lambda (_.2) (_.2 _.1)))))
;   (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.1 _.2)))
;   (sym _.0 _.1 _.2)))

;> (run* (x) (:normalo x '((a -> b) -> ((b -> c) -> (a -> c)))))
;(((lambda (_.0)
;    (lambda (_.1) (lambda (_.2) (_.1 (_.0 _.2)))))
;   (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.1 _.2)))
;   (sym _.0 _.1 _.2)))
;> (run* (x) (:acyclico x '((a -> b) -> ((b -> c) -> (a -> c)))))
;(((lambda (_.0)
;    (lambda (_.1) (lambda (_.2) (_.1 (_.0 _.2)))))
;   (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.1 _.2)))
;   (sym _.0 _.1 _.2)))

;> (run* (x) (:normalo '(lambda (a) (lambda (b) (a (lambda (c) (c b))))) x))
;((((((_.0 -> _.1) -> _.1) -> _.2) -> (_.0 -> _.2))))
;> (run* (x) (:acyclico '(lambda (a) (lambda (b) (a (lambda (c) (c b))))) x))
;((((((_.0 -> _.1) -> _.1) -> _.2) -> (_.0 -> _.2))))

;> (run* (x) (:normalo x '(a -> a)))
;(((lambda (_.0) _.0) (sym _.0)))
;> (run* (x) (:acyclico x '(a -> a)))
;(((lambda (_.0) _.0) (sym _.0)))
;> (run* (x) (:normalo x '(a -> (a -> a))))
;(((lambda (_.0) (lambda (_.1) _.1)) (sym _.0 _.1))
;  ((lambda (_.0) (lambda (_.1) _.0))
;    (=/= ((_.0 _.1)))
;    (sym _.0 _.1)))
;> (run* (x) (:acyclico x '(a -> (a -> a))))
;(((lambda (_.0) (lambda (_.1) _.1)) (sym _.0 _.1))
;  ((lambda (_.0) (lambda (_.1) _.0))
;    (=/= ((_.0 _.1)))
;    (sym _.0 _.1)))
;> (run* (x) (:normalo x '(a -> (b -> a))))
;(((lambda (_.0) (lambda (_.1) _.0))
;   (=/= ((_.0 _.1)))
;   (sym _.0 _.1)))
;> (run* (x) (:acyclico x '(a -> (b -> a))))
;(((lambda (_.0) (lambda (_.1) _.0))
;   (=/= ((_.0 _.1)))
;   (sym _.0 _.1)))
;> (run* (x) (:normalo x '(a -> (b -> b))))
;(((lambda (_.0) (lambda (_.1) _.1)) (sym _.0 _.1)))
;> (run* (x) (:acyclico x '(a -> (b -> b))))
;(((lambda (_.0) (lambda (_.1) _.1)) (sym _.0 _.1)))

;> (run 1 (x z) (:normalo `(lambda (a) ,z) x))
;((((_.0 -> _.0) a)))
;> (run 2 (x z) (:normalo `(lambda (a) (lambda (b) ,z)) x))
;((((_.0 -> (_.1 -> _.1)) b))
; (((_.0 -> (_.1 -> _.0)) a)))
;> (run 2 (x z) (:acyclico `(lambda (a) (lambda (b) ,z)) x))
;((((_.0 -> (_.1 -> _.1)) b))
; (((_.0 -> (_.1 -> _.0)) a)))


;; TODO: maybe define evalo, like the filename suggests
