#lang racket

(define (extend-env env name value)
  `((,name . ,value) . ,env))

(define (lookup env name)
  (let ((binding (assoc name env)))
    (if binding
      (cdr binding)
      (error 'lookup (format "unbound variable: ~s" name)))))

(define ((not-keyword? env) head)
  (or (not (symbol? head)) (assoc head env)))


;; Direct version:

(define (dk->k dk k) (lambda (v) (dk (k v))))

(define (tagged? tag d) (and (vector? d) (eq? tag (vector-ref d 0))))

(define (value datum) `#(value ,datum))
(define (value? d) (tagged? 'value d))
(define (value-datum d) (vector-ref d 1))

(define (effect name payload k) `#(effect ,name ,payload ,k))
(define (effect? d) (tagged? 'effect d))
(define (effect-name d) (vector-ref d 1))
(define (effect-payload d) (vector-ref d 2))
(define (effect-k d) (vector-ref d 3))
(define (effect/dk d dk)
  (effect (effect-name d) (effect-payload d) (dk->k dk (effect-k d))))

(define (bind d k)
  (if (value? d)
    (k (value-datum d))
    (effect/dk d (lambda (d) (bind d k)))))

(define (eval-primop1 op expr env)
  (bind (evaluate expr env) (lambda (v) (value (op v)))))

(define (eval-primop2 op ea eb env)
  (bind (evaluate ea env)
        (lambda (a) (bind (evaluate eb env)
                          (lambda (b) (value (op a b)))))))

(define (value! d) (if (value? d) (value-datum d)
                     (error 'evaluate (format "unexpected effect: ~s" d))))

(define (eval-value expr env) (value! (evaluate expr env)))

(define (apply-handler handler payload k) ((value! (handler payload)) k))

(define (evaluate expr env)
  (match expr
    (#t              (value #t))
    (#f              (value #f))
    ((? number? x)   (value x))
    ((? symbol? x)   (value (lookup env x)))

    (`(,(? (not-keyword? env) rator) ,rand)
      (bind (evaluate rator env)
            ;; Alternatively: (lambda (p) (bind (evaluate rand env) p))
            (lambda (p) (bind (evaluate rand env)
                              (lambda (a) (p a))))))

    (`(quote ,datum) (value datum))

    (`(car ,ec)      (eval-primop1 car ec env))
    (`(cdr ,ec)      (eval-primop1 cdr ec env))
    (`(null? ,e)     (eval-primop1 null? e env))
    (`(cons ,ea ,ed) (eval-primop2 cons ea ed env))
    (`(+ ,ex ,ey)    (eval-primop2 + ex ey env))
    (`(* ,ex ,ey)    (eval-primop2 * ex ey env))

    (`(if ,ec ,et ,ef)
      (bind (evaluate ec env)
            (lambda (c) (if c (evaluate et env) (evaluate ef env)))))

    (`(lambda (,x) ,body)
      (value (lambda (a) (evaluate body (extend-env env x a)))))

    (`(handle ,body ,ereturn ,ehandlers)
      (define return (eval-value ereturn env))
      (define handlers  ;; ((name . handler) ...)
        (map (lambda (neh) (cons (car neh) (eval-value (cadr neh) env)))
             ehandlers))
      (define (handle d)
        (define hb (and (effect? d) (assoc (effect-name d) handlers)))
        (cond ((value? d) (return (value-datum d)))
              (hb (apply-handler
                    (cdr hb) (effect-payload d) (dk->k handle (effect-k d))))
              (else (effect/dk d handle))))
      (handle (evaluate body env)))

    (`(invoke ,name ,rand)
      (bind (evaluate rand env)
            (lambda (a) (effect name a (lambda (v) (value v))))))))

(define (ev expr) (evaluate expr '()))


;; Continuation-passing version:

(define (id h v) v)

(define (henv->k henv) (caar henv))
(define (henv->handlers henv) (cadar henv))
(define (henv->henv-prev henv) (cdr henv))
(define (make-henv k handlers henv-prev) `((,k ,handlers) . ,henv-prev))

(define (invoke-handler henv-full invoked-name arg k)
  (define handlers (henv->handlers henv-full))
  (let handle ((remaining-handlers handlers)
               (hk (henv->k henv-full))
               (henv (henv->henv-prev henv-full)))
    (match remaining-handlers
      ('() (if henv
             (invoke-handler henv invoked-name arg
                             (lambda (h v)
                               (k (make-henv hk handlers h) v)))
             (error 'invoke-handler
                    (format "unhandled handler: ~s" invoked-name))))
      (`((,name ,handler) . ,rest-handlers)
        (if (equal? invoked-name name)
          ((handler arg id #f)
           (lambda (v kk h) (k (make-henv kk handlers h) v))
           hk henv)
          (handle rest-handlers hk henv))))))

(define (extend-handlers env henv k ehandlers)
  (define handlers
    (map (lambda (neh)
           `(,(car neh) ,(evaluate-k (cadr neh) env henv id)))
         ehandlers))
  (make-henv k handlers henv))

(define (evaluate-k expr env henv k)
  (match expr
    (#t (k henv #t))
    (#f (k henv #f))
    ((? number? x) (k henv x))
    ((? symbol? x) (k henv (lookup env x)))

    (`(,(? (not-keyword? env) rator) ,rand)
      (evaluate-k rator env henv
                  (lambda (henv p)
                    (evaluate-k rand env henv
                                (lambda (henv a) (p a k henv))))))

    (`(quote ,datum) (k henv datum))

    (`(cons ,ea ,ed)
      (evaluate-k ea env henv
                (lambda (henv a)
                  (evaluate-k ed env henv
                            (lambda (henv d) (k henv (cons a d)))))))

    (`(car ,ec) (evaluate-k ec env henv (lambda (h c) (k h (car c)))))

    (`(cdr ,ec) (evaluate-k ec env henv (lambda (h c) (k h (cdr c)))))

    (`(null? ,e) (evaluate-k e env henv (lambda (h v) (k h (null? v)))))

    (`(+ ,ex ,ey)
      (evaluate-k ex env henv
                (lambda (henv x)
                  (evaluate-k ey env henv
                            (lambda (henv y)
                              (k henv (+ x y)))))))
    (`(* ,ex ,ey)
      (evaluate-k ex env henv
                (lambda (henv x)
                  (evaluate-k ey env henv
                            (lambda (henv y)
                              (k henv (* x y)))))))

    (`(if ,ec ,et ,ef)
      (evaluate-k ec env henv
                (lambda (henv c)
                  (if c
                    (evaluate-k et env henv k)
                    (evaluate-k ef env henv k)))))

    (`(lambda (,x) ,body)
      (k henv (lambda (a k henv)
                (evaluate-k body (extend-env env x a) henv k))))

    (`(handle ,body ,return ,handlers)
      (let ((preturn (evaluate-k return env henv id)))
        (evaluate-k body env (extend-handlers env henv k handlers)
                  (lambda (henv returned)
                    (preturn
                      returned (henv->k henv) (henv->henv-prev henv))))))

    (`(invoke ,name ,rand)
      (evaluate-k rand env henv
                (lambda (henv rand-value)
                  (invoke-handler henv name rand-value k))))))

(define (ev-k expr) (evaluate-k expr '() #f id))

(ev '(((lambda (x) (lambda (y) x)) (cons 'one '1)) (cons 'two '2)))

(ev
  '((handle
      ;'here

      ;; This returns 'initial.
      ;(invoke get '#f)

      ;; This returns 'unit.
      ;(invoke set 'new-value)

      ;; These both return 'new-value.
      ;((lambda (_) (invoke get '#f)) (invoke set 'new-value))
      (invoke get (invoke set 'new-value))

      ;; return
      (lambda (returned) (lambda (v) returned))

      ((get (lambda (_)
              (lambda (k)
                (lambda (v) ((k v) v)))))
       (set (lambda (a)
              (lambda (k)
                (lambda (_) ((k 'unit) a)))))))
    'initial))

(ev
  '((handle
      ((handle
         ;'here2

         ;; '(s0 . s1)
         ;(cons (invoke get0 '#f) (invoke get1 '#f))

         ;; '(new-value0 . new-value1)
         (((lambda (_)
             (lambda (_)
               (cons (invoke get0 '#f) (invoke get1 '#f))))
           (invoke set0 'new-value0))
          (invoke set1 'new-value1))

         ;; return
         (lambda (returned) (lambda (v) returned))

         ((get1 (lambda (_)
                  (lambda (k)
                    (lambda (v) ((k v) v)))))
          (set1 (lambda (a)
                  (lambda (k)
                    (lambda (_) ((k 'unit) a)))))))
       's1)

      (lambda (returned) (lambda (v) returned))

      ((get0 (lambda (_)
               (lambda (k)
                 (lambda (v) ((k v) v)))))
       (set0 (lambda (a)
               (lambda (k)
                 (lambda (_) ((k 'unit) a)))))))
    's0))

(define (let-in name code body) `((lambda (,name) ,body) ,code))

(define Z '(lambda (f)
             ((lambda (d) (d d))
              (lambda (x) (f (lambda (a) ((x x) a)))))))

(define (fix-in name code body) `((lambda (,name) ,body)
                                  (,Z (lambda (,name) ,code))))


(ev
  `(handle
     ;; '(5 6 7 14 15 16)
     ,(let-in 'x '(invoke choose (cons 1 (cons 10 '())))
              (let-in 'y '(invoke choose (cons 4 (cons 5 (cons 6 '()))))
                      '(+ x y)))

     ;; return
     (lambda (returned) (cons returned '()))

     ((choose
        (lambda (choices)
          (lambda (k)
            ,(fix-in 'append
                     '(lambda (xs)
                        (lambda (ys)
                          (if (null? xs) ys
                            (cons (car xs) ((append (cdr xs)) ys)))))
                     (fix-in 'loop
                             '(lambda (cs)
                                (if (null? cs) '()
                                  ((append (k (car cs))) (loop (cdr cs)))))
                             '(loop choices)))))))))

(ev
  `(handle
     ,(let-in
        'prompt '(lambda (t) (invoke prompt t))
        (let-in
          'control '(lambda (f) (invoke control f))

          '(cons
             'result:
             (prompt
               (lambda (_)
                 ((((lambda (_)
                      (lambda (_)
                        (lambda (_) '())))
                    (control (lambda (k) (cons 1 (k 'unit)))))
                   (control (lambda (k) (cons 2 (k 'unit)))))
                  (control (lambda (k) (cons 3 (k 'unit))))))))))

     (lambda (returned) returned)

     ((prompt (lambda (thunk)
                (lambda (k)
                  (k (handle
                       (thunk 'unit)

                       (lambda (returned) returned)

                       ((control (lambda (f)
                                   (lambda (k) (f k)))))))))))))

