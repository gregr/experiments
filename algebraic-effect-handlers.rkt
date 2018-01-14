#lang racket

(define (id h v) v)

(define (extend-env env name value)
  `((,name . ,value) . ,env))

(define (lookup env name)
  (let ((binding (assoc name env)))
    (if binding
      (cdr binding)
      (error 'lookup (format "unbound variable: ~s" name)))))

(define (extend-handlers env henv k ehandlers)
  (define handlers
    (map (lambda (neh)
           `(,(car neh) ,(evaluate (cadr neh) env henv id)))
         ehandlers))
  (let handle ((remaining-handlers handlers) (hk k) (henv henv))
    (lambda (invoked-name arg k)
      (if invoked-name
        (match remaining-handlers
          ('() (henv invoked-name arg (lambda (h v)
                                        (k (handle handlers hk h) v))))
          (`((,name ,handler) . ,rest-handlers)
            (if (equal? invoked-name name)
              ((handler arg id #f)
               (lambda (v kk h) (k (handle handlers kk h) v))
               hk henv)
              ((handle rest-handlers hk henv) invoked-name arg k))))
        (if arg henv hk)))))

(define (evaluate expr env henv k)
  (match expr
    (#t (k henv #t))
    (#f (k henv #f))
    ((? number? x) (k henv x))
    (`(quote ,datum) (k henv datum))

    ((? symbol? x) (k henv (lookup env x)))

    (`(cons ,ea ,ed)
      (evaluate ea env henv
                (lambda (henv a)
                  (evaluate ed env henv
                            (lambda (henv d) (k henv (cons a d)))))))

    (`(car ,ec) (evaluate ec env henv (lambda (h c) (k h (car c)))))

    (`(cdr ,ec) (evaluate ec env henv (lambda (h c) (k h (cdr c)))))

    (`(null? ,e) (evaluate e env henv (lambda (h v) (k h (null? v)))))

    (`(+ ,ex ,ey)
      (evaluate ex env henv
                (lambda (henv x)
                  (evaluate ey env henv
                            (lambda (henv y)
                              (k henv (+ x y)))))))

    (`(if ,ec ,et ,ef)
      (evaluate ec env henv
                (lambda (henv c)
                  (if c
                    (evaluate et env henv k)
                    (evaluate ef env henv k)))))

    (`(lambda (,x) ,body)
      (k henv (lambda (a k henv)
                (evaluate body (extend-env env x a) henv k))))

    (`(handle ,body ,return ,handlers)
      (let ((preturn (evaluate return env henv id)))
        (evaluate body env (extend-handlers env henv k handlers)
                  (lambda (henv returned)
                    ;; (henv #f #f #f) is a hack to extract updated k.
                    ;; (henv #f #t #f) is a hack to get the previous henv.
                    (preturn returned (henv #f #f #f) (henv #f #t #f))))))

    (`(invoke ,name ,rand)
      (evaluate rand env henv
                (lambda (henv rand-value)
                  (henv name rand-value k))))

    (`(,rator ,rand)
      (evaluate rator env henv
                (lambda (henv p)
                  (evaluate rand env henv
                            (lambda (henv a) (p a k henv))))))))

(define (henv-initial . _)
  (error 'evaluate (format "unhandled handler: ~s" _)))

(define (ev expr) (evaluate expr '() henv-initial id))

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
