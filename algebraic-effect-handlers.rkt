#lang racket


(define (id v) v)

(define (extend-env env name value)
  `((,name . ,value) . ,env))

(define (lookup env name)
  (let ((binding (assoc name env)))
    (if binding
      (cdr binding)
      (error 'lookup (format "unbound variable: ~s" name)))))

(define (extend-handlers env henv hk ehandlers)
  (define handlers
    (map (lambda (neh)
           `(,(car neh) ,(evaluate (cadr neh) env henv id)))
         ehandlers))
  (let handle ((handlers handlers))
    (lambda (invoked-name arg k)
      (match handlers
        ('() (henv invoked-name arg k))
        (`((,name ,handler) . ,rest-handlers)
          (if (equal? invoked-name name)
            ;((handler arg id) (lambda (v _) (k v)) id)
            ;((handler arg id) (lambda (v _) (k v)) hk)

            ;; TODO: restore updated henv around (k v)
            ((handler arg id) (lambda (v _) (k v)) hk)

            ((handle rest-handlers) invoked-name arg k)))))))

(define (evaluate expr env henv k)
  (match expr
    (`(quote ,datum) (k datum))

    ((? symbol? x) (k (lookup env x)))

    (`(cons ,ea ,ed)
      (evaluate ea env henv
                (lambda (a)
                  (evaluate ed env henv
                            (lambda (d) (k (cons a d)))))))

    (`(car ,ec) (evaluate ec env henv (lambda (c) (k (car c)))))

    (`(cdr ,ec) (evaluate ec env henv (lambda (c) (k (cdr c)))))

    (`(if ,ec ,et ,ef)
      (evaluate ec env henv
                (lambda (c)
                  (if c
                    (evaluate et env henv k)
                    (evaluate ef env henv k)))))

    (`(lambda (,x) ,body)
      (k (lambda (a k)
           (evaluate body (extend-env env x a) henv k))))

    (`(handle ,body ,return ,handlers)
      (let ((preturn (evaluate return env henv id)))
        (evaluate body env (extend-handlers env henv k handlers)
                  (lambda (returned) (preturn returned k)))))

    (`(invoke ,name ,rand)
      (evaluate rand env henv
                (lambda (rand-value)
                  (henv name rand-value k))))

    (`(,rator ,rand)
      (evaluate rator env henv
                (lambda (p)
                  (evaluate rand env henv
                            (lambda (a) (p a k))))))))

(define (henv-initial . _)
  (error 'evaluate (format "unhandled handler: ~s" _)))

(define (ev expr) (evaluate expr '() henv-initial id))

(ev '(((lambda (x) (lambda (y) x)) '1) '2))

(ev
  '((handle
      ;'here
      ;(invoke get '#f)
      ;(invoke set 'new-value)

      ((lambda (_) (invoke get '#f)) (invoke set 'new-value))

      ;(invoke get (invoke set 'new-value))

      ;; return
      (lambda (returned) (lambda (v) returned))

      ((get (lambda (_)
              (lambda (k)
                (lambda (v) ((k v) v)))))
       (set (lambda (a)
              (lambda (k)
                (lambda (_) ((k 'unit) a)))))))
    'initial))


;; TODO: test nested handle
