#lang racket
(require racket/stxparam)
(provide (all-defined-out))

(define (pretty-string x) (call-with-output-string (curry pretty-print x)))

(define (list-set xs idx val)
  (let-values (((start end) (split-at xs idx)))
              (append start (cons val (cdr end)))))

(define-for-syntax (identifier-with-? ident)
  (datum->syntax
    ident
    (string->symbol
      (string-append (symbol->string (syntax->datum ident))
                     "?"))))

(define-syntax variant
  (syntax-rules ()
    ((_) (void))
    ((_ (name fields) more ...)
      (begin (struct name fields #:transparent) (variant more ...)))))
(define-syntax data
  (syntax-rules ()
    ((_ name var ...) (variant var ...))))
(define-syntax (records stx)
  (syntax-case stx ()
    ((_ name (rname rfield ...) ...)
     #`(begin
         (define (#,(identifier-with-? #'name) datum)
           (or
             #,@(map
                  (lambda (ident)
                    (list (identifier-with-? ident) #'datum))
                  (syntax->list #'(rname ...)))))
         (record rname rfield ...) ...))))
; TODO use data name to store properties
; for example, collect typeclass/multimethod instances
;   allow do notation to refer only to data name, rather than name-monad

(require (for-syntax racket/list))

(define-syntax (record-hash stx)
  (syntax-case stx ()
    ((_ field ...)
     (let ((kvs (flatten (map syntax->list
                              (syntax->list #'(('field field) ...))))))
       #`(hash #,@kvs)))))

(define-syntax record-struct
  (syntax-rules ()
    ((_ name (field ...) struct-rest ...)
     (struct name (field ...) #:transparent
      #:methods gen:dict
      ((define (dict-ref rec . rest)
         (match rec
           ((name field ...)
            (apply hash-ref
                   (cons (record-hash field ...) rest)))))
       (define (dict-set rec key val)
         (match rec
           ((name field ...)
            (let ((temp (hash-set (record-hash field ...) key val)))
              (name (hash-ref temp 'field) ...)))))
       (define (dict-iterate-first rec)
         (if (empty? '(field ...)) #f 0))
       (define (dict-iterate-next rec pos)
         (let ((next (+ pos 1)))
           (if (< next (length '(field ...))) next #f)))
       (define (dict-iterate-key rec pos)
         (list-ref '(field ...) pos))
       (define (dict-iterate-value rec pos)
         (match rec
           ((name field ...) (list-ref (list field ...) pos))))
       (define (dict-count rec)
         (length '(field ...))))
      struct-rest ...))))

(define-syntax record
  (syntax-rules ()
    ((_ name field ...) (record-struct name (field ...)))))

; cursors
(define (ref+set datum)
  (cond
    ((list? datum) (list list-ref list-set))
    ((dict? datum) (list dict-ref dict-set))))
(record cursor focus trail ancestors)
(define (cursor-new datum) (cursor datum '() '()))
(define (cursor-refocus cur new-focus) (dict-set cur 'focus new-focus))
(define (cursor-ascend cur)
  (match cur
    ((cursor focus (cons key keys) (cons parent ancestors))
     (match-let* ((`(,_ ,p-set) (ref+set parent))
                  (new-focus (p-set parent key focus)))
       (cursor new-focus keys ancestors)))))
(define (cursor-ascend-to cur-src cur-tgt)
  (for/fold ((cur cur-src))
            ((idx (in-range (- (length (cursor-trail cur-src))
                               (length (cursor-trail cur-tgt))))))
    (cursor-ascend cur)))
(define (cursor-ascend* cur) (cursor-ascend-to cur (cursor-new '())))
(define (cursor-descend cur key)
  (match cur
    ((cursor focus keys ancestors)
     (match-let* ((`(,p-ref ,_) (ref+set focus))
                  (new-focus (p-ref focus key)))
       (cursor new-focus (cons key keys) (cons focus ancestors))))))
(define (cursor-descend* cur keys)
  (foldl (flip cursor-descend) cur keys))

; cursor notation
(define :o (curry apply append))
(define ::^
  (case-lambda
    ((cur) (cursor-ascend cur))
    ((cur-src cur-tgt) (cursor-ascend-to cur-src cur-tgt))))
(define (::^. cur-src cur-tgt)
  (cursor-focus (cursor-ascend-to cur-src cur-tgt)))
(define ::^* cursor-ascend*)
(define ::^*. (compose1 cursor-focus cursor-ascend*))
(define (::@* cur paths) (cursor-descend* cur (:o paths)))
(define (::@ cur . paths) (::@* cur paths))
(define (::. cur . paths) (cursor-focus (::@* cur paths)))
(define (::= cur val . paths)
  (cursor-ascend-to (cursor-refocus (::@* cur paths) val)
                    cur))
(define (::~ cur trans . paths)
  (let ((cur-next (::@* cur paths)))
    (cursor-ascend-to
      (cursor-refocus cur-next (trans (cursor-focus cur-next)))
      cur)))

; lens-like operators
(define (:. src . paths)       (::. (cursor-new src) (:o paths)))
(define (:= src val . paths)   (::. (::= (cursor-new src) val (:o paths))))
(define (:~ src trans . paths) (::. (::~ (cursor-new src) trans (:o paths))))
(define (:.* src . path)       (:. src path))
(define (:=* src . path)       (:= src path))
(define (:~* src . path)       (:~ src path))


(record monad pure bind)

(define-syntax do-with
  (syntax-rules (<-)
    ((_ combine pat <- stmt rest ...)
      (combine stmt (match-lambda (pat (do-with combine rest ...)))))
    ((_ combine pat = stmt rest ...)
      (match-let ((pat stmt)) (do-with combine rest ...)))
    ((_ combine stmt) stmt)))
(define-syntax-parameter pure
  (lambda (stx) (raise-syntax-error #f "must be used inside 'do'" stx)))
(define-syntax do
  (syntax-rules ()
    ((_ monad stmt ...)
      (syntax-parameterize
        ((pure (syntax-rules () ((_ ex) ((monad-pure monad) ex)))))
          (do-with (monad-bind monad) stmt ...)))))

(define (map-monad monad proc xs)
  (match xs
    ('() (do monad (pure '())))
    ((cons y ys)
      (do monad
        y0 <- (proc y)
        ys0 <- (map-monad monad proc ys)
        (pure (cons y0 ys0))))))

(records maybe
  (nothing)
  (just x))
; TODO: automatically generate folds from data definitions
(define (maybe-fold nothing-fold just-fold maybe)
  (match maybe
    ((nothing) nothing-fold)
    ((just x) (just-fold x))))
(define (maybe-from nothing-from maybe)
  (match maybe
    ((nothing) nothing-from)
    ((just x) x)))
(define maybe-monad (monad
  just
  (lambda (prev next)
    (match prev
      ((nothing) (nothing))
      ((just x) (next x))))))

(records either
  (left x)
  (right x))
(define (either-fold left-fold right-fold either)
  (match either
    ((left x) (left-fold x))
    ((right x) (right-fold x))))
(define either-monad (monad
  right
  (lambda (prev next)
    (match prev
      ((left x) (left x))
      ((right x) (next x))))))
(define (either-iterate f arg)
  (match (f arg)
    ((left _)    arg)
    ((right arg) (either-iterate f arg))))

(define (maybe->either left-arg maybe)
  (maybe-fold (left left-arg) right maybe))

(define ((flip proc) x y) (proc y x))

(define (zip xs ys) (map cons xs ys))

(define (iterate proc seed count)
  (if (<= count 0) (list seed)
    (cons seed (iterate proc (proc seed) (- count 1)))))

(define (list-index lst key)
  (let loop ((lst lst) (key key) (index 0))
    (match lst
      ('() (nothing))
      ((cons key0 lst)
        (if (equal? key0 key) (just index) (loop lst key (+ index 1)))))))
(define (list-init lst) (reverse (cdr (reverse lst))))
(define (list-inits lst) (reverse (iterate list-init lst (length lst))))

(define dict-empty (hash))
(define (dict-add dct key val) (dict-set dct key (just val)))
(define (dict-get dct key) (dict-ref dct key (nothing)))
(define (dict-get-default dct key default)
  (maybe-from default (dict-get dct key)))

(define set-empty (set))
(define (set-unions ss)
  (match ss ('() set-empty) (_ (apply set-union ss))))

; TODO:
; for1[-monad]: flip last two params of map[-monad]

; testing

;(display
  ;(do-with (lambda (prev next) (+ 1 (next prev)))
    ;a (+ 3 4)
    ;b (+ a 5)
    ;b))
;(newline)

;> (penv-syntax-add penv-empty 'x 'y)
;(penv (dict (list (cons 'x (just 'y)))) '())
;> (penv-vars-add penv-empty 'z)
;(penv (dict '()) '(z))
;> (penv-vars-add (penv-syntax-add penv-empty 'x 'y) 'z)
;(penv (dict (list (cons 'x (just 'y)))) '(z))
;> (penv-syntax-del (penv-vars-add (penv-syntax-add penv-empty 'x 'y) 'z) 'x)
;(penv (dict (list (cons 'x (nothing)) (cons 'x (just 'y)))) '(z))
;>

;> (list-index (list 1 2 3 4 5 'a 'b 'c) 4)
;(just 3)
;> (list-index (list 1 2 3 4 5 'a 'b 'c) 'b)
;(just 6)
;> (list-index (list 1 2 3 4 5 'a 'b 'c) 'd)
;(nothing)
;> (list-index (list 1 2 3 4 5 'a 'b 'c) 7)
;(nothing)
;> (list-index (list 1 2 3 4 5 'a 'b 'c) 0)
;(nothing)
;> (list-index (list 1 2 3 4 5 'a 'b 'c) 1)
;(just 0)
;>
