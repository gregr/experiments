#lang racket
(require "util.rkt")
(require "syntax-abstract.rkt")
(provide (all-defined-out))

;; tuple
(define tuple-nil         (uno))
(define (tuple-cons x xs) (pair x xs))
(define (tuple? val)
  (match val
    ((pair _ xs) (tuple? xs))
    ((uno)       #t)
    (_           #f)))
(define (tuple-foldr f acc tup)
  (match tup
    ((uno)       acc)
    ((pair x xs) (f x (tuple-foldr f acc xs)))))
(define (tuple-foldl f acc tup)
  (match tup
    ((uno)       acc)
    ((pair x xs) (tuple-foldl f (f x acc) xs))))

(define (tuple-length tup) (tuple-foldl (lambda (_ len) (+ len 1)) 0 tup))

(define (tuple-encode-revappend xs tup)
  (match xs
    ('()         tup)
    ((cons y ys) (tuple-encode-revappend ys (tuple-cons y tup)))))
(define (tuple-encode xs) (tuple-encode-revappend (reverse xs) tuple-nil))
(define (tuple-decode tup) (tuple-foldr cons '() tup))
(define (tuple-pad len val tup)
  (tuple-encode-revappend (make-list (- len (tuple-length tup)) val) tup))

;; nat
(define (nat-encode n)
  (tuple-pad (+ n 1) (bit (b-1)) (tuple-encode (list (bit (b-0))))))
(define (nat-decode nat) (- (tuple-length nat) 1))

;; bit
(define (bit-encode bool)
  (if bool (bit (b-1)) (bit (b-0))))
(define (bit-decode b)
  (match b
    ((bit (b-0)) #f)
    ((bit (b-1)) #t)))

;; bits
(define (bits-encode n)
  (let loop ((acc '()) (n n))
    (if (equal? 0 n)
      (tuple-encode acc)
      (loop (cons (bit-encode (odd? n)) acc)
            (floor (/ n 2))))))
(define (bits-decode bits)
  (tuple-foldl
    (lambda (b total)
      (+ (* 2 total) (if (bit-decode b) 1 0)))
    0 bits))
(define (bits-pad n bits)
  (tuple-pad n (bit (b-0)) bits))
(define (bits-count bits) (tuple-length bits))
(define (bits-required n) (bits-count (bits-encode (- n 1))))

;; length-encoding for tuple-like values
(define (length-encoded tup) (pair (nat-encode (tuple-length tup)) tup))

;; symbol
(record symbol-entry repr sub-table)
(define (symbol-repr uid bitwidth) (bits-pad bitwidth (bits-encode uid)))

(record symbol-table capacity mapping rev-mapping next-uid)
(define (symbol-table-empty capacity)
  (symbol-table capacity dict-empty dict-empty 0))
(define (symbol-table-bitwidth table)
  (bits-required (symbol-table-capacity table)))

(define (symbol-table-get table key)
  (dict-get (symbol-table-mapping table) key))
(define (symbol-table-encode table key)
  (match (symbol-table-get table key)
    ((just entry) (symbol-entry-repr entry))
    ((nothing)    (error (format "cannot encode key '~v' with table: ~v"
                                 key table)))))
(define (symbol-table-decode table symbol)
  (just-x (dict-get (symbol-table-rev-mapping table) (bits-decode symbol))))
(define (symbol-table-add table key max-children)
  (match table
    ((symbol-table capacity mapping rev-mapping next-uid)
     (if (equal? capacity next-uid)
       (error (format
                "cannot add symbol '~v' to table at max capacity: ~v"
                key table))
       (void))
     (let ((entry (symbol-entry
                    (symbol-repr next-uid (symbol-table-bitwidth table))
                    (symbol-table-empty max-children))))
       (match (symbol-table-get table key)
         ((nothing)
          (symbol-table capacity
                        (dict-add mapping key entry)
                        (dict-add rev-mapping next-uid key)
                        (+ 1 next-uid)))
         (_ (error (format "symbol already added for key: ~v" key))))))))

(define ((symbol-table-lens key) table)
  (match table
    ((symbol-table capacity mapping rev-mapping next-uid)
     (let ((entry (just-x (symbol-table-get table key))))
       (match entry
         ((symbol-entry repr sub-table)
          (define (rebuild new-table)
            (symbol-table
              capacity
              (dict-add mapping key (symbol-entry repr new-table))
              rev-mapping
              next-uid))
          (lens-result sub-table rebuild)))))))
(define (symbol-table-lens* keys) (:o (map symbol-table-lens keys)))

(define ((symbol-table-decode-lens symbol) table)
  ((symbol-table-lens (symbol-table-decode table symbol)) table))

(define (symbol-table-encode* table keys)
  (symbol-table-encode
    (:. table (symbol-table-lens* (list-init keys)))
    (last keys)))
(define (symbol-table-decode* table keys symbol)
  (symbol-table-decode
    (:. table (symbol-table-lens* keys))
    symbol))
(define (symbol-table-add* table keys max-children)
  (:~ table
      (lambda (tgt-table)
        (symbol-table-add tgt-table (last keys) max-children))
      (symbol-table-lens* (list-init keys))))

(define (symbol-table-encode** table keys tgt-keys)
  (map (curry symbol-table-encode* table)
       (map (curry append keys) (cdr (list-inits tgt-keys)))))
(define (symbol-table-decode** table keys symbols)
  (match symbols
    ('() '())
    ((cons symbol symbols)
     (let ((next-key (symbol-table-decode* table keys symbol)))
       (cons next-key
             (symbol-table-decode** table (append keys (list next-key))
                                    symbols))))))

(define symbol-capacity-default 256)  ; TODO: arbitrary-precision encoding?
(define *symbol-table* (box (symbol-table-empty symbol-capacity-default)))

(define (symbol-encode key)
  (let* ((table (unbox *symbol-table*))
         (table (match (symbol-table-get table key)
                  ((just _) table)
                  ((nothing) (symbol-add key) (unbox *symbol-table*)))))
    (symbol-table-encode table key)))
(define (symbol-decode symbol)
  (symbol-table-decode (unbox *symbol-table*) symbol))
(define (symbol-add key (max-children 0))
  (set-box! *symbol-table*
            (symbol-table-add (unbox *symbol-table*) key max-children)))

(define (symbol-encode* keys)
  (symbol-table-encode* (unbox *symbol-table*) keys))
(define (symbol-decode* keys symbol)
  (symbol-table-decode* (unbox *symbol-table*) keys symbol))
(define (symbol-add* keys (max-children 0))
  (set-box! *symbol-table*
            (symbol-table-add* (unbox *symbol-table*) keys max-children)))

(define (symbol-encode** keys tgt-keys)
  (symbol-table-encode** (unbox *symbol-table*) keys tgt-keys))
(define (symbol-decode** keys symbols)
  (symbol-table-decode** (unbox *symbol-table*) keys symbols))
