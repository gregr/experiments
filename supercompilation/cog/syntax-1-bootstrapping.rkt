#lang racket
(require "util.rkt")
(require "syntax-abstract.rkt")
(require "semantics-operational.rkt")
(require "data-encoding.rkt")
(require "syntax-0-parsing.rkt")
(require "syntax-0-unparsing.rkt")
(require "interaction.rkt")
(provide (all-defined-out))

(define (syntax-0-le val)
  (unparse upenv-empty (value (length-encoded val))))

;; tagged data
(define (basic-tag-def name (namespace 1))
  (syntax-0-le (symbol-encode (list namespace name))))

(define sym-tag     (basic-tag-def 'sym))
(define lam-tag     (basic-tag-def 'lam))
(define bit-tag     (basic-tag-def 'bit))
(define uno-tag     (basic-tag-def 'uno))
(define pair-tag    (basic-tag-def 'pair))

(define error-effect-tag      (basic-tag-def 'error  2))
(define gen-sym-effect-tag    (basic-tag-def 'gensym 2))

(define (_sym name) (symbol-encode (list 0 name)))
(define (sym name) `(tagged ,sym-tag ,(syntax-0-le (_sym name))))

;; bootstrapping
(define (let-module defs body)
  (foldr (lambda (def body)
           (match def
             (`(,name ,expr) `((lam (,name) ,body) ,expr))))
         body defs))

(define (std prog)
  (let-module `(
    (tagged       (lam (tag datum) (pair tag datum)))
    (tagged-tag   (lam (td) (pair-l td)))
    (tagged-datum (lam (td) (pair-r td)))

    (error        (lam (val) (produce (tagged ,error-effect-tag val))))
    (gen-sym      (lam (sym-name parent)
                    (produce (tagged ,gen-sym-effect-tag
                                     (pair sym-name parent)))))

    (pair-cons    (lam (l r) (pair l r)))
    (size-empty   (pair 0 ()))
    (size-inc     (lam (sz) (pair 1 sz)))
    (size-dec     (lam (sz) (pair-r sz)))
    (tuple-size   (lam (tup) (pair-l tup)))
    (tuple-data   (lam (tup) (pair-r tup)))
    (tuple-empty  (pair size-empty ()))
    (tuple-cons   (lam (val tup) (pair-cons (size-inc (tuple-size tup))
                                            (pair-cons val (tuple-data tup)))))
    (tuple-first  (lam (tup) (pair-l (tuple-data tup))))
    (tuple-rest   (lam (tup) (pair-cons (size-dec (tuple-size tup))
                                        (pair-r (tuple-data tup)))))

    (bit-eq?              (lam (bta btb) (if-0 bta (if-0 btb 0 1) (if-0 btb 1 0))))
    (size-eq?             (fix (size-eq? sa sb)
                            (if-0 (bit-eq? (pair-l sa) (pair-l sb))
                              (if-0 (pair-l sa) 0
                                (size-eq? (pair-r sa) (pair-r sb)))
                              1)))
    (bits-unsized-eq?     (fix (bits-unsized-eq? sz ba bb)
                            (if-0 (pair-l sz)
                              0
                              (if-0 (bit-eq? (pair-l ba) (pair-l bb))
                                (bits-unsized-eq? (pair-r sz) (pair-r ba) (pair-r bb))
                                1))))
    (bits-eq?             (lam (ba bb)
                            (if-0 (size-eq? (tuple-size ba) (tuple-size bb))
                              (bits-unsized-eq? (tuple-size ba) (tuple-data ba) (tuple-data bb))
                              (error ,(sym 'bitsize-mismatch)))))

    (bits-assoc   (fix (bits-assoc default assocs bits)
                    (if-0 (size-eq? size-empty (tuple-size assocs)) default
                      ((lam (assoc)
                        (if-0 (bits-eq? bits (pair-l assoc)) (pair-r assoc)
                          (bits-assoc default
                                      (tuple-rest assocs)
                                      bits)))
                       (tuple-first assocs)))))

    (tagged-with? (lam (tag td) (bits-eq? tag (tagged-tag td))))

    (sym?   (tagged-with? ,sym-tag))
    (lam?   (tagged-with? ,lam-tag))
    (bit?   (tagged-with? ,bit-tag))
    (uno?   (tagged-with? ,uno-tag))
    (pair?  (tagged-with? ,pair-tag))

    (pair-x      (lam (accessor p)
                   (if-0 (pair? p) (accessor (tagged-datum p))
                     (error ,(sym 'expected-pair)))))
    (lam-wrap    (lam (arg-name lam) (tagged ,lam-tag (pair lam arg-name))))
    (_lam-unwrap (lam (wrapped-lam) (pair-l (tagged-datum wrapped-lam))))
    (lam-unwrap  (lam (lm)
                   (if-0 (lam? lm) (_lam-unwrap lm)
                     (error ,(sym 'expected-lam)))))

    (pred-wrap (lam (pred) (lam-wrap () (lam (val) (tagged ,bit-tag (pred val))))))

    (1-sym?  (pred-wrap sym?))
    (1-lam?  (pred-wrap lam?))
    (1-bit?  (pred-wrap bit?))
    (1-uno?  (pred-wrap uno?))
    (1-pair? (pred-wrap pair?))

    (1-sym-eq?      (lam-wrap () (lam (sa) (lam-wrap () (lam (sb)
                      (if-0 (sym? sa)
                        (if-0 (sym? sb)
                          (tagged ,bit-tag
                            (bits-eq? (tagged-datum sa) (tagged-datum sb)))
                          (error ,(sym 'expected-sym-rhs)))
                        (error ,(sym 'expected-sym-lhs))))))))
    (1-uno          (tagged ,uno-tag ()))
    (1-0b           (tagged ,bit-tag 0))
    (1-1b           (tagged ,bit-tag 1))
    (1-pair         (lam-wrap () (lam (l) (lam-wrap () (lam (r)
                      (tagged ,pair-tag (pair l r)))))))
    (1-pair-access  (lam-wrap () (lam (bt) (lam-wrap () (lam (pr)
                      (if-0 (bit? bt)
                        (pair-access (tagged-datum bt) (pair-x (lam (p) p)))
                        (error ,(sym 'expected-bit))))))))

    (1-produce      (lam-wrap () (lam (val) (produce val))))

    (1-error        (lam-wrap () error))
    (1-gen-sym      (lam-wrap () (lam (sym-name) (lam-wrap () (lam (parent)
                      (gen-sym sym-name parent))))))
    )
    prog))

(define interact-with-0
  (compose1 interact-with right-x (curry parse-0 penv-init-0)))

(define std-0-output-prog (right-x (parse-0 penv-init-0 (std `(tuple
  lam-wrap lam-unwrap 1-uno
  1-sym? 1-lam? 1-bit? 1-uno? 1-pair?
  1-sym-eq?
  1-0b 1-1b
  1-pair 1-pair-access
  1-produce
  1-error
  1-gen-sym
  )))))

(define std-0-output (tuple-decode (value-v
  (step-big std-0-output-prog))))

(match-define (cons lam-wrap (cons lam-unwrap (cons uno-1 std-1-input)))
  (map value std-0-output))
