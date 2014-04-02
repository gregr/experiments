#lang racket
(require net/url)

(define unicode-data-raw
  (call/input-url
    (string->url "http://www.unicode.org/Public/UNIDATA/UnicodeData.txt")
    get-pure-port port->string))
(struct unicode-record (name char code category other) #:transparent)
(define unicode-records
  (for/list ((line (string-split unicode-data-raw "\n")))
    (match (string-split line ";")
      (`(,code ,name ,category . ,other)
        (let* ((code (string->number code 16))
               (char (if ((integer-in #xD800 #xDFFF) code)
                       #f (list->string (list (integer->char code))))))
          (unicode-record name char code category other))))))
(define unicode-records-categorized
  (for/fold ((cat->recs (hash)))
            ((rec (in-list unicode-records)))
    (hash-update cat->recs (unicode-record-category rec)
                 (curry cons rec) '())))
(define (unicode-category-chars category)
  (map unicode-record-char (hash-ref unicode-records-categorized category)))
(define (display-unicode-category category)
  (for ((rec (hash-ref unicode-records-categorized category)))
    (displayln (format "~v : ~a : ~a"
                       (unicode-record-char rec)
                       (unicode-record-name rec)
                       (unicode-record-code rec)))))
(define (display-unicode-range start end (per-line 16))
  (for ((code (in-range start (+ 1 end))))
    (display (list->string (list (integer->char code) #\space)))
    (if (= 0 (remainder (+ 1 (- code start)) per-line))
      (display "\n")
      (void))))

(define (list-drop-range xs range)
  (append (take xs (car range))
          (drop xs (+ (car range) (- (cadr range) (car range))))))
(define (list-drop-ranges xs ranges)
  (for/fold ((xs xs))
            ((range ranges))
    (list-drop-range xs range)))

(define brackets
  (map list
       (list-drop-ranges (unicode-category-chars "Ps") '((66 68)))
       (list-drop-ranges (unicode-category-chars "Pe") '((19 20)))))

(define (display-preferred)
  (for ((range '(
                 (#x0370 #x03ff) ; greek and coptic
                 (#x2190 #x21ff) ; arrows
                 (#x2200 #x22ff) ; math ops
                 (#x2300 #x23ff) ; misc technical
                 (#x2500 #x257f) ; box drawing
                 (#x2580 #x259f) ; block elements
                 )))
    (displayln (format "~a" range))
    (apply display-unicode-range range))
  (displayln "bracket pairs:")
  (for ((bpair brackets))
    (displayln (apply string-append bpair))))

; interesting categories:
  ; Pc Pd Ps Pe Pi Pf Po
  ; Sm Sc Sk So
