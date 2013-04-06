;; How much harder would it be to prove quicksort correct than mergesort?
;; see mergesort.scm for the approach

(defun quick-lteq (threshold xs)
  (if (endp xs) nil
    (let ((part (quick-lteq threshold (cdr xs))))
      (if (<= (car xs) threshold)
        (cons (car xs) part)
        part))))

(defun quick-gt (threshold xs)
  (if (endp xs) nil
    (let ((part (quick-gt threshold (cdr xs))))
      (if (> (car xs) threshold)
        (cons (car xs) part)
        part))))

(defun quicksort (xs)
  (if (endp xs) nil
    (append (quicksort (quick-lteq (car xs) (cdr xs)))
            (cons (car xs) (quicksort (quick-gt (car xs) (cdr xs)))))))

;(quicksort '(3 68 45 2 6 -7 44 88 9 3 6))

(defun sortedp (xs)
  (if (endp xs) t
    (if (endp (cdr xs)) t
      (and (<= (car xs) (car (cdr xs)))
           (sortedp (cdr xs))))))

;(sortedp '(8 34 2 1 6 83 7 2 5))
;(sortedp (quicksort '(8 34 2 1 6 83 7 2 5)))

(defun occurrences (a xs)
  (if (endp xs) 0
    (+ (occurrences a (cdr xs))
       (if (equal a (car xs)) 1 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prove sorted
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lteqp (val xs)
  (if (endp xs) t
    (and (<= (car xs) val)
         (lteqp val (cdr xs)))))

(defun gteqp (val xs)
  (if (endp xs) t
    (and (<= val (car xs))
         (gteqp val (cdr xs)))))

(defthm quick-lteq-lteqp
        (implies (<= b a)
                 (lteqp a (quick-lteq b c))))

(defthm quick-gt-lteqp
        (implies (<= b a)
                 (equal (lteqp a (quick-gt b c))
                        (lteqp a c))))

(defthm quick-lteq-gteqp
        (implies (<= a b)
                 (gteqp a (quick-gt b c))))

(defthm quick-gt-gteqp
        (implies (<= a b)
                 (equal (gteqp a (quick-lteq b c))
                        (gteqp a c))))

(defthm lteqp-quicksort
        (equal (lteqp a (quicksort xs))
               (lteqp a xs)))

(defthm gteqp-quicksort
        (equal (gteqp a (quicksort xs))
               (gteqp a xs)))

(defthm sortedp-append
        (equal (sortedp (append a (cons b c)))
               (and (lteqp b a) (gteqp b c) (sortedp a) (sortedp c))))

(defthm sortedp-quicksort
        (sortedp (quicksort xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prove permutation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defthm occurrences-append
        (equal (occurrences a (append b c))
               (+ (occurrences a b) (occurrences a c))))

(defthm occurrences-partition
        (equal (+ (occurrences a (quick-gt b c))
                  (occurrences a (quick-lteq b c)))
               (occurrences a c)))

(defthm occurrences-partition-plus
        (equal (+ (occurrences a (quick-lteq b c))
                  d
                  (occurrences a (quick-gt b c)))
               (+ d (occurrences a c))))

(defthm occurrences-quicksort
        (equal (occurrences a (quicksort xs))
               (occurrences a xs)))
