;; based on: http://www.cs.utexas.edu/~boyer/wos.pdf
;;
;; for a sort procedure to be correct, the result must be both
;; 1. sorted
;; 2. a permutation of the original list

(defun merge-lists (l m)
  (declare (xargs :measure (+ (len l) (len m))))
  (if (endp l) m
    (if (endp m) l
      (if (< (car l) (car m))
        (cons (car l) (merge-lists (cdr l) m))
        (cons (car m) (merge-lists l (cdr m)))))))

(defun odds-list (xs)
  (if (endp xs) nil
    (cons (car xs) (odds-list (cddr xs)))))

(defthm mergesort-measure-helper
        (IMPLIES (AND (CONSP XS) (CONSP (CDR XS)))
                 (< (LEN (ODDS-LIST XS))
                    (+ 1 (LEN (CDR XS))))))

(defun mergesort (xs)
  (declare (xargs :measure (len xs)))
  (if (endp xs) nil
    (if (endp (cdr xs)) xs
      (merge-lists (mergesort (odds-list (cdr xs)))
                   (mergesort (odds-list xs))))))

(defun sortedp (xs)
  (if (endp xs) t
    (if (endp (cdr xs)) t
      (and (<= (car xs) (car (cdr xs)))
           (sortedp (cdr xs))))))

; if forall a. occurrences a (mergesort xs) == occurrences a xs
; then (mergesort xs) is a permutation of xs
(defun occurrences (a xs)
  (if (endp xs) 0
    (if (equal a (car xs))
      (+ 1 (occurrences a (cdr xs)))
      (occurrences a (cdr xs)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prove sorted
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defthm sortedp-mergesort (sortedp (mergesort xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prove permutation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defthm occurrences-merge-lists
        (equal (occurrences a (merge-lists b c))
               (+ (occurrences a b) (occurrences a c))))

(defthm occurrences-add-odds
        (equal (+ (occurrences a (odds-list b))
                  (occurrences a (odds-list (cdr b))))
               (occurrences a b)))

(defthm occurrences-mergesort
        (equal (occurrences a (mergesort xs))
               (occurrences a xs)))
