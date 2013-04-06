;; My answer to this problem is several steps shorter than the given answer:
;; http://www.cs.utexas.edu/users/moore/acl2/v6-1/INTRODUCTORY-CHALLENGE-PROBLEM-4-ANSWER.html
;;
;; They involve a list-minus theorem in their theorem 'general-equivalence'.
;; I circumvent this in my theorem 'while-collect-equivalence'.

;(collect-once (list 1 2 3 2 4 1 5))
;(while-loop-version (list 1 2 3 2 4 1 5) nil)
;(while-loop-version (rev (list 1 2 3 2 4 1 5)) nil)

;(defun test (xs a) (equal (while-loop-version xs a) (collect-once (append (rev xs) a))))
;(test '(1 2 3 1 4 1 2 5) '(1 2 3))

(defun rev (xs)
  (if (endp xs)
    nil
    (append (rev (cdr xs)) (list (car xs)))))

(defun dupsp (x)  ; does x contain duplicate elements?
  (if (endp x)
      nil
      (if (member (car x) (cdr x))
          t
          (dupsp (cdr x)))))

(defun collect-once (xs)
  (if (endp xs)
    nil
    (if (member (car xs) (cdr xs))
      (collect-once (cdr xs))
      (cons (car xs) (collect-once (cdr xs))))))

(defun while-loop-version (xs acc)
  (if (endp xs) acc
    (if (member (car xs) acc) (while-loop-version (cdr xs) acc)
      (while-loop-version (cdr xs) (cons (car xs) acc)))))


; collect-once

(defthm subset-collect-once (subsetp (collect-once x) x))

(defthm member-collect-once
        (implies (not (member a b))
                 (not (member a (collect-once b)))))

(defthm dupsp-collect-once (not (dupsp (collect-once x))))


; while-loop-version direct

(defthm subset-bigger (implies (subsetp a b) (subsetp a (cons c b))))

(defthm subset-self (subsetp x x))

(defthm subset-while-loop-parts
        (implies (and (subsetp xs zs) (subsetp ys zs))
                 (subsetp (while-loop-version xs ys) zs)))

(defthm subset-while-loop-version (subsetp (while-loop-version x nil) x))

;

(defthm dupsp-while-loop-acc
  (equal (dupsp (while-loop-version xs ys))
         (dupsp ys)))

(defthm dupsp-while-loop-version (not (dupsp (while-loop-version x nil))))

; :ubt! 1
; then resubmit everything above not from while-loop-version direct
; while-loop-version related to collect-once

(defthm member-append (iff (member x (append a b)) (or (member x a) (member x b))))

(defthm append-associativity
        (equal (append (append a b) c)
               (append a (append b c))))

(defthm while-collect-equivalence
        (implies (and (true-listp a) (not (dupsp a)))
                 (equal (while-loop-version xs a)
                        (collect-once (append (rev xs) a)))))

(defthm subset-while-loop-version (subsetp (while-loop-version x nil) x))

;

(defthm dupsp-while-loop-version (not (dupsp (while-loop-version x nil))))
