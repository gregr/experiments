;; Direct reverse is equivalent to accumulator-passing reverse.

(defun rev1 (xs)
  (if (endp xs) '()
    (append (rev1 (cdr xs)) (list (car xs)))))

(defun rev/acc (xs acc)
  (if (endp xs) acc
    (rev/acc (cdr xs) (cons (car xs) acc))))

(defun rev2 (xs) (rev/acc xs '()))

(defthm append-of-rev/acc (equal (append (rev/acc x y) z)
                                 (rev/acc x (append y z))))

(thm (equal (rev1 xs) (rev2 xs)))


;; Direct map composes with itself.

(defun map+1 (xs)
  (if (endp xs) '()
    (cons (+ 1 (car xs)) (map+1 (cdr xs)))))

(defun map*2 (xs)
  (if (endp xs) '()
    (cons (* 2 (car xs)) (map*2 (cdr xs)))))

(defun map*2+1 (xs)
  (if (endp xs) '()
    (cons (+ (* 2 (car xs)) 1) (map*2+1 (cdr xs)))))

(thm (equal (map+1 (map*2 xs)) (map*2+1 xs)))


;; Direct map is equivalent to accumulator-passing map.

(defun map*2+1/racc (xs acc)
  (if (endp xs) acc
    (map*2+1/racc (cdr xs) (cons (+ 1 (* 2 (car xs))) acc))))

(defun map*2+1/indirect (xs) (rev2 (map*2+1/racc xs '())))

(defthm rev/acc-of-append (equal (rev/acc (append y z) x)
                                 (rev/acc z (rev/acc y x))))

(defthm cons-of-rev/acc-of-map
        (equal (cons x (rev/acc (map*2+1/racc y z) '()))
               (rev/acc (map*2+1/racc y (append z (list x))) '())))

(thm (equal (map*2+1 xs) (map*2+1/indirect xs)))
