(defun pnatp (a)
  (or (equal a '())
      (and (equal (car a) 's)
           (pnatp (cdr a)))))

(defun add (a b)
  (if (consp a)
    (cons (car a)
          (add (cdr a) b))
    b))

(defun mul (a b)
  (if (consp a)
    (add b
         (mul (cdr a) b))
    '()))

(thm (implies (pnatp a)
              (equal (mul '(s s) a)
                     (add a a))))

(defthm add-one-step-right-thm
  (implies (and (consp a) (pnatp a) (pnatp b))
           (equal (add b a)
                  (cons 's (add b (cdr a))))))

(thm (implies (pnatp a)
              (equal (mul a '(s s))
                     (add a a))))
