-- playing with the last AGDA termination checking example at:
-- http://wiki.portal.chalmers.se/agda/pmwiki.php?n=ReferenceManual.TerminationChecker

-- termination check fails for f and g in the following:

  h : (x y : Nat) → Nat
  h zero zero = zero
  h zero (suc y) = h zero y
  h (suc x) y = h x y

  mutual

  f : (x y : Nat) → Nat
  f zero z = zero
  f (suc x) zero = zero
  f (suc x) (suc y) = h (g x y) (f (suc (suc x)) y)

  g : (x y : Nat) → Nat
  g zero y = zero
  g (suc x) zero = zero
  g (suc x) (suc y) = h (f x y) (g x (suc (suc y)))

-- =======================================
-- re-encoding f and g as two cases of the same recursive function
  f : (p x y : Nat) → Nat
  f p zero z = zero
  f p (suc x) zero = zero
  f 1 (suc x) (suc y) = h (f 0 x y) (f 1 (suc (suc x)) y)
  f 0 (suc x) (suc y) = h (f 1 x y) (f 0 x (suc (suc y)))

-- this measure seems to work
(x + y), if inside-f then y else x

f -> f
f -> g -> f
f -> g -> g -> f

f -> f: [x + y, y] -> [x+1 + y-1, y-1]
f -> g -> f: [x + y, y] -> [x-1 + y-1, x-1] -> [x-2 + y-2, y-2]
f -> g -> g -> f: [x + y, y] -> [x-1 + y-1, x-1] -> [x-2 + y, x-2] -> [x-3 + y-1, y-1]

-- encode in ACL2, which is able to prove termination with this lexicographical measure encoded as an ordinal
(defun h (x y)
  (declare (xargs :measure (+ (len x) (len y))))
  (if (endp y) nil
    (if (endp x) (h x (cdr y))
      (h (cdr x) y))))

(defun f (p x y)
  (declare (xargs :measure (cons (cons 1 (+ 1 (len x) (len y))) (if p (len y) (len x)))))
  (if (endp x) nil
    (if (endp y) nil
      (if p
        (h (f nil (cdr x) (cdr y)) (f t (cons 0 x) (cdr y)))
        (h (f t (cdr x) (cdr y)) (f nil (cdr x) (cons 0 y)))))))
