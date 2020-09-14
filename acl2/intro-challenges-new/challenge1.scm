(defun rev (x)
  (if (endp x)
      nil
      (append (rev (cdr x)) (list (car x)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defthm triple-rev (equal (rev (rev (rev x))) (rev x)))
;
;Subgoal *1/2''
;(IMPLIES (AND (CONSP X)
;              (EQUAL (REV (REV (REV (CDR X))))
;                     (REV (CDR X))))
;         (EQUAL (REV (REV (APPEND (REV (CDR X)) (LIST (CAR X)))))
;                (APPEND (REV (CDR X)) (LIST (CAR X)))))

;; pairs of function symbols:
;; (rev (rev _)), or (rev (append _ _))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defthm double-rev (equal (rev (rev a)) a))
;
;Subgoal *1/2''
;(IMPLIES (AND (CONSP A)
;              (EQUAL (REV (REV (CDR A))) (CDR A)))
;         (EQUAL (REV (APPEND (REV (CDR A)) (LIST (CAR A))))
;                A))
;
;Subgoal *1/1''
;(IMPLIES (NOT (CONSP A)) (NOT A))

;; oops, it has to be a list


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defthm double-rev
        (implies (true-listp a)
                 (equal (rev (rev a))
                        a)))

(defthm triple-rev (equal (rev (rev (rev x))) (rev x)))
