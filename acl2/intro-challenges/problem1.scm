(defun rev (x)
  (if (endp x)
      nil
      (append (rev (cdr x)) (list (car x)))))

;Subgoal *1/2''
;(IMPLIES (AND (CONSP X)
              ;(EQUAL (REV (REV (REV (CDR X))))
                     ;(REV (CDR X))))
         ;(EQUAL (REV (REV (APPEND (REV (CDR X)) (LIST (CAR X)))))
                ;(APPEND (REV (CDR X)) (LIST (CAR X)))))

(defthm double-rev
        (implies (true-listp x)
                 (equal (rev (rev x))
                        x)))


(thm (equal (rev (rev (rev x))) (rev x)))
