;Subgoal *1/2''
;(IMPLIES (AND (CONSP X)
              ;(SUBSETP (CDR X) (CDR X)))
         ;(SUBSETP (CDR X) X))

(defthm subset-bigger
        (implies (subsetp a b)
                 (subsetp a (cons x b))))


(thm (subsetp x x))
