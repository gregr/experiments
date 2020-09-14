;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defthm subsetp-reflexive (subsetp x x))
;
;Subgoal *1/2''
;(IMPLIES (AND (CONSP X)
;              (SUBSETP-EQUAL (CDR X) (CDR X)))
;         (SUBSETP-EQUAL (CDR X) X))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defthm subsetp-bigger
;        (implies (subsetp a a)
;                 (subsetp a (cons b a))))
;
;Subgoal *1/3.2
;(IMPLIES (AND (CONSP A)
;              (NOT (EQUAL (CAR A) B))
;              (SUBSETP-EQUAL (CDR A) (CONS B (CDR A)))
;              (SUBSETP-EQUAL (CDR A) A))
;         (SUBSETP-EQUAL (CDR A) (CONS B A)))
;
;Subgoal *1/3.1
;(IMPLIES (AND (CONSP A)
;              (CONS B A)
;              (SUBSETP-EQUAL (CDR A) (CONS B (CDR A)))
;              (SUBSETP-EQUAL (CDR A) A))
;         (SUBSETP-EQUAL (CDR A) (CONS B A)))
;
;Subgoal *1/2.2
;(IMPLIES (AND (CONSP A)
;              (NOT (EQUAL (CAR A) B))
;              (NOT (SUBSETP-EQUAL (CDR A) (CDR A)))
;              (SUBSETP-EQUAL (CDR A) A))
;         (SUBSETP-EQUAL (CDR A) (CONS B A)))

;; maybe not general enough


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defthm subsetp-bigger
        (implies (subsetp a b)
                 (subsetp a (cons c b))))

(defthm subsetp-reflexive (subsetp x x))
