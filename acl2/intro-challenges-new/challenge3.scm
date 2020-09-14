(defun rev (x)
  (if (endp x)
    nil
    (append (rev (cdr x)) (list (car x)))))

(defun dupsp (x)  ; does x contain duplicate elements?
  (if (endp x)
    nil
    (if (member (car x) (cdr x))
      t
      (dupsp (cdr x)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defthm dupsp-rev (equal (dupsp (rev x)) (dupsp x)))
;
;Subgoal *1/3''
;(IMPLIES (AND (CONSP X)
;              (NOT (MEMBER-EQUAL (CAR X) (CDR X)))
;              (EQUAL (DUPSP (REV (CDR X)))
;                     (DUPSP (CDR X))))
;         (EQUAL (DUPSP (APPEND (REV (CDR X)) (LIST (CAR X))))
;                (DUPSP (CDR X))))
;
;Subgoal *1/2''
;(IMPLIES (AND (CONSP X)
;              (MEMBER-EQUAL (CAR X) (CDR X)))
;         (DUPSP (APPEND (REV (CDR X)) (LIST (CAR X)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defthm dupsp-append-not-member
;        (implies (not (member a b))
;                 (equal (dupsp (append b (list a)))
;                        (dupsp b))))
;
;Subgoal *1/4'''
;(IMPLIES (AND (CONSP B)
;              (NOT (MEMBER-EQUAL (CAR B) (CDR B)))
;              (NOT (DUPSP (APPEND (CDR B) (LIST A))))
;              (NOT (EQUAL A (CAR B)))
;              (NOT (MEMBER-EQUAL A (CDR B)))
;              (MEMBER-EQUAL (CAR B)
;                            (APPEND (CDR B) (LIST A))))
;         (DUPSP (CDR B)))
;
;Subgoal *1/2''
;(IMPLIES (AND (CONSP B)
;              (MEMBER-EQUAL (CAR B) (CDR B))
;              (NOT (EQUAL A (CAR B)))
;              (NOT (MEMBER-EQUAL A (CDR B)))
;              (NOT (MEMBER-EQUAL (CAR B)
;                                 (APPEND (CDR B) (LIST A)))))
;         (DUPSP (APPEND (CDR B) (LIST A))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; try this one later
;(equal (not (member a (append b c)))
       ;(and (not (member a b))
            ;(not (member a c))))

;; this didn't help
;(defthm dupsp-append
;        (equal (not (dupsp (append a b)))
;               (and (not (dupsp a))
;                    (not (dupsp b)))))
;
;Subgoal *1/3''
;(IMPLIES (AND (CONSP A)
;              (NOT (MEMBER-EQUAL (CAR A) (CDR A)))
;              (NOT (DUPSP (APPEND (CDR A) B)))
;              (NOT (DUPSP (CDR A)))
;              (NOT (DUPSP B)))
;         (NOT (MEMBER-EQUAL (CAR A)
;                            (APPEND (CDR A) B))))
;
;Subgoal *1/2''
;(IMPLIES (AND (CONSP A)
;              (MEMBER-EQUAL (CAR A) (CDR A))
;              (NOT (MEMBER-EQUAL (CAR A)
;                                 (APPEND (CDR A) B))))
;         (DUPSP (APPEND (CDR A) B)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fails to help, so negate both sides
;(defthm not-member-append
;        (equal (not (member a (append b c)))
;               (and (not (member a b))
;                    (not (member a c)))))

;;; nope, ACL2 tells us this is futile due to outer not
;;; make it propositional using iff instead so we can cancel nots
;(defthm member-append
;        (equal (not (not (member a (append b c))))
;               (or (not (not (member a b)))
;                   (not (not (member a c))))))

(defthm member-append
        (iff (member a (append b c))
             (or (member a b) (member a c))))

(defthm dupsp-append-not-member
        (implies (not (member a b))
                 (equal (dupsp (append b (list a)))
                        (dupsp b))))

;(defthm dupsp-rev (equal (dupsp (rev x)) (dupsp x)))
;
;Subgoal *1/3''
;(IMPLIES (AND (CONSP X)
;              (NOT (MEMBER-EQUAL (CAR X) (CDR X)))
;              (EQUAL (DUPSP (REV (CDR X)))
;                     (DUPSP (CDR X))))
;         (EQUAL (DUPSP (APPEND (REV (CDR X)) (LIST (CAR X))))
;                (DUPSP (CDR X))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defthm member-rev
        (iff (member a (rev b))
             (member a b)))

(defthm dupsp-rev (equal (dupsp (rev x)) (dupsp x)))
