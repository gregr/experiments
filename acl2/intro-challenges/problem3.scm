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

;Subgoal *1/4'''
;(IMPLIES (AND (CONSP B)
              ;(NOT (MEMBER (CAR B) (CDR B)))
              ;(NOT (DUPSP (APPEND (CDR B) (LIST A))))
              ;(NOT (EQUAL A (CAR B)))
              ;(NOT (MEMBER A (CDR B)))
              ;(MEMBER (CAR B)
                      ;(APPEND (CDR B) (LIST A))))
         ;(DUPSP (CDR B)))

(defthm nonmember-bigger
        (implies (and (not (member a b))
                      (not (equal a c)))
                 (not (member a (append b (list c))))))

;Subgoal *1/2''
;(IMPLIES (AND (CONSP B)
              ;(MEMBER (CAR B) (CDR B))
              ;(NOT (EQUAL A (CAR B)))
              ;(NOT (MEMBER A (CDR B)))
              ;(NOT (MEMBER (CAR B)
                           ;(APPEND (CDR B) (LIST A)))))
         ;(DUPSP (APPEND (CDR B) (LIST A))))

(defthm member-bigger
        (implies (member a b)
                 (member a (append b c))))

;Subgoal *1/3''
;(IMPLIES (AND (CONSP X)
              ;(NOT (MEMBER (CAR X) (CDR X)))
              ;(EQUAL (DUPSP (REV (CDR X)))
                     ;(DUPSP (CDR X))))
         ;(EQUAL (DUPSP (APPEND (REV (CDR X)) (LIST (CAR X))))
                ;(DUPSP (CDR X))))

(defthm member-rev
        (iff (member a (rev b))
             (member a b)))

(defthm nonmember-dupsp-smaller
        (implies (not (member a b))
                 (equal (dupsp (append b (list a)))
                        (dupsp b))))

;Subgoal *1/2''
;(IMPLIES (AND (CONSP X) (MEMBER (CAR X) (CDR X)))
         ;(DUPSP (APPEND (REV (CDR X)) (LIST (CAR X)))))

(defthm dupsp-appendcar
        (dupsp (append (cons a b) (list a))))

(defthm dupsp-appendmember
        (implies (member a b)
                 (dupsp (append b (list a)))))


(thm (equal (dupsp (rev x)) (dupsp x)))
