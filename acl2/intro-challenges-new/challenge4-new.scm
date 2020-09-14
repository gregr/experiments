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

(defun collect-once (x)
  (if (endp x)
      nil
      (if (member (car x) (cdr x))
          (collect-once (cdr x))
          (cons (car x) (collect-once (cdr x))))))

(defun while-loop-version (x a)
  (if (endp x)
      a
      (while-loop-version (cdr x)
                          (if (member (car x) a)
                              a
                              (cons (car x) a)))))

(defun list-difference (x a)
  (if (endp x)
    nil
    (if (member (car x) a)
      (list-difference (cdr x) a)
      (cons (car x) (list-difference (cdr x) a)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defthm main-theorem-1-about-collect-once
        (subsetp (collect-once x) x))

;(defthm main-theorem-2-about-collect-once
;  (not (dupsp (collect-once x))))
;
;Subgoal *1/3''
;(IMPLIES (AND (CONSP X)
;              (NOT (MEMBER-EQUAL (CAR X) (CDR X)))
;              (NOT (DUPSP (COLLECT-ONCE (CDR X)))))
;         (NOT (MEMBER-EQUAL (CAR X)
;                            (COLLECT-ONCE (CDR X)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defthm member-collect-once
        (iff (member a (collect-once b))
             (member a b)))

(defthm main-theorem-2-about-collect-once
        (not (dupsp (collect-once x))))

(thm (subsetp x (collect-once x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; relating while-loop-version to collect-once
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; alternative
;(defthm while-loop-version-collect-once
;        (equal (while-loop-version x a)
;               (append (list-difference (collect-once (rev x)) a) a)))
;
;Subgoal *1/2.2
;(IMPLIES
; (AND (CONSP X)
;      (MEMBER-EQUAL (CAR X) A)
;      (EQUAL (WHILE-LOOP-VERSION (CDR X) A)
;             (APPEND (LIST-DIFFERENCE (COLLECT-ONCE (REV (CDR X)))
;                                      A)
;                     A)))
; (EQUAL
;   (WHILE-LOOP-VERSION (CDR X) A)
;   (APPEND
;        (LIST-DIFFERENCE (COLLECT-ONCE (APPEND (REV (CDR X)) (LIST (CAR X))))
;                         A)
;        A)))
;
;Subgoal *1/2.1
;(IMPLIES
; (AND (CONSP X)
;      (NOT (MEMBER-EQUAL (CAR X) A))
;      (EQUAL (WHILE-LOOP-VERSION (CDR X)
;                                 (CONS (CAR X) A))
;             (APPEND (LIST-DIFFERENCE (COLLECT-ONCE (REV (CDR X)))
;                                      (CONS (CAR X) A))
;                     (CONS (CAR X) A))))
; (EQUAL
;   (WHILE-LOOP-VERSION (CDR X)
;                       (CONS (CAR X) A))
;   (APPEND
;        (LIST-DIFFERENCE (COLLECT-ONCE (APPEND (REV (CDR X)) (LIST (CAR X))))
;                         A)
;        A)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; alternative
;(defthm while-loop-version-collect-once
;        (equal (while-loop-version x a)
;               (append (collect-once (rev (list-difference x a))) a)))
;
;Subgoal *1/3''
;(IMPLIES
;     (AND (CONSP X)
;          (NOT (MEMBER-EQUAL (CAR X) A))
;          (EQUAL (WHILE-LOOP-VERSION (CDR X) A)
;                 (APPEND (COLLECT-ONCE (REV (LIST-DIFFERENCE (CDR X) A)))
;                         A)))
;     (EQUAL (WHILE-LOOP-VERSION X A)
;            (APPEND (COLLECT-ONCE (APPEND (REV (LIST-DIFFERENCE (CDR X) A))
;                                          (LIST (CAR X))))
;                    A)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; chosen alternative (key checkpoints seem the most promising)
;(defthm while-loop-version-collect-once
;        (equal (while-loop-version x a)
;               (append (collect-once (list-difference (rev x) a)) a)))
;
;Subgoal *1/2.2
;(IMPLIES
; (AND (CONSP X)
;      (MEMBER-EQUAL (CAR X) A)
;      (EQUAL (WHILE-LOOP-VERSION (CDR X) A)
;             (APPEND (COLLECT-ONCE (LIST-DIFFERENCE (REV (CDR X)) A))
;                     A)))
; (EQUAL
;    (WHILE-LOOP-VERSION (CDR X) A)
;    (APPEND
;         (COLLECT-ONCE (LIST-DIFFERENCE (APPEND (REV (CDR X)) (LIST (CAR X)))
;                                        A))
;         A)))
;
;Subgoal *1/2.1
;(IMPLIES
; (AND (CONSP X)
;      (NOT (MEMBER-EQUAL (CAR X) A))
;      (EQUAL (WHILE-LOOP-VERSION (CDR X)
;                                 (CONS (CAR X) A))
;             (APPEND (COLLECT-ONCE (LIST-DIFFERENCE (REV (CDR X))
;                                                    (CONS (CAR X) A)))
;                     (CONS (CAR X) A))))
; (EQUAL
;    (WHILE-LOOP-VERSION (CDR X)
;                        (CONS (CAR X) A))
;    (APPEND
;         (COLLECT-ONCE (LIST-DIFFERENCE (APPEND (REV (CDR X)) (LIST (CAR X)))
;                                        A))
;         A)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defthm list-difference-append
        (equal (list-difference (append a b) c)
               (append (list-difference a c) (list-difference b c))))

;(defthm while-loop-version-collect-once
;        (equal (while-loop-version x a)
;               (append (collect-once (list-difference (rev x) a)) a)))
;
;Subgoal *1/2''
;(IMPLIES
;     (AND (CONSP X)
;          (NOT (MEMBER-EQUAL (CAR X) A))
;          (EQUAL (WHILE-LOOP-VERSION (CDR X)
;                                     (CONS (CAR X) A))
;                 (APPEND (COLLECT-ONCE (LIST-DIFFERENCE (REV (CDR X))
;                                                        (CONS (CAR X) A)))
;                         (CONS (CAR X) A))))
;     (EQUAL (WHILE-LOOP-VERSION (CDR X)
;                                (CONS (CAR X) A))
;            (APPEND (COLLECT-ONCE (APPEND (LIST-DIFFERENCE (REV (CDR X)) A)
;                                          (LIST (CAR X))))
;                    A)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defthm collect-once-append
;        (equal (collect-once (append a b))
;               (append (collect-once (list-difference a b))
;                       (collect-once b))))
;
;Subgoal *1/3.3'
;(IMPLIES (AND (CONSP A)
;              (NOT (MEMBER-EQUAL (CAR A) B))
;              (EQUAL (COLLECT-ONCE (APPEND (CDR A) B))
;                     (APPEND (COLLECT-ONCE (LIST-DIFFERENCE (CDR A) B))
;                             (COLLECT-ONCE B)))
;              (MEMBER-EQUAL (CAR A)
;                            (APPEND (CDR A) B)))
;         (MEMBER-EQUAL (CAR A)
;                       (LIST-DIFFERENCE (CDR A) B)))
;
;Subgoal *1/3.1'
;(IMPLIES (AND (CONSP A)
;              (NOT (MEMBER-EQUAL (CAR A) B))
;              (EQUAL (COLLECT-ONCE (APPEND (CDR A) B))
;                     (APPEND (COLLECT-ONCE (LIST-DIFFERENCE (CDR A) B))
;                             (COLLECT-ONCE B)))
;              (NOT (MEMBER-EQUAL (CAR A)
;                                 (APPEND (CDR A) B))))
;         (NOT (MEMBER-EQUAL (CAR A)
;                            (LIST-DIFFERENCE (CDR A) B))))
;
;Subgoal *1/2'''
;(IMPLIES (AND (CONSP A)
;              (MEMBER-EQUAL (CAR A) B)
;              (EQUAL (COLLECT-ONCE (APPEND (CDR A) B))
;                     (APPEND (COLLECT-ONCE (LIST-DIFFERENCE (CDR A) B))
;                             (COLLECT-ONCE B))))
;         (MEMBER-EQUAL (CAR A)
;                       (APPEND (CDR A) B)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defthm member-append
        (iff (member a (append b c))
             (or (member a b) (member a c))))

;(defthm collect-once-append
;        (equal (collect-once (append a b))
;               (append (collect-once (list-difference a b))
;                       (collect-once b))))
;
;Subgoal *1/3.3'
;(IMPLIES (AND (CONSP A)
;              (NOT (MEMBER-EQUAL (CAR A) B))
;              (EQUAL (COLLECT-ONCE (APPEND (CDR A) B))
;                     (APPEND (COLLECT-ONCE (LIST-DIFFERENCE (CDR A) B))
;                             (COLLECT-ONCE B)))
;              (MEMBER-EQUAL (CAR A) (CDR A)))
;         (MEMBER-EQUAL (CAR A)
;                       (LIST-DIFFERENCE (CDR A) B)))
;
;Subgoal *1/3.1'
;(IMPLIES (AND (CONSP A)
;              (NOT (MEMBER-EQUAL (CAR A) B))
;              (EQUAL (COLLECT-ONCE (APPEND (CDR A) B))
;                     (APPEND (COLLECT-ONCE (LIST-DIFFERENCE (CDR A) B))
;                             (COLLECT-ONCE B)))
;              (NOT (MEMBER-EQUAL (CAR A) (CDR A))))
;         (NOT (MEMBER-EQUAL (CAR A)
;                            (LIST-DIFFERENCE (CDR A) B))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defthm member-list-difference
        (iff (member a (list-difference b c))
             (and (member a b) (not (member a c)))))

(defthm collect-once-append
        (equal (collect-once (append a b))
               (append (collect-once (list-difference a b))
                       (collect-once b))))

(defthm while-loop-version-collect-once
        (equal (while-loop-version x a)
               (append (collect-once (list-difference (rev x) a)) a)))

(defthm main-theorem-1-about-while-loop-version
        (subsetp (while-loop-version x nil) x))

(defthm main-theorem-2-about-while-loop-version
        (not (dupsp (while-loop-version x nil))))
