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
;(defthm wlv-collect-once
;        (implies (and (true-listp b) (not (dupsp b)))
;                 (equal (while-loop-version a b)
;                        (collect-once (append (rev a) b)))))
;
;Subgoal *1/4.2
;(IMPLIES (AND (CONSP A)
;              (MEMBER-EQUAL (CAR A) B)
;              (EQUAL (WHILE-LOOP-VERSION (CDR A) B)
;                     (COLLECT-ONCE (APPEND (REV (CDR A)) B)))
;              (TRUE-LISTP B)
;              (NOT (DUPSP B)))
;         (EQUAL (WHILE-LOOP-VERSION (CDR A) B)
;                (COLLECT-ONCE (APPEND (APPEND (REV (CDR A)) (LIST (CAR A)))
;                                      B))))
;
;Subgoal *1/4.1
;(IMPLIES (AND (CONSP A)
;              (NOT (MEMBER-EQUAL (CAR A) B))
;              (EQUAL (WHILE-LOOP-VERSION (CDR A)
;                                         (CONS (CAR A) B))
;                     (COLLECT-ONCE (APPEND (REV (CDR A))
;                                           (CONS (CAR A) B))))
;              (TRUE-LISTP B)
;              (NOT (DUPSP B)))
;         (EQUAL (WHILE-LOOP-VERSION (CDR A)
;                                    (CONS (CAR A) B))
;                (COLLECT-ONCE (APPEND (APPEND (REV (CDR A)) (LIST (CAR A)))
;                                      B))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defthm append-associative
        (equal (append (append a b) c) (append a (append b c))))

;(defthm wlv-collect-once
;        (implies (and (true-listp b) (not (dupsp b)))
;                 (equal (while-loop-version a b)
;                        (collect-once (append (rev a) b)))))
;
;Subgoal *1/4''
;(IMPLIES (AND (CONSP A)
;              (MEMBER-EQUAL (CAR A) B)
;              (EQUAL (WHILE-LOOP-VERSION (CDR A) B)
;                     (COLLECT-ONCE (APPEND (REV (CDR A)) B)))
;              (TRUE-LISTP B)
;              (NOT (DUPSP B)))
;         (EQUAL (WHILE-LOOP-VERSION (CDR A) B)
;                (COLLECT-ONCE (APPEND (REV (CDR A))
;                                      (CONS (CAR A) B)))))

;; looks like a dead end, so try to find a way to redirect rewriting


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitional insight:
;(consp a) implies that
;(collect-once (append a b)) ==>
;(collect-once (cons (car a) (append (cdr a) b))) ==>
;(if (member (car a) (append (cdr a) b))  ;; notice this
;    (collect-once (append (cdr a) b))
;    (cons (car a) (collect-once (append (cdr a) b))))
;; so try this:
(defthm member-append
        (iff (member a (append b c))
             (or (member a b) (member a c))))

(defthm wlv-collect-once
        (implies (and (true-listp b) (not (dupsp b)))
                 (equal (while-loop-version a b)
                        (collect-once (append (rev a) b)))))

(defthm main-theorem-1-about-while-loop-version
        (subsetp (while-loop-version x nil) x))

(defthm main-theorem-2-about-while-loop-version
        (not (dupsp (while-loop-version x nil))))
