;; Shorthands
(pand*     A B) ==> (pnot* (por* (pnot* A) (pnot* B)))
(pimplies* A B) ==> (por* (pnot* A) B)


;; This definition won't be admissible in the usual way since the recursion is non-decreasing in rank.
(defun bad (x)
  (if (consp x)
    (bad x)
    5))

;; But it's possible to describe this partial function using a formula:
(pimplies* (pnot* (pequal* (consp X) t))
           (pequal* (bad X) 5))

;; Let's try to prove this formula under a hypothesis that includes the bad definition:
(pimplies* (pequal* (consp X) t)
           (pequal* (bad X) 5)

F: (pimplies* (pimplies* (pnot* (pequal* (consp X) t))
                         (pequal* (bad X) 5))
              (pimplies* (pequal* (consp X) t)
                         (pequal* (bad X) 5)))

m: (if (consp Y)
     (rank Y)
     0)

m: (if (consp X)
     (rank Y)
     0)

q1: (pnot* (pequal* (consp X) t))  sigmas1 = []
q2:        (pequal* (consp X) t)   sigmas2 = [{Y -> (cdr Y)}]


q1: (pnot* (pequal* (consp Y) t))  sigmas1 = []
q2:        (pequal* (consp Y) t)   sigmas2 = [{Y -> (cdr Y)}]




m: (rank Y)
q0: (pand* (pequal* (consp X) t)
           (pequal* (consp Y) t)) sigmas0 = [{Y -> (cdr Y)}]


(pimplies* (por* (pnot* (pequal* (consp X) t))
                 (pnot* (pequal* (consp Y) t)))
           F)
==>
(pimplies* (pnot* (pequal* (consp X) t))
           F)

;; this part of the exhaustiveness check cannot be proven
(pimplies* (pnot* (pequal* (consp Y) t))
           F)
