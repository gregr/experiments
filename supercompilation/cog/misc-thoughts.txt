; TODO: the following rules need revision/simplification

; TODO: meta-vars? implications? quantification?

;(action-2 lam-apply (value (lam T)) (value V)) |-> (substitute 0 V T)  ; (reduction-lam-apply)
;(action-2 pair-access (value (bit (b-0))) (value (pair V0 V1))) |-> (value V0)  ; (reduction-pair-left)
;(action-2 pair-access (value (bit (b-1))) (value (pair V0 V1))) |-> (value V1)  ; (reduction-pair-right)

;T0 |-> T1
;---------  ; (step-reduction)
;T0 --> T1

                ;T0-0 --> T0-1
;---------------------------------------------  ; (step-left)
;(action-2 I T0-0 T1) --> (action-2 I T0-1 T1)

                       ;T1-0 --> T1-1
;-----------------------------------------------------------  ; (step-right)
;(action-2 I (value V) T1-0) --> (action-2 I (value V) T1-1)



;T0 --> T1
;---------  ; (skipstep-step)
;T0 ~~> T1

                ;T1-0 ~~> T1-1
;---------------------------------------------  ; (skipstep-right)
;(action-2 I T0 T1-0) ~~> (action-2 I T0 T1-1)

              ;T0 ~~> T1
;-------------------------------------  ; (skipstep-underlambda)
;(value (lam T0)) ~~> (value (lam T1))



;T -->* T  ; (step*-reflexivity)

;T --> U  U -->* V
;-----------------  ; (step*-step)
    ;T -->* V



;T -->* (value V)
;----------------  ; (bigstep)
;T ==> V



;T ~~>* T  ; (skipstep*-reflexivity)

;T0 ~~> T1
;----------  ; (skipstep*-skipstep)
;T0 ~~>* T1

;T ~~>* U  U ~~>* V
;------------------  ; (skipstep*-transitivity)
     ;T ~~>* V



;T ~~>* A  U ~~>* A
;------------------  ; (equiv-convergence)
      ;T ~= U

;(diverges T) (diverges U)
;-------------------------  ; (equiv-divergence)
          ;T ~= U



;T0 ~~>* T1  (diverges T1)
;-------------------------  ; (divergence-propagation)
     ;(diverges T0)

;; TODO: divergence rules for errors and non-termination


;; possible solution to all the pain?

;;meta-0:
;; f x = ...

;;meta-1: expected, f and arg are encoded meta-0 terms
;myprop arg =  ; forall arg.
  ;(implies (P arg)
           ;(Q `(lam-apply ,f ,arg)))

;;meta-2: qf, qmyprop, qhas-some-type, and qequal are encoded meta-1 terms
;myproof-of-myprop =
  ;let qarg = (new-term-metavar)  ; not sure how this should really look...
  ;let hyp = `(lam-apply ,qP ,qarg)
  ;let conclusion = `(lam-apply ,qQ (lam-apply ,qf ,qarg))

  ;for all meta-1 terms qarg
  ;either
    ;(diverges hyp)
  ;or the meta-1 program myprop can be shown to be pequal to (quote true)
    ;(pequal
      ;'true
      ;`(lam-apply ,qmyprop ,qarg))
  ;otherwise written as
    ;(pequal
      ;(quote true)
      ;(quote (if (unquote hyp)
              ;(unquote conclusion)
              ;(quote true))))

; myproof-of-myprop should be possible to write as a concrete proof, since it takes no parameters
; it will need to (inductively) consider all possible term cases for qarg
;   this suggests proof constructors for each type of metavar that combine proofs following all possible instantiations of such a metavar
;     proof of forall b. (P b) =
;       (all-bits-proof (proof of (P bit-0)) (proof of (P bit-1)))

; new question then: how to represent and justify use of such induction hypotheses? will it even be necessary? perhaps this will require meta-3 level reasoning, to inject fake IH proofs into the fray, available to then construct the forall proofs?
