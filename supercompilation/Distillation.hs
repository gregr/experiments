module Language where

-- Based on Geoff Hamilton's paper on Distillation
-- The following has barely been started.

type Name = Int
type Pattern = (Name, [Name])
type Branch = (Pattern, Term)
data Term =
  Var Name | Constr Name [Term] | Lam Name Term | App Term Term |
  Case Term [Branch]
type Prog = (Term, [(Name, (Name, Term))])

nil = Constr "Nil" []
nilp = ("Nil", [])
cons a as = Constr "Cons" [a, as]
consp a as = ("Cons", [a, as])

fig2 =
  (App (Var "nrev") (Var "xs"),
  [
  ("nrev", Lam "xs" $ Case (Var "xs")
              [(nilp, nil),
              (cons "x'" "xs'", App (App (Var "app") (App (Var "nrev") (Var "xs'"))) (cons (Var "x'") nil))]),
  ("app", Lam "xs" $ Lam "ys" $ Case (Var "xs")
              [(nilp, Var "ys"),
              (consp "x'" "xs'", cons (Var "x'") (App (Var "xs'") (Var "ys")))])
  ])

drive_sc term =
generalize_sc term =
fold_sc term =

psc = fold_sc . generalize_sc . drive_sc

{-data Redex =-}
{-f-}
{-(λv .e0 ) e1-}
{-case (v e1 . . . en ) of p1 ⇒ e1 | · · · | pk ⇒ ek-}
{-case (c e1 . . . en ) of p1 ⇒ e1 | · · · | pk ⇒ ek-}


data Frame = ApplyTo Term | Select [Branch]
type Context = [Frame]

{-data Observable =-}
{-obs ::= v e1 . . . en-}
{-| c e1 . . . en-}
{-| λv .e-}


{-DS [[v e1 . . . en ]]-}
{-= v e1 . . . en → DS [[e1 ]], . . . , DS [[en ]]-}
{-DS [[c e1 . . . en ]]-}
{-= c e1 . . . en → DS [[e1 ]], . . . , DS [[en ]]-}
{-DS [[λv .e]]-}
{-= λv .e → DS [[e]]-}
{-DS [[con f ]]-}
{-= con f → DS [[con unfold f ]]-}
{-DS [[con (λv .e0 ) e1 ]] = con (λv .e0 ) e1 → DS [[con e0 [e1 /v ] ]]-}
{-DS [[con case (v e1 . . . en ) of p1 ⇒ e1 | · · · | pk ⇒ ek ]]-}
{-= con case (v e1 . . . en ) of p1 ⇒ e1 | · · · | pk ⇒ ek →-}
{-DS [[v e1 . . . en ]], DS [[e1 [p1 /v e1 . . . en ]]], . . .,DS [[ek [pk /v e1 . . . en ]]]-}
{-DS [[con case (c e1 . . . en ) of p1 ⇒ e1 | · · · | pk ⇒ ek ]]-}
{-= con case (c e1 . . . en ) of p1 ⇒ e1 | · · · | pk ⇒ ek →-}
{-DS [[con ei [e1 /v1 , . . . , en /vn ] ]]-}
{-where pi = c v1 . . . vn-}

{--}
{-n-}
{-n-}
{-g-}
{- (φ(e1 , . . . , eg ), i=1 θi , i=1 θi ), if e e e-}
{-n-}
{--}
{--}
{- where e = φ(e1 , . . . , en )-}
{--}
{-e = φ(e1 , . . . , en )-}
{-e ee =-}
{--}
{--}
{-(eg , θi , θi ) = ei e ei-}
{--}
{-i-}
{--}
{--}
{-(v, [e/v], [e /v]),-}


{-GS [[β = con f → t ]] =-}
{-[ei /ei ]-}
{-con f-}
{-α, if ∃α ∈ anc(t, β).t(α)-}
{-con f → GS [[t ]], otherwise-}
{-where-}
{-t(α) e t(β) = (eg , [ei /vi ], [ei /vi ])-}
{-e-}
{-t(β)-}
{-GS [[e → t1 , . . . , tn ]] = e → GS [[t1 ]], . . . , GS [[tn ]]-}

{-FS [[e-}
{-θ-}
{-55-}
{-θ-}
{-α]]-}
{-=-}
{-e-}
{-α,-}
{-if is-sub(θ)-}
  {-t{α := S[[abstracte (t(α), e)]]}, otherwise-}
  {-FS [[e → t1 , . . . , tn ]] = e → FS [[t1 ]], . . . , FS [[tn ]]-}

generalize_d =
fold_d = .. in terms of generalize_d

distill = fold_d . drive_sc

{--}
{-n-}
{-n-}
{-g-}
{-g-}
{- (e → t1 , . . . , tn , i=1 θi , i=1 θi ), if t t t-}
{--}
{--}
{- where t = e → t1 , . . . , tn-}
{--}
{--}
{--}
{-t = e → t1 , . . . , tn-}
{-t tt =-}
{-(tg , θi , θi ) = ti t ti-}
{--}
{-i-}
{--}
{--}
{- (DS [[eg ]], θ, θ ),-}
{-otherwise-}
{--}
{--}
{--}
{-where (eg , θ, θ ) = root(t) e root(t )-}

{-GD [[β = con f → t ]] =-}
{-[ei /ei ]-}
{-con f-}
{-α, if ∃α ∈ anc(t, β).t(α)-}
{-con f → GD [[t ]], otherwise-}
{-where-}
{-t(α) e t(β) = (eg , [ei /vi ], [ei /vi ])-}
{-e-}
{-t(β)-}
{-GD [[e → t1 , . . . , tn ]] = e → GD [[t1 ]], . . . , GD [[tn ]]-}

{--}
{-θ-}
{- con f-}
{-α,-}
{-if ∃α ∈ anc(t, β).GD [[α]] θ GD [[β]]-}
  {--}
  {--}
  {-t{α := FD [[abstractt (α, β)]]},-}
  {-FD [[β = con f → t ]] =-}
  {--}
  {-if ∃α ∈ anc(t, β).GD [[α]] t GD [[β]]-}
    {--}
    {--}
    {-con f → FD [[t ]], otherwise-}
    {-FD [[e → t1 , . . . , tn ]] = e → FD [[t1 ]], . . . , FD [[tn ]]-}


{-C[[(v e1 . . . en ) → t1 , . . . , tn ]] φ-}
{-C[[(c e1 . . . en ) → t1 , . . . , tn ]] φ-}
{-C[[(λv .e) → t]] φ-}
{-C[[(con f ) → t]] φ-}
{-C[[(con f )-}
{-θ-}
{-t]] φ-}
{-=-}
{-=-}
{-=-}
{-=-}
{-v (C[[t1 ]] φ) . . . (C[[tn ]] φ)-}
{-c (C[[t1 ]] φ) . . . (C[[tn ]] φ)-}
{-λv .(C[[t]] φ)-}
{-f v1 . . . vn-}
{-where-}
{-f = λv1 . . . vn .C[[t]] (φ ∪ {f v1 . . . vn = con f → t})-}
{-{v1 . . . vn } = f v(t)-}
{-= (f v1 . . . vn ) θ-}
{-where-}
{-(f v1 . . . vn = t) ∈ φ-}
{-C[[(con case (v e1 . . . en ) of p1 ⇒ e1 | · · · | pn ⇒ en ) → t0 , . . . , tn ]] φ-}
{-= case (C[[t0 ]] φ) of p1 ⇒ C[[t1 ]] φ | · · · | pn ⇒ C[[tn ]] φ-}
{-C[[let v1 = t1 , . . . , vn = tn in t]] φ-}
{-= (C[[t]] φ)[(C[[t1 ]] φ)/v1 , . . . , (C[[tn ]] φ)/vn ]-}

