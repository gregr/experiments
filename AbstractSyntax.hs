module AbstractSyntax where

type Name = String
type Index = Integer
type Binding val = (Name, val)
type Tag = Int

data Term' term = Var Name
                | Abstract [Name] term -- procedure
                | DefModule [Name] [Binding term]
                | Apply term [(Index, term)] [(Name, term)] -- eventually, args should be actual data tuple/record
                | LinkAsymm term term
                | LinkSymm term term -- though symmetric, one side gets precedence
                | Project term term term -- this should subsume typeclass dictionary lookup? final term is 'default value'?
                | DefVariant [Name] [Binding [Type]]
                | Construct Tag [term] -- should stratify, to handle type constructors as well? maybe not: different kinds
                | Destruct Tag term term -- (a -> b -> ... -> r) -> (Tag a b ...) -> r

type Attr = Int
data Term = Term (Term' Term) Attr

-- a type should be paired with its environment, as per principle typings
data Type' ty = TyVar Name
              | TyTop
              | TyBot
              | TyIntersection [ty]
              | TyUnion [ty]
              | TyConstruct TyTag [Binding ty] -- subsumes procedures; args are named
              | TyModule
              | TyRecord -- can this be subsumed by typeclass-like constraints? maybe record-type syntax is sugar for that?
              | TyLinkAsymm
              | TyLinkSymm
              | TyPromote -- bring term up to the type level
              | TyRefine -- specify a reduced set of values of some type

-- Constraint
-- Region
-- Effect
-- Closure?
-- Cost (time, space, etc.)

data Value' value = ty subtag datums
