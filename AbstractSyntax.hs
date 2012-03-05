module AbstractSyntax where

import Data.Map as M qualified
import Data.Set as S qualified

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


-- new idea for stratified kind/type/value/repr via unified representation
-- todo: maybe start without refinements to experiment with something simpler
-- todo: environments need to carry constraints (such as Int <: x, which aren't natural binding forms like x <: Int)
-- todo: free vs. bound 'type' variables; renaming only bound vars during environment linking? such as the 'a' in 'a -> List a'
--      might be better to call bound vars non-free, since they may not have explicit bounds
--      free vars are those directly associated with function params, or other free term names within a term body

type VSet = S.Set Value
type RSet = S.Set RefinedDatum

type VIntersection = VSet

data VUnion = PosSet RSet -- empty is bottom
            | NegSet RSet -- empty is top

data Variance = Covariant | Contravariant | Invariant
-- instead of variance, negative types? hmm that seems gross, and doesn't allow for invariance

data Typing = Typing Value Env

data ConstrArrow = ConstrArrow [(Variance, Value)] Datum

-- todo: these need to carry around environments? do datums? maybe not since they can always be linked to the current context
-- todo: these and datums also need to specify implicit args (constraint satisfiers)
--    envs should identify these components that provide constraint satisfaction, so these can be propagated upon lookup
data Constr = Constr { tag :: Tag, constrArrow :: ConstrArrow, constrEnv :: Env }
  -- unifying paramTypes that are vars should instantiate new vars in Env

data Datum = Datum Constr [Value] -- type of Datum is its constructor's [partially] evaluated arrow, given these args
           | Infinity Integer -- allow for endless stratification, type is greater Infinity

data RefinedDatum = RefinedDatum Datum RSet

data Value = VUnion VUnion -- todo: is this necessary given Env's implicit union? maybe for efficiency
           | VVar Name

type BindingMap = M.Map Name VIntersection

-- todo: more efficient representation for variable lookup
data Env = Env [BindingMap] -- represent unions of entangled vars / universes

