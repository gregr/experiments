{-# LANGUAGE NoMonomorphismRestriction #-}
module GraphTerm where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Control.Monad.State
import Data.Graph.Inductive.Query.Monad ((><))
{-import Control.Monad.Error-}
{-import Control.Monad.Identity-}

-- TODO: will probably need this later for ad-hoc evaluation
{-bn_lift idx target =-}
  {-if idx >= target then idx + 1 else idx-}
{-bn_lower idx target =-}
  {-if idx == target then Nothing else-}
    {-Just (if idx > target then idx - 1 else idx)-}
{--- TODO: lift to make room for a new lambda binding; does this operation make sense on bn?-}
{--- bn_abstract ... =-}
{-bn_substitute idx target val =-}
  {-case bn_lower idx target of-}
    {-Nothing -> val-}
    {-Just bname -> Var bname-}
{--- TODO: term wrappers?-}
{-term_substitute rtsub term target val = tsub term-}
  {-where-}
    {-tsub (Var bname) = bn_substitute bname target val-}
    {-tsub (Lam body) = Lam $ rtsub body (target + 1) val-}
    {-tsub (App proc arg) = App (rtsub proc target val) (rtsub arg target val)-}

-- coinductive data dealt with by change in evaluation strategy to normal order
--   no need for explicit delay/force syntax (reserved for lower level languages?)
--   functions parameters are analyzed to see which can endanger termination
--   when termination is jeopardized by coinductively defined data, evaluation of
--   applications switches to normal order, thunking/delaying expressions
--     strictness analysis done to figure out when to force thunks before case scrutiny?

-- function cost analysis: per iteration/step, full undelayed process (may be infinite), parameterized by function parameters
-- possible nontermination as effect
-- termination analysis indicates dependency on the finiteness of certain parameters, maybe just in a sentence using AND and OR; failing this dependency indicates the function application needs to be delayed until forced (recursive calls will naturally do the same)
--   when operating dynamically, finding "delayed" expressions in these positions triggers further delay

type Address = Int
type Name = Int

type Nat = Integer
type SymUid = String
type SymName = String
type SymSet = (SymUid, [SymName])
type Symbol = (SymUid, SymName)

data ConstFiniteSet = CSNat [Nat] | CSInt [Integer] | CSSym SymSet
  deriving (Show, Eq)
data Constant = CNat Nat | CInt Integer | CSym Symbol
              | CInterpreted ConstFiniteSet Nat
  deriving (Show, Eq)

-- (ordered) (sub)sets of: nats; ints; symbols
--   unique ids for sets so tags can be distinguished/unforgeable
--   layering/association of ordered sets over nats to describe records on top of tuples
-- variable-sized tuple/array alloc, given initialization value
--   can be given abstract value (type) for size-only initialization
-- tuples:
--   allocation in mutable regions
--   read/write
--   recombine: form a new pre-tuple given [(tuple, range)]
--     this can be used to achieve forms of copying, concatenation and slicing
--   flatten: flatten the recombined pre-tuple into an actual tuple
--     this separation minimizes construction of new tuples from algebraic data
--     maybe there's a better name for flatten if subrange is alsy separated
--       something that sounds like execute, commit, or allocate?
--   maybe subrange should also be separated; recombine over other recombines or subranges
-- everything from recombination down needs rethinking
data ValueT term env value = Lam term env
                           | Tuple [value]
                           | Const Constant
                           | Tagged Constant value
  deriving (Show, Eq)
data TermT term = Value (ValueT term () term)
                | Var Name
                | LetRec [(Name, term)] term
                | App term term
                | TupleRecombine [(term, (term, term))]
                | TupleRead term term
                | TaggedGetConst term
                | TaggedGetPayload term
  deriving (Show, Eq)

-- TODO: evaluate with zipper context?
--   small-step
--   generic context over subterms that are: Either still-a-term already-a-value
--     allows ad-hoc eval order (think user interaction)
--     any fixed eval order can also be defined
--       maintain and traverse a sequence of get/put functions over a context's remaining 'still-a-term' subterms

data EvalCtrl a b c d = EvalCtrl { ctrl_eval :: a
                                 , ctrl_unwrap :: b
                                 , ctrl_env_lookup :: c
                                 , ctrl_env_extend :: d }

evalT ctrl term env = evT term
  where
    eval = ctrl_eval ctrl
    unwrap = ctrl_unwrap ctrl
    evalUnwrap = unwrap . (`eval` env)
    env_lookup = ctrl_env_lookup ctrl
    env_extend = ctrl_env_extend ctrl

    apply proc arg = case unwrap proc of
      Lam body penv -> unwrap $ eval body env'
        where env' = env_extend penv arg
      otherwise -> error "bad proc"

    untag ttagged = case unwrap $ eval ttagged env of
      Tagged const payload -> (const, payload)
      otherwise -> error "not a tagged value"

    construct (Lam body ()) = Lam body env
    construct (Tuple vals) = Tuple $ map (`eval` env) vals
    construct (Const const) = Const const
    construct (Tagged const val) = Tagged const $ eval val env

    asTup (Tuple vals) = vals
    asTup _ = error "not a Tuple"

    unConst (Const const) = const
    unConst _ = error "not a Const"

    asNat val = case unConst val of
      CNat nat -> fromInteger nat -- fromInteger needed for recombine take/drop
      otherwise -> error "not a Nat"

    evT (Value val) = construct val
    evT (Var name) = env_lookup env name
    evT (App tproc targ) = apply proc arg
      where proc = eval tproc env
            arg = eval targ env
    evT (TupleRecombine tslices) = Tuple . concat $ map chop slices
      where -- NOTE: end is not inclusive in range
        eu = evalUnwrap
        slices = map (eu >< (eu >< eu)) tslices
        chop (vtup, (vstart, vend)) = take (end - start) $ drop start tup
          where
            tup = asTup vtup
            start = asNat vstart
            end = asNat vend
    evT (TupleRead ttup tidx) =
      if idx < length tup then unwrap $ tup !! idx
        else error "Tuple index out of bounds"
      where tup = asTup $ evalUnwrap ttup
            idx = asNat $ evalUnwrap tidx
    evT (TaggedGetConst ttagged) = Const . fst $ untag ttagged
    evT (TaggedGetPayload ttagged) = unwrap . snd $ untag ttagged

----------------------------------------------------------------
-- Simple guiding example
----------------------------------------------------------------
newtype SimpleTerm = SimpleTerm { simple_term :: TermT SimpleTerm }
  deriving (Show, Eq)
newtype SimpleValue =
  SimpleValue { simple_value :: ValueT SimpleTerm SimpleEnv SimpleValue }
  deriving (Show, Eq)
newtype SimpleEnv = SimpleEnv [SimpleValue]
  deriving (Show, Eq)
simple_env_lookup (SimpleEnv vals) name = simple_value $ vals !! name
simple_env_extend (SimpleEnv vals) val = SimpleEnv $ val : vals

simple_ctrl =
  EvalCtrl simple_eval simple_value simple_env_lookup simple_env_extend
simple_eval (SimpleTerm term) env = SimpleValue $ evalT simple_ctrl term env

app tp ta = SimpleTerm $ App tp ta
lam body = SimpleTerm . Value $ Lam body ()
var = SimpleTerm . Var
tuple = SimpleTerm . Value . Tuple
tupread tup idx = SimpleTerm $ TupleRead tup idx
constant = SimpleTerm . Value . Const
recombine = SimpleTerm . TupleRecombine
cnat = constant . CNat

test_tup0 = tuple [cnat 0, cnat 1, cnat 2, cnat 3, cnat 4, cnat 5, cnat 6]
test_tup1 = tuple [cnat 7, cnat 8, cnat 9, cnat 10, cnat 11, cnat 12]
test_term = tuple [cnat 4,
                   app (lam $ var 0) (lam $ lam $ var 1),
                   recombine [(test_tup0, (cnat 2, cnat 6)),
                              (test_tup1, (cnat 1, cnat 4))],
                   tupread (tuple [cnat 11, cnat 421]) (cnat 1)]
test = simple_eval test_term $ SimpleEnv []

----------------------------------------------------------------
-- Somewhat more heavy-duty approach
----------------------------------------------------------------
-- TODO: where to track open binders?
type Labeled term = ((), term)
type Addressed term = Either Address term
newtype ALTerm = ALTerm (Addressed (Labeled (TermT ALTerm)))
  deriving (Show, Eq)
