{-# LANGUAGE NoMonomorphismRestriction, DoRec #-}
module GraphTerm where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Control.Monad.State
import Data.Graph.Inductive.Query.Monad ((><))

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
cfs_index (CSNat nats) (CNat nat) = _cfs_elemIndex nat nats
cfs_index (CSInt ints) (CInt int) = _cfs_elemIndex int ints
cfs_index (CSSym (uid0, names)) (CSym (uid1, name)) =
  if uid0 == uid1 then _cfs_elemIndex name names
    else Left "cfs_index: mismatching symbol family"
cfs_index _ _ = Left "cfs_index: mismatching constant and set type"
_cfs_elemIndex elem elems = Right . CNat $ toInteger idx
    where default_idx = length elems
          idx = fromMaybe default_idx $ elemIndex elem elems

-- TODO
-- TupleWrite and test
-- recursion-friendly pretty printing
-- switch to zipper contexts
--   small-step
--   generic context over subterms that are: Either still-a-term already-a-value
--     allows ad-hoc eval order (think user interaction)
--     any fixed eval order can also be defined
--       maintain and traverse a sequence of get/put functions over a context's remaining 'still-a-term' subterms
-- eval/substitute at arbitrary term positions
-- define evaluation orders as zipper traversals
-- try to simplify EvalCtrl
-- const/mutable regions?
-- need garbage collection of memory store?

-- NOTES
-- (ordered) (sub)sets of: nats; ints; symbols
--   unique ids for sets so tags can be distinguished/unforgeable
--   layering/association of ordered sets over nats to describe records on top of tuples
-- tuples:
--   allocation
--     indicate allocations performed in a mutable region: (mutable expr-that-allocates)
--       'mutable' means that mutability can be observed from a distance; implies sharing
--       linear/unshared tuples can be modified without having been allocated in a mutable region
--         allows efficient initialization dynamically-sized of yet-to-be-shared 'constant' tuples
data ValueT term env value1 value2 =
    Lam term env
  | Tuple [value1]
  | Const Constant
  | ConstFinSet ConstFiniteSet
  | Tagged Constant value2
  | Undefined String
  deriving (Show, Eq)
data TermT term = Value (ValueT term () term term)
                | Var Name
                | LetRec [term] term
                | App term term
                | TupleAlloc term
                | TupleRead term term
                | TupleWrite term term term
                | ConstFinSetIndex term term
                | TaggedGetConst term
                | TaggedGetPayload term
  deriving (Show, Eq)

data EvalCtrl a b c d e f g h = EvalCtrl
  { ctrl_eval :: a
  , ctrl_wrap :: b
  , ctrl_unwrap :: c
  , ctrl_env_lookup :: d
  , ctrl_env_extend :: e
  , ctrl_store_lookup :: f
  , ctrl_store_extend :: g
  , ctrl_store_update :: h }

evalT ctrl term env = do
  result <- evT term
  return $ case result of
    Left msg -> Undefined msg
    Right val -> val
  where
    eval = ctrl_eval ctrl
    wrap = ctrl_wrap ctrl
    unwrap = ctrl_unwrap ctrl
    evalUnwrap term = liftM unwrap $ eval term env
    env_lookup = ctrl_env_lookup ctrl
    env_extend = ctrl_env_extend ctrl
    store_deref address = do
      store <- get
      return $ fromMaybe undef $ ctrl_store_lookup ctrl store address
      where undef = wrap $ Undefined "dereferenced invalid address"
    store_allocate values = do
      store <- get
      let (store', addresses) = ctrl_store_extend ctrl store values
      put store'
      return addresses
    store_assign address value = do
      store <- get
      case ctrl_store_update ctrl store address value of
        Nothing -> return . Right $ Undefined "assigned value to invalid address"
        Just store' -> put store' >> (return . Right $ Tuple [])

    apply proc arg = case unwrap proc of
      Lam body penv -> liftM (Right . unwrap) $ eval body env'
        where env' = env_extend penv arg
      otherwise -> return $ Left "expected Lam"

    untag ttagged = do
      tagged <- eval ttagged env
      return $ case unwrap tagged of
        Tagged const payload -> Right (const, payload)
        otherwise -> Left "expected Tagged"

    construct (Lam body ()) = return $ Lam body env
    construct (Tuple vals) =
      liftM Tuple $ store_allocate =<< mapM (`eval` env) vals
    construct (Const const) = return $ Const const
    construct (ConstFinSet cfs) = return $ ConstFinSet cfs
    construct (Tagged const val) = liftM (Tagged const) $ eval val env
    construct (Undefined description) = return $ Undefined description

    asTup (Tuple vals) = Right vals
    asTup _ = Left "expected Tuple"
    evalTup = liftM asTup . evalUnwrap

    asConst (Const const) = Right const
    asConst _ = Left "expected Const"

    asNat val = do
      cnat <- asConst val
      case cnat of
        CNat nat -> Right $ fromInteger nat
        otherwise -> Left "expected Nat"
    evalNat = liftM asNat . evalUnwrap

    asCfs (ConstFinSet cfs) = Right cfs
    asCfs _ = Left "expected ConstFinSet"

    evT (Value val) = liftM Right $ construct val
    evT (Var name) = return . Right $ env_lookup env name
    evT (LetRec bindings body) = do
      rec env' <- liftM (foldl env_extend env) $ mapM (`eval` env') bindings
      liftM (Right . unwrap) $ eval body env'
    evT (App tproc targ) = do
      proc <- eval tproc env
      arg <- eval targ env
      apply proc arg
    evT (TupleAlloc tsize) = do
      esize <- evalNat tsize
      case esize of
        Left msg -> return $ Left msg
        Right size ->
          liftM (Right . Tuple) $ store_allocate $ replicate size undef
      where undef = wrap $ Undefined "TupleAlloc: uninitialized slot"
    evT (TupleRead ttup tidx) = do
      etup <- evalTup ttup
      eidx <- evalNat tidx
      case (do
        tup <- etup
        idx <- eidx
        if idx < length tup then Right $ tup !! idx
          else Left "TupleRead: index out of bounds") of
        Left msg -> return $ Left msg
        Right address -> liftM (Right . unwrap) $ store_deref address
    evT (TupleWrite ttup tidx targ) = do
      etup <- evalTup ttup
      eidx <- evalNat tidx
      arg <- eval targ env
      case (do
        tup <- etup
        idx <- eidx
        if idx < length tup then Right (tup !! idx, arg)
          else Left "TupleWrite: index out of bounds") of
        Left msg -> return $ Left msg
        Right (address, arg) -> store_assign address arg
    evT (ConstFinSetIndex tcfs tconst) = do
      vcfs <- evalUnwrap tcfs
      vconst <- evalUnwrap tconst
      return (do
        cfs <- asCfs vcfs
        const <- asConst vconst
        liftM Const $ cfs_index cfs const)
    evT (TaggedGetConst ttagged) = withTagged (Const . fst) ttagged
    evT (TaggedGetPayload ttagged) = withTagged (unwrap . snd) ttagged

    withTagged f = liftM (liftM f) . untag

----------------------------------------------------------------
-- Simple guiding example
----------------------------------------------------------------
newtype SimpleTerm = SimpleTerm { simple_term :: TermT SimpleTerm }
  deriving (Show, Eq)
newtype SimpleValue = SimpleValue {
  simple_value :: ValueT SimpleTerm SimpleEnv Address SimpleValue }
  deriving (Show, Eq)
newtype SimpleEnv = SimpleEnv [SimpleValue]
  deriving (Show, Eq)
data SimpleStore = SimpleStore
  { sstore_values :: M.Map Address SimpleValue
  , sstore_next :: Address }
  deriving (Show, Eq)
senv_lookup (SimpleEnv vals) name = simple_value $ vals !! name
senv_extend (SimpleEnv vals) val = SimpleEnv $ val : vals
sstore_empty = SimpleStore M.empty 0
sstore_lookup store address = M.lookup address $ sstore_values store
sstore_extend store vals = (store', addresses)
  where current = sstore_next store
        next = current + length vals
        addresses = [current .. next - 1]
        assocs = zip addresses vals
        values' = foldr (uncurry M.insert) (sstore_values store) assocs
        store' = store {sstore_values = values', sstore_next = next}
sstore_update store address val =
  if M.member address values then Just store' else Nothing
  where values = sstore_values store
        store' = store { sstore_values = M.insert address val values }

simple_ctrl =
  EvalCtrl simple_eval SimpleValue simple_value
  senv_lookup senv_extend
  sstore_lookup sstore_extend sstore_update
simple_eval (SimpleTerm term) env = do
  value <- evalT simple_ctrl term env
  return $ SimpleValue value

app tp ta = SimpleTerm $ App tp ta
lam body = SimpleTerm . Value $ Lam body ()
var = SimpleTerm . Var
value = SimpleTerm . Value
tuple = value . Tuple
tupalloc sz = SimpleTerm $ TupleAlloc sz
tupread tup idx = SimpleTerm $ TupleRead tup idx
constant = value . Const
cnat = constant . CNat
cfsidx cfs const = SimpleTerm $ ConstFinSetIndex cfs const

-- TODO: recursion-friendly pretty-printing
test_recfunc0 = lam $ var 0
test_recfunc1 = lam $ app (var 3) $ cnat 64
test_recfunc2 = lam $ app (var 2) $ var 0
test_letrec = SimpleTerm $ LetRec [test_recfunc0, test_recfunc1, test_recfunc2] $ app (var 0) $ cnat 72

test_sym = constant . CSym $ ("global", "two")
test_cfs = value . ConstFinSet $ CSSym ("global", ["one", "two", "three"])
test_tup0 = tuple [cnat 0, cnat 1, cnat 2, cnat 3, cnat 4, cnat 5, cnat 6]
test_tup1 = tuple [cnat 7, cnat 8, cnat 9, cnat 10, cnat 11, cnat 12]
test_term = tuple [cnat 4,
                   app (lam $ var 0) (lam $ lam $ var 1),
                   tupread (tuple [cnat 11, cnat 421]) (cnat 1),
                   tupalloc (cnat 2),
                   cfsidx test_cfs test_sym,
                   test_letrec]
test = runState (simple_eval test_term $ SimpleEnv []) sstore_empty

----------------------------------------------------------------
-- Somewhat more heavy-duty approach
----------------------------------------------------------------
-- TODO: where to track open binders?
type Labeled term = ((), term)
type Addressed term = Either Address term
newtype ALTerm = ALTerm (Addressed (Labeled (TermT ALTerm)))
  deriving (Show, Eq)
