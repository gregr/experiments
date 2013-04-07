{-# LANGUAGE NoMonomorphismRestriction #-}
module GraphTerm where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Control.Monad.State
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

data ValueT term env value = Lam term env | Tuple [value]
  deriving (Show, Eq)
data TermT term = Value (ValueT term () term) | Var Name | LetRec [(Name, term)] term | App term term
  deriving (Show, Eq)

-- TODO: evaluate with zipper context?

data EvalCtrl a b c d = EvalCtrl { ctrl_eval :: a, ctrl_apply :: b, ctrl_env_lookup :: c, ctrl_env_extend :: d }

applyT ctrl (Lam body env) arg = eval body env'
  where eval = ctrl_eval ctrl
        env' = ctrl_env_extend ctrl env arg
applyT _ _ _ = error "bad proc"

constructT ctrl (Lam body ()) env = Lam body env
constructT ctrl (Tuple vals) env = Tuple $ map (`eval` env) vals
  where eval = ctrl_eval ctrl

evalT ctrl (Value val) env = constructT ctrl val env
evalT ctrl (Var name) env = ctrl_env_lookup ctrl env name
evalT ctrl (App tproc targ) env = apply proc arg
  where proc = eval tproc env
        arg = eval targ env
        eval = ctrl_eval ctrl
        apply = ctrl_apply ctrl

----------------------------------------------------------------
-- Simple guiding example
----------------------------------------------------------------
newtype SimpleTerm = SimpleTerm { simple_term :: (TermT SimpleTerm) }
  deriving (Show, Eq)
newtype SimpleValue = SimpleValue { simple_value :: (ValueT SimpleTerm SimpleEnv SimpleValue) }
  deriving (Show, Eq)
newtype SimpleEnv = SimpleEnv [SimpleValue]
  deriving (Show, Eq)
simple_env_lookup (SimpleEnv vals) name = simple_value $ vals !! name
simple_env_extend (SimpleEnv vals) val = SimpleEnv $ val : vals

simple_ctrl = EvalCtrl simple_eval simple_apply simple_env_lookup simple_env_extend
simple_eval (SimpleTerm term) env = SimpleValue $ evalT simple_ctrl term env
simple_apply (SimpleValue proc) arg = simple_value $ applyT simple_ctrl proc arg

app tp ta = SimpleTerm $ App tp ta
lam body = SimpleTerm . Value $ Lam body ()
var = SimpleTerm . Var

test_term = (app (lam $ var 0) (lam $ lam $ var 1))
test = simple_eval test_term $ SimpleEnv []

----------------------------------------------------------------
-- Somewhat more heavy-duty approach
----------------------------------------------------------------
-- TODO: where to track open binders?
type Labeled term = ((), term)
type Addressed term = Either Address term
newtype ALTerm = ALTerm (Addressed (Labeled (TermT ALTerm)))
  deriving (Show, Eq)
