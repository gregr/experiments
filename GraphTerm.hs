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

data ValueT term = Lam term | Tuple [Name] -- TODO: constructors must be in ANF
  deriving (Show, Eq)
data TermT term = Value (ValueT term) | Var Name | LetRec [(Name, term)] term | App term term
  deriving (Show, Eq)

-- TODO: where to track open binders?
type Labeled term = ((), term)
type Addressed term = Either Address term
newtype ALTerm = ALTerm (Addressed (Labeled (TermT ALTerm)))
  deriving (Show, Eq)

newtype SimpleTerm = SimpleTerm (TermT SimpleTerm)
  deriving (Show, Eq)

data Env term = Env [(Env term, ValueT term)]
  deriving (Show, Eq)
env_lookup (Env vals) name = vals !! name
env_extend (Env vals) val = Env $ val : vals

-- TODO: evaluate with zipper context?

data EvalCtrl a c d = EvalCtrl { ctrl_eval :: a, ctrl_env_lookup :: c, ctrl_env_extend :: d }

applyT ctrl (Lam body) arg env = eval body env'
  where eval = ctrl_eval ctrl
        env' = ctrl_env_extend ctrl env arg
applyT _ _ _ _ = error "bad proc"

evalT ctrl (Value val) env = (env, val)
evalT ctrl (Var name) env = ctrl_env_lookup ctrl env name
evalT ctrl (App tproc targ) env = applyT ctrl proc arg penv
  where (penv, proc) = eval tproc env
        arg = eval targ env
        eval = ctrl_eval ctrl

simple_ctrl = EvalCtrl simple_eval env_lookup env_extend
simple_eval (SimpleTerm term) env = evalT simple_ctrl term env

app tp ta = SimpleTerm $ App tp ta
lam = SimpleTerm . Value . Lam
var = SimpleTerm . Var

test_term = (app (lam $ var 0) (lam $ lam $ var 1))
test = simple_eval test_term $ Env []
