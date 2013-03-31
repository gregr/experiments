{-# LANGUAGE NoMonomorphismRestriction #-}
module GraphTerm where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Control.Monad.State
{-import Control.Monad.Error-}
{-import Control.Monad.Identity-}

newtype FreeName = FreeName Int
  deriving (Show, Eq, Ord)
newtype BoundName = BoundName Int
  deriving (Show, Eq, Ord)
bn_inc (BoundName idx) = BoundName $ idx + 1
bn_lift (BoundName idx) (BoundName target) =
  if idx >= target then idx + 1 else idx
bn_lower (BoundName idx) (BoundName target) =
  if idx == target then Nothing else
    Just $ BoundName (if idx > target then idx - 1 else idx)
-- TODO: lift to make room for a new lambda binding; does this operation make sense on bn?
-- bn_abstract ... =
bn_substitute idx target val =
  case bn_lower idx target of
    Nothing -> val
    Just bname -> BoundVar bname

data TermT term = FreeVar FreeName | BoundVar BoundName | Lam term | App term term
  deriving (Show, Eq)
-- TODO: term wrappers?
term_substitute recsub term target@(BoundName idx) val = tsub term
  where
    tsub (BoundVar bname) = bn_substitute bname target val
    tsub (Lam body) = Lam $ recsub body (BoundName $ idx + 1) val
    tsub (App proc arg) = App (recsub proc target val) (recsub arg target val)

data Graph term = Graph{gr_index :: M.Map FreeName term, gr_next :: FreeName}
  deriving (Show, Eq)
graph_new = Graph{gr_index = M.empty, gr_next = FreeName 0}
graph_insert term = do
  graph <- get
  let
    index = gr_index graph
    fnext = gr_next graph
    FreeName next = fnext
    index' = M.insert fnext term index
  put $ Graph{gr_index = index', gr_next = FreeName (next + 1)}
  return fnext

type GraphTerm = TermT FreeName
data BindingGraphTerm = BindingGraphTerm{bnd_term :: GraphTerm, bnd_deps :: S.Set BoundName}
type BindingGraph = Graph BindingGraphTerm

{-data Substitution term = Offset Int | Bind term-}
{---newtype Env term = Env (M.Map BoundName term)-}
{-newtype Env term = Env {env_subs :: [Substitution term], env_bindcount :: Int}-}
{-env_make bindings = Env {env_subs = subs, env_bindcount = length bindings}-}
  {-where subs =-}
{-type GraphEnv = Env FreeName-}

-- TODO: must rename/lift affected bound names while substituting
-- must increment env bound names when entering lambda bodies
-- gsub (bname, fname) bodyname graph =

-- maybe later; keep things simple
--newtype Binder = Binder {bnd_name :: Name, bnd_deps :: S.Set BoundName}
--type BoundTerm = TermT Binder Binder
--newtype BoundTerm = BoundTerm {bt_term :: TermT Name Name, bt_deps :: S.Set BoundName}
--type BoundGraph = Graph BoundTerm

--type FreeTerm = TermT FreeName Binder
--type FreeGraph = Graph FreeTerm

--type Env = (FreeGraph, BoundGraph)

{-apply lam arg = ...-}

-- eval term or eval name?
{-eval fname graph = evalf fterm env-}
  {-where-}
    {-evalf term@(Lam bterm) env = term-}
    {-evalf (App tlam targ) env = apply lam arg-}
      {-where lam = evalf tlam env-}
            {-arg = evalf targ env-}
    --evalb
