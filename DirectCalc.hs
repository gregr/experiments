{-# LANGUAGE NoMonomorphismRestriction, TupleSections #-}
module DirectCalc where

import qualified Data.Map as M
import Data.Maybe
import Control.Monad.State
import Control.Monad.Error

type Ident = String
type Arity = Int
type EnvKey = Arity
type Env = [Value]
data Name = Free Ident | Bound EnvKey
  deriving (Show, Eq, Ord)

data Term =
  Type | Arrow Term Term | Ann Term Term |
  Var Name | App Term Term | Lam Term
  deriving (Show, Eq)

data Neutral = NVar Ident | NApp Neutral Value
  deriving (Show, Eq)

type Closure = (Env, Term)
data Value = VNeut Neutral | VClo Closure | VType | VArrow Value Value
  deriving (Show, Eq)

vvar (Free ident) env = VNeut $ NVar ident
vvar (Bound key) env = env !! key
vapp (VNeut neutral) arg = VNeut $ NApp neutral arg
vapp (VClo (cenv, cbody)) arg = bigstep cbody $ arg : cenv

bigstep (Var name) env = vvar name env
bigstep (App tproc targ) env = vapp proc arg
  where proc = bigstep tproc env
        arg = bigstep targ env
bigstep (Lam body) env = VClo (env, body)

data TContext = TContext{tc_tvars :: M.Map EnvKey Term,
                         tc_bvars :: M.Map Name [Term],
                         tc_nextFresh :: EnvKey}
tc_fresh = do
  cxt <- get
  let fresh = tc_nextFresh cxt
  put $ cxt{tc_nextFresh = fresh + 1}
  return $ Var $ Bound fresh
tc_bvarFresh name = do
  fresh <- tc_fresh
  cxt <- get
  let tys = fresh : (maybe [] id $ M.lookup name $ tc_bvars cxt)
  put $ cxt{tc_bvars = M.insert name tys $ tc_bvars cxt}
  return fresh
tc_find key = do
  cxt <- get
  let (term, tvars) = _find (tc_tvars cxt) key []
  put $ cxt{tc_tvars = tvars}
  return term
  where
    _find tvars key accKeys = case M.lookup key tvars of
      Just (Var (Bound next)) -> _find tvars next $ key : accKeys
      Just term -> (term, merge term)
      Nothing -> (vkey, merge vkey)
      where vkey = Var $ Bound key
            merge term = M.union tvars $ M.fromList $ map (, term) accKeys
tc_assign key term = do
  cxt <- get
  put $ cxt{tc_tvars = M.insert key term $ tc_tvars cxt}

subterms (Arrow ta tb) = [ta, tb]
subterms (App ta tb) = [ta, tb]
subterms (Ann ta tb) = [ta, tb]
subterms (Lam body) = [body]
subterms _ = []

occurs name (Var vname) = name == vname
occurs name term = any (occurs name) $ subterms term

sunify (Ann ta tb) other = liftM (Ann ta) $ sunify tb other
sunify other ann@(Ann ta' tb') = sunify ann other
sunify (Var (Free _)) _ = error "something went wrong"
sunify tvar@(Var (Bound key)) other = do
  term <- tc_find key
  if tvar == term
    then if occurs (Bound key) term
      then throwError $ "occurs check failed: " ++ show (key, term)
      else tc_assign key term >> return term
    else sunify term other
sunify other (Var name) = sunify (Var name) other
sunify Type Type = return Type
sunify (Arrow ta tb) (Arrow ta' tb') =
  liftM2 Arrow (sunify ta ta') $ sunify tb tb'
-- TODO: reduce apps and inside lambdas for non-simple unify
sunify (App ta tb) (App ta' tb') = liftM2 App (sunify ta ta') $ sunify tb tb'
sunify (Lam body) (Lam body') = liftM Lam $ sunify body body'
sunify ta tb = throwError $ "type mismatch: " ++ show (ta, tb)

--sunifyE lenv renv = undefined

sbvar name = do
  cxt <- get
  let bvars = tc_bvars cxt
  case M.lookup name bvars of
    Nothing -> tc_bvarFresh name
    Just (ty : tys) -> do
      final <- foldM sunify ty tys
      put $ cxt{tc_bvars = M.insert name [final] bvars}
      return final

bigtype Type = return Type
bigtype (Arrow ta tb) = bigtype ta >> bigtype tb >> return Type
bigtype (Ann term ty) =
  bigtype ty >>= sunify Type >> bigtype term >>= sunify ty >> return ty
bigtype (Var name) = tc_bvarFresh name
bigtype (Lam body) = do -- TODO: how to quantify? free var dependencies?
  tbody <- bigtype body
  targ <- sbvar $ Bound 0
  return $ Arrow targ tbody

{-bigtype (App proc arg) = (tyRes, env')-}
  {-where (tyProc, penv) = bigtype proc-}
        {-(tyArg, aenv) = bigtype arg-}
        {-tyRes = undefined -- create fresh var-}
        {-tenv = sunify tyProc $ Arrow tyArg tyRes-}
        {-env = sunifyE penv aenv-}
        {-env' = sunifyE env tenv-}

-- data TermD_ lterm rterm =
