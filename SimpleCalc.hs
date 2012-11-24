{-# LANGUAGE NoMonomorphismRestriction #-}
module SimpleCalc where

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity

type Name = Int
nm_next name = name + 1
type Env = [Value]
env_empty = []
env_lookup env name = env !! name
env_extend env value = value : env

data Term =
  Ann Term Term |
  Type | Arrow Term Term |
  Var Name | Lam Term | App Term Term
  deriving (Show, Eq)

-- TODO: unifiable neutral terms/types
type Closure = (Env, Term)
data Value = VClo Closure | VCVar Name | VType | VArrow Value Value
  deriving (Show, Eq)

bigeval (Ann term _) env = bigeval term env
bigeval Type _ = VType
bigeval (Arrow ta tb) env = VArrow (bigeval ta env) (bigeval tb env)
bigeval (Var name) env = env_lookup env name
bigeval (Lam body) env = VClo (env, body)
bigeval (App proc arg) env = bigeval cbody nenv
  where
    VClo (cenv, cbody) = bigeval proc env
    varg = bigeval arg env
    nenv = env_extend cenv varg

type Typing = (Value, Constraint)
type Constraint = M.Map Name Term
cstr_empty = M.empty
cstr_assign name ty = modify $ M.insert name ty
cstr_find (VCVar name) = do
  cstr <- get
  case M.lookup name cstr of
    Nothing -> return $ VCVar name
    Just ty -> do
      ty' <- cstr_find ty
      cstr_assign name ty'
      return ty'
cstr_find ty = return ty
cstr_occurs na (VCVar nb) = na == nb
cstr_occurs name (VArrow ta tb) = any (cstr_occurs name) [ta, tb]
cstr_occurs name _ = False
cstr_unify' (VCVar na) var@(VCVar nb) | na == nb = return var
cstr_unify' (VCVar name) ty
  | cstr_occurs name ty = throwError $ "Occurs check: " ++ show (name, ty)
  | otherwise = cstr_assign name ty >> return ty
cstr_unify' other var@(VCVar _) = cstr_unify' var other
cstr_unify' VType VType = return VType
cstr_unify' (VArrow la lb) (VArrow ra rb) = do
  ta <- cstr_unify la ra
  tb <- cstr_unify lb rb
  return $ VArrow ta tb
cstr_unify' ta tb = throwError $ "Type mismatch: " ++ show (ta, tb)
cstr_unify ta tb = do
  ta' <- cstr_find ta
  tb' <- cstr_find tb
  cstr_unify' ta' tb'
cstr_join ca cb = do
  ((), cstr') <- runStateT (merge cbshared) cstr
  return cstr'
  where
    cbshared = M.toList $ M.intersection ca cb
    cstr = M.union ca $ M.difference cb ca
    merge = mapM_ (\(name, bval) -> cstr_unify (VCVar name) bval)

data Context = Context{cxt_nextName :: Name}
  deriving Show
cxt_empty = Context{cxt_nextName = 0}
cxt_freshName = do
  fresh <- gets cxt_nextName
  modify (\cxt -> cxt{cxt_nextName = nm_next fresh})
  return . VCVar $ fresh

bigtype Type _ = return (VType, cstr_empty)
bigtype (Arrow ta tb) env = do
  (vta, ca) <- bigtype ta env
  (vtb, cb) <- bigtype tb env
  cstr <- cstr_join ca cb
  (_, cstr') <- runStateT (cstr_unify vta VType >> cstr_unify vtb VType) cstr
  return (VType, cstr')
bigtype (Var name) env = return (env_lookup env name, cstr_empty)
bigtype (Lam body) env = do
  fresh <- cxt_freshName
  (tbody, cstr) <- bigtype body (fresh : env)
  return (VArrow fresh tbody, cstr)
bigtype (App proc arg) env = do
  (tproc, ca) <- bigtype proc env
  (targ, cb) <- bigtype arg env
  cstr <- cstr_join ca cb
  na <- cxt_freshName
  nb <- cxt_freshName
  (_, cstr') <- runStateT
    (cstr_unify tproc (VArrow na nb) >> cstr_unify targ na) cstr
  return (nb, cstr')
bigtype (Ann term tyterm) env = do
  (tt, ca) <- bigtype tyterm env
  (tty, cb) <- bigtype term env
  cstr <- cstr_join ca cb
  let ty = bigeval tyterm env
  runStateT (cstr_unify tt VType >> cstr_unify tty ty) cstr

-- TODO: garbage collect constraints based on env reachability

-- testing
test_id = Lam $ Var 0
test_k = Lam . Lam $ Var 1
test_app0 = test_id `App` test_id
test_app1 = test_k `App` test_id
test_app2 = test_k `App` test_k
test_app3 = test_app1 `App` test_k
test_app4 = test_app2 `App` test_k
tests = [test_id, test_k, test_app0, test_app1, test_app2, test_app3, test_app4]
test_evals = map (`bigeval` env_empty) tests
runInit = runIdentity . runErrorT . flip runStateT cxt_empty
test_types = map (runInit . flip bigtype env_empty) tests
test = forM_ test_types print
