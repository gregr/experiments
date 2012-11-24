{-# LANGUAGE NoMonomorphismRestriction #-}
module SimpleCalc where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity

-- TODO
-- normal forms for quantifiers
--  subsumption vs. unification
-- fixpoints
-- let generalization -> fully polymorphic arguments
-- constructors/case-elimination -> records and variants
-- implicits
-- generalize the concept of constraint propagation in abstract interpretation

type Name = Int
nm_next name = name + 1
type Env = [Value]
env_empty = []
env_lookup env name = env !! name
env_extend env value = value : env

type OrderedSet a = (Int, M.Map Int a, M.Map a Int)
ords_empty = (0, M.empty, M.empty) :: OrderedSet a
ords_count (count, _, _) = count
ords_isEmpty (0, _, _) = True
ords_isEmpty _ = False
ords_insert os@(count, index, table) el = case M.lookup el table of
  Nothing -> (count + 1, M.insert count el index, M.insert el count table)
  Just _ -> os
ords_concat oa ob = ords_fromList $ ords_toList oa ++ ords_toList ob
ords_lookup (_, _, table) name = fromMaybe
  (error $ "out of bounds: " ++ show (name, table))
  (M.lookup name table)
ords_toSet (_, _, table) = M.keysSet table
ords_toList (count, index, _) =
  [fromJust (M.lookup ix index) | ix <- take count [0 ..]]
ords_fromList xs = (length xs, index, table)
  where index = M.fromList $ zip [0 ..] xs
        table = M.fromList $ zip xs [0 ..]
ords_diffMeet oa ob = (ords_fromList ldiff, ords_fromList lmeet)
  where shared = S.intersection (ords_toSet oa) (ords_toSet ob)
        la = ords_toList oa
        ldiff = filter (not . (`S.member` shared)) la
        lmeet = filter (`S.member` shared) la

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
cstr_unify ta tb = do
  ta' <- cstr_find ta
  tb' <- cstr_find tb
  cstr_unify' ta' tb'
  where
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
cstr_join ca cb = do
  ((), cstr') <- runStateT (merge cbshared) cstr
  return cstr'
  where
    cbshared = M.toList $ M.intersection ca cb
    cstr = M.union ca $ M.difference cb ca
    merge = mapM_ (\(name, bval) -> cstr_unify (VCVar name) bval)
cstr_freeInType = cstr_freeInType' ords_empty
cstr_freeInType' acc ty = cstr_find ty >>= cft acc
  where
    cft acc var@(VCVar name) = return $ ords_insert acc name
    cft acc VType = return acc
    cft acc (VArrow ta tb) = inner acc ta >>= (`inner` tb)
      where inner = cstr_freeInType'
    cft acc (VClo (env, _)) = cstr_freeInEnv' acc env
cstr_freeInEnv = cstr_freeInEnv' ords_empty
cstr_freeInEnv' = foldM cstr_freeInType'
cstr_generalize ty env = do
  (oquant, ofree) <-
    liftM2 ords_diffMeet (cstr_freeInType ty) (cstr_freeInEnv env)
  if ords_isEmpty oquant then return ty
    else
      let oall = ords_concat oquant ofree
          termify ty = cstr_find ty >>= tfy
          tfy (VCVar name) = return . Var $ ords_lookup oall name
          tfy VType = return Type
          tfy (VArrow ta tb) = liftM2 Arrow (termify ta) (termify tb)
          tfy (VClo (env, body)) = error "TODO: will this ever happen?"
      in do
        term <- termify ty
        let free = map VCVar (ords_toList ofree)
        return $ VClo (free, iterate Lam term !! (ords_count oquant - 1))

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
runType = runInit . flip bigtype env_empty
test_types = map runType tests
test = forM_ test_types print
runGeneralize term = do
  ((ty, cstr), _) <- runType term
  return $ runIdentity $ runStateT (cstr_generalize ty env_empty) cstr
test_generalize = map runGeneralize tests
testg = forM_ test_generalize print
