{-# LANGUAGE NamedFieldPuns #-}
module PiSig where

-- loosely based on http://www.andres-loeh.de/LambdaPi/

import Control.Monad.Error

infixl 3 :@:

type BName = Int
data FName = Global String | Local BName | Quote BName
  deriving (Show, Eq)

-- start simply typed
data Ty = TyFree FName | TyArrow Ty Ty
  deriving (Show, Eq)
data TermInfer = Ann TermCheck Ty | Bound BName | Free FName | TermInfer :@: TermCheck
  deriving (Show, Eq)
data TermCheck = Infer TermInfer | Lam TermCheck
  deriving (Show, Eq)
data Neutral = NFree FName | NApp Neutral Value
  deriving (Show, Eq)
data Value = VNeut Neutral | VClo Closure
  deriving (Show, Eq)

data Code = CClo [Code] | CLit Value | CRef BName | CApp
  deriving (Show, Eq)
type Env = [Value]
type Closure = (Env, [Code])

data State = State {s_code :: [Code], s_env, s_stack :: Env, s_cont :: Maybe State}
s_init codes = State {s_code = codes, s_env = [], s_stack = [], s_cont = Nothing}
s_push st val = st{s_stack = val : s_stack st}
s_pop st@State {s_stack = val : vals} = (val, st{s_stack = vals})
s_lookup State {s_env} bname = s_env !! bname
s_isFinal State {s_code = [], s_cont = Nothing} = True
s_isFinal _ = False
s_ret st
  | not (s_isFinal st) && null (s_code st) = s_cont st
  | otherwise = Just st
s_next st@State {s_code = code : codes} = (code, st{s_code = codes})
-- TODO: assert empty code implies empty stack?
s_step State {s_code = [], s_stack = [val], s_cont = Just st'} = s_push st' val
s_step st@State {s_code = code : codes} = exec code st{s_code = codes}

vfree n = VNeut (NFree n)
vapp st (VNeut neutral) arg = s_push st $ VNeut (NApp neutral arg)
vapp st (VClo (env, codes)) arg = State codes (arg : env) [] $ s_ret st

exec (CClo codes) st@State {s_env} = s_push st $ VClo (s_env, codes)
exec (CLit val) st = s_push st val
exec (CRef bname) st = s_push st $ s_lookup st bname
exec CApp st = vapp st'' proc arg
  where
    (proc, st') = s_pop st
    (arg, st'') = s_pop st'

eval = dropWhile (not . s_isFinal) . iterate s_step

data Kind = Star
  deriving (Show)
data Info = HasKind Kind | HasType Ty
  deriving (Show)
type Context = [(FName, Info)]
cxt_lookup cxt name = case lookup name cxt of
  Just info -> return info
  Nothing -> throwError "unknown identifier"

kind cxt (TyFree name) Star = do
  HasKind Star <- cxt_lookup cxt name
  return ()
kind cxt (TyArrow tyProc tyArg) Star = kind cxt tyProc Star >> kind cxt tyArg Star

typeInfer level cxt (Ann term ty) =
  kind cxt ty Star >> typeCheck level cxt term ty >> return ty
typeInfer level cxt (Free name) = do
  HasType ty <- cxt_lookup cxt name
  return ty
typeInfer level cxt (proc :@: arg) = do
  tyProc <- typeInfer level cxt proc
  case tyProc of
    TyArrow tyArg tyRes -> typeCheck level cxt arg tyArg >> return tyRes
    _ -> throwError $ "applying non-proc type: " ++ show tyProc
typeCheck level cxt (Infer term) ty = do
  ty' <- typeInfer level cxt term
  unless (ty == ty') $ throwError $ "expected type '" ++ show ty ++ "' but inferred: " ++ show ty'
typeCheck level cxt (Lam term) (TyArrow tyArg tyRes) = typeCheck (level + 1)
  ((Local level, HasType tyArg) : cxt) (substCheck 0 (Free (Local level)) term) tyRes
typeCheck _ _ _ _ = throwError "type error"

substInfer = undefined -- level
substCheck = undefined

quote level (VClo (env, codes)) = undefined
quote level (VNeut neutral) = Infer (neutQuote level neutral)
neutQuote level (NFree name) = boundFree level name
neutQuote level (NApp neut arg) = neutQuote level neut :@: quote level arg

boundFree level (Quote depth) = Bound (level - depth - 1)
boundFree level name = Free name
