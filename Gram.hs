{-# LANGUAGE NoMonomorphismRestriction #-}
module Gram where

import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe
import System.IO.Unsafe

data Term key tok = Fail
                  | Eps
                  | Conc (Term key tok) (Term key tok)
                  | Alt (Term key tok) (Term key tok)
                  | Ref key
                  | One tok
  --deriving (Show)

instance (Show key, Show tok) => Show (Term key tok) where
  show Fail = "Fail"
  show Eps = "Eps"
  show (Conc lhs rhs) = show lhs ++ show rhs
  show (Alt lhs rhs) = "(" ++ show lhs ++ "|" ++ show rhs ++ ")"
  show (Ref key) = "<" ++ show key ++ ">"
  show (One tok) = show tok

type Rule key tok = (key, Term key tok)
type Gram key tok = [Rule key tok]

{-conc = Conc-}
{-alt = Alt-}

conc Eps t2 = t2
conc t1 Eps = t1
conc Fail t2 = Fail
conc t1 Fail = Fail
conc t1 t2 = Conc t1 t2

alt Fail t2 = t2
alt t1 Fail = t1
alt t1 t2 = Alt t1 t2

type BM key = M.Map key (Bool, Bool)
type RM key tok = M.Map key (Term key tok)

data RuleSt key tok = RuleSt { rules :: RM key tok, nullables :: BM key, changed :: Bool }
clearNullables = do
  st <- get
  put st{nullables=M.empty, changed=False}
getSt proj key = get >>= return . M.lookup key . proj
getRule = getSt rules
getNullable key = do
  res <- getSt nullables key
  case res of
    Just res -> return res
    Nothing -> return (False, False)
getChanged = get >>= return . changed

putNullable key val = do
  st <- get
  put st{nullables=M.insert key val $ nullables st}
putRule key val = do
  st <- get
  put st{rules=M.insert key val $ rules st}
putChanged ch = do
  st <- get
  put st{changed=ch}

{-fromBool False = 0-}
{-fromBool True = 1-}
{-countTrue xs = foldl (\cnt (_, bl) -> cnt + fromBool bl) 0 $ toList xs-}

--cmpNullables n0 n1 = countTrue n0 == countTrue n1
--
--bidiff ma mb = M.union (M.difference ma mb) $ M.difference mb ma

{-cmpNullables n0 n1 = shared && diff-}
  {-where shared = all (True ==) $ map snd . M.toList $ M.intersectionWith (==) n0 n1-}
        {-diff = all (False ==) $ map snd . M.toList $ bidiff n0 n1-}
resetNullables = do
  st <- get
  let nls = nullables st
  let nls' = fmap (\(_, val) -> (False, val)) nls
  put $ st{nullables=nls'}

nullable' Fail = return False
nullable' Eps = return True
nullable' (Conc lhs rhs) = do
  nl <- nullable' lhs
  nr <- nullable' rhs
  return $ nl && nr
nullable' (Alt lhs rhs) = do
  nl <- nullable' lhs
  nr <- nullable' rhs
  return $ nl || nr
nullable' (Ref key) = do
  res <- getNullable key
  case res of
    (True, res) -> return res
    (False, old) -> do
      putNullable key (True, old)
      (Just term) <- getRule key
      res <- nullable' term
      when (res /= old) $ putChanged True
      putNullable key (True, res)
      return res
nullable' (One tok) = return False

nullable rule = do
  res <- nullable' rule
  change <- getChanged
  putChanged False
  if change then do
    resetNullables
    nullable rule
  else return res

testNullable rule = fst $ runState (nullable rule) RuleSt { rules=M.empty, nullables=M.empty, changed=False }

mkKey nm = (nm, "")
derivKey tok (nm, ts) = (nm, tok:ts)
deriv _ Fail = return Fail
deriv _ Eps = return Fail
deriv tok (Conc lhs rhs) = do
  nl <- nullable lhs
  dl <- deriv tok lhs
  dr <- deriv tok rhs
  return $ if nl then alt (conc dl rhs) dr else conc dl rhs
deriv tok (Alt lhs rhs) = do
  dl <- deriv tok lhs
  dr <- deriv tok rhs
  return $ alt dl dr
deriv tok (Ref key) = do
  res <- getRule dkey
  case res of
    Just res -> return ()
    Nothing -> do
      putRule dkey Fail
      (Just term) <- getRule key
      dterm <- deriv tok term
      putRule dkey dterm
  return $ Ref dkey
  where dkey = derivKey tok key
deriv t0 (One t1) = return $ if t0 == t1 then Eps else Fail

mkSt gram = RuleSt {rules=gram, nullables=M.empty, changed=False}

recognize gram start toks = fst $ runState (recog toks start) $ mkSt gram
  where recog [] term = nullable term
        recog (tok:ts) term = deriv tok term >>= recog ts

drv gram start toks = fst $ runState (drv' toks start) $ mkSt gram
  where drv' [] term = return term
        drv' (tok:ts) term = deriv tok term >>= drv' ts

-- todo: match/parse

-- S ::= S + S | 1
kS = mkKey 'S'
rsum = Ref $ kS
tone = One '1'
tsum = conc rsum $ conc (One '+') rsum
tsum1 = alt tsum tone
gsum = M.fromList [(kS, tsum1)]

td = drv gsum tsum1
test = recognize gsum tsum1

easy = "1"
good = "1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1"
bad = good ++ "+"

-- P ::= P ( P ) ∪ ε
kP = mkKey 'P'
rparen = Ref $ kP
tpl = One '('
tpr = One ')'
tparen = conc rparen $ conc tpl $ conc rparen $ tpr
tparen1 = alt tparen Eps
gparen = M.fromList [(kP, tparen1)]

tdp = drv gparen tparen1
testp = recognize gparen tparen1

goodp = "(())()(()())((()))(()(()))((((((()))))))"
badp = "(())()(()())((()))(()(()))(((((()))))))"
