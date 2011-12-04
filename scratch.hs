{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad.State
import Data.Map as M
import Data.Maybe

test = do
  put 4
  val <- get
  return "hi"
test' = runState test 0

data GTerm st key tok = Fail
                      | Eps
                      | Conc (GTerm st key tok) (GTerm st key tok)
                      | Alt (GTerm st key tok) (GTerm st key tok)
                      | Ref key
                      | One tok

instance (Show key, Show tok) => Show (GTerm st key tok) where
  show Fail = "Fail"
  show Eps = "Eps"
  show (Conc p1 p2) = show p1 ++ " ; " ++ show p2
  show (Alt p1 p2) = show p1 ++ "|" ++ show p2
  show (Ref key) = "<" ++ show key ++ ">"
  show (One tok) = show tok

conc Eps p2 = p2
conc p1 Eps = p1
conc Fail p2 = Fail
conc p1 Fail = Fail
conc p1 p2 = Conc p1 p2

alt Fail p2 = p2
alt p1 Fail = p1
alt p1 p2 = Alt p1 p2

data GState rm = GState { curRules :: rm,
                          newRules :: rm,
                          nullities :: rm}
type RefMap st key tok = M.Map key (GTerm st key tok)

gstGet proj key = get >>= return . (M.lookup key) . proj
getCurRule = gstGet curRules
getNewRule = gstGet newRules
getNullity = gstGet nullities
putNewRule key val = do
  state <- get
  put $ state{newRules=M.insert key val $ newRules state}--(cur, M.insert key val new)
putNullity key val = do
  state <- get
  put $ state{nullities=M.insert key val $ nullities state}--(cur, M.insert key val new)

-- TODO: fixpoint (too eager to Fail as is)
nullify Fail = return Fail
nullify Eps = return Eps
nullify (Conc p1 p2) = do
  n1 <- nullify p1
  n2 <- nullify p2
  return $ conc n1 n2
nullify (Alt p1 p2) = do
  n1 <- nullify p1
  n2 <- nullify p2
  return $ alt n1 n2
nullify (Ref key) = do
  res <- getNullity key
  case res of
    Just res -> return res
    Nothing -> do
      (Just pat) <- getCurRule key
      putNullity key Fail
      res <- nullify pat
      putNullity key res
      return res
nullify (One tok) = return Fail

deriv _ Fail = return Fail
deriv _ Eps = return Fail
deriv tok (Conc p1 p2) = do
  d1 <- deriv tok p1
  n1 <- nullify p1
  d2 <- deriv tok p2
  return $ alt (conc n1 d2) $ conc d1 p2
deriv tok (Alt p1 p2) = do
  d1 <- deriv tok p1
  d2 <- deriv tok p2
  return $ alt d1 d2
deriv tok (Ref key) = do
  res <- getNewRule dk
  case res of
    Just _ -> return ()
    Nothing -> do
      putNewRule dk Fail
      pat <- getCurRule key
      case pat of
        Nothing -> error $ show key
        Just pat -> do
          dpat <- deriv tok pat
          putNewRule dk dpat
  return $ Ref dk
  where dk = newKey key tok
deriv t1 (One t2) = if t1 == t2 then return Eps else return Fail

newKey (ident, toks) tok = (ident, tok:toks)

gstFirst gram = GState gram M.empty M.empty
gstNext = do
  (GState cur new nulls) <- get
  put $ GState (M.union cur new) M.empty nulls

recognize gram start toks = fst $ runState (recog toks start) $ gstFirst gram
  where recog [] pat = do
          final <- nullify pat
          case final of
            Fail -> return False
            _ -> return True
        recog (tok:toks) pat = do
          dpat <- deriv tok pat
          gstNext
          recog toks dpat

match gram start toks = fst $ runState (match' toks start) $ gstFirst gram
  where match' [] pat = nullify pat
        match' (tok:toks) pat = do
          dpat <- deriv tok pat
          gstNext
          match' toks dpat

-- testing

lang1 = Ref (0, "")
gram1 = M.fromList [
  ((0, ""), alt (Eps) $ conc (One 'r') lang1)]

{-
lang2 = Ref (1, "") $ alt (Eps) $ conc [lang2, One 'r']
lang3 = Ref (2, "") $ alt lang3 (Eps)
lang4 :: TestGrammar
lang4 = Ref (3, "") lang4
lang5 = Ref (4, "") $ conc [Eps, lang5]
lang6 = Ref (5, "") $ conc [lang6, Eps]
langS = Ref (6, "") $ alt (conc [langS, One '+', langS]) $ One 'n'
-}
