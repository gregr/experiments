{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleContexts, NamedFieldPuns, RankNTypes, ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}

module Examples where

data Thing = Thing { name :: String, info :: Int }

showName (Thing { name = nm }) = nm

showTh th = nm where
  Thing { name = nm } = th

shth (Thing {info, name}) = info

data Pun = One { x :: Int, y :: Int } | Two { x :: Int }

--showOther { name = nm } = nm

-- polymorphic recursion
data Expr' key expr = Var key
                    | App expr expr
newtype Expr key = Expr (Expr' key (Expr key))

{-class Misc a where-}
  {-misc :: a -> String-}

{-data (Misc a) => MiscHolder a = MH a-}
                              {-| MH2 a a-}
  {-deriving (Show)-}

{-instance Misc Int where-}
  {-misc x = show $ x + 1-}

retident :: Int -> forall a. a -> a
retident 0 = \x -> x
retident _ = fst (retident 0, ((retident 0) 4, (retident 0) 'c'))

-- no need for dependent types for this example
class ApplyTup args func result where
  applyTup :: func -> args -> result

instance (a ~ b) => ApplyTup () a b where
  applyTup result () = result

instance (ApplyTup b d result, a ~ c) => ApplyTup (a, b) (c -> d) result where
  applyTup f (arg, rest) = applyTup (f arg) rest


-- explicit "dictionary-passing" translation
applyTupEx (self, next) f args = self next f args

applyTupUnitMethod () result () = result
applyTupPairMethod (next, cont) f (arg, rest) = next cont (f arg) rest

testMethod = (applyTupPairMethod, (applyTupPairMethod, (applyTupPairMethod, (applyTupUnitMethod, ()))))
testArgs = (True, ((), (4, ())))
testApplyTupEx = applyTupEx testMethod (\x () y -> (x, y)) testArgs

-- gadt translation
data LL r where
  LLNil :: LL ()
  LLCons :: a -> LL bs -> LL (a, bs)
-- can these be derived?
instance Show (LL ()) where
  show LLNil = "LLNil"
instance (Show a, Show (LL bs)) => Show (LL (a, bs)) where
  show (LLCons a bs) = "(LLCons " ++ show a ++ " " ++ show bs ++ ")"
-- this does not work
{-deriving instance Show (LL ())-}
{-deriving instance (Show a, Show (LL b)) => Show (LL (a, b))-}

-- cleaner than the class-based version?
type family ApplyFuncLL tup result where
  ApplyFuncLL () result = result
  ApplyFuncLL (a, b) result = a -> ApplyFuncLL b result
applyLL :: ApplyFuncLL ab result -> LL ab -> result
applyLL f LLNil = f
applyLL f (LLCons a bs) = applyLL (f a) bs
-- applyLL ((,) . (+ 1)) (LLCons 3 (LLCons 'c' LLNil))
-- ===> (4, 'c')
