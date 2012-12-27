{-# LANGUAGE NamedFieldPuns, RankNTypes, ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}

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
