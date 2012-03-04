{-# LANGUAGE NamedFieldPuns #-}

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

class Misc a where
  misc :: a -> String

data (Misc a) => MiscHolder a = MH a
                              | MH2 a a
  deriving (Show)

instance Misc Int where
  misc x = show $ x + 1
