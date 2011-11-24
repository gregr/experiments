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
