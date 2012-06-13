{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Stuff where

class One a where
  aa :: a -> Int

class Two a where
  bb :: a -> String

class (One a, Two a) => Three a where

-- class (One a, Two a) => Four a where
class (Three a) => Four a where
  cc :: a -> (Int, String)
  cc x = (aa x, bb x)

data State s a = State { runst :: (s -> (a, s)) }

bind (State k) f = State k''
  where k'' s = (b, s'')
          where (a, s') = k s
                (State k') = f a
                (b, s'') = k' s'
get = State $ \s -> (s, s)
put a = State $ \_ -> ((), a)

instance Monad (State s) where
  return x = State $ \s -> (x, s)
  (>>=) = bind

tryget x = do
  s <- get
  return (x, s)

class Coerce a b where
  co :: a -> b

instance Coerce Integer [Char] where
  co n = show n

instance Coerce Int Integer where
  co n = toInteger n

instance Coerce Int [Char] where
  co n = co (co n :: Integer)

testco :: String
testco = co (4 :: Int)

data SuperList a = Nil
                 | Cons a (SuperList (SuperList a))
  deriving Show
