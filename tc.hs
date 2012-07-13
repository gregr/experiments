{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
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

-- "fun" trying to hack the type system to pretend it has intersection types
class Apply f a b | f a -> b where
  apply :: f -> a -> b

data F0 = F0

instance Apply F0 Int Int where
  apply F0 x = x + 1

{-instance Apply (a -> b) a b where-}
  {-apply f x = f x-}

instance Apply F00 where
  apply

data TWO = TWO

data F00 = F00

instance Apply TWO F0 F00 where
  apply TWO F0 = F00

instance Apply TWO F00 (Int -> Int) where
  apply TWO F00 = two F00

-- data F1 = F1

-- two :: (Apply f a b, Apply f b c) => f -> a -> c
two f zero = f `apply` (f `apply` zero)

three f zero = f `apply` (f `apply` (f `apply` zero))
