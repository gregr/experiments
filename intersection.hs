{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FunctionalDependencies, ScopedTypeVariables, RankNTypes #-}
module Intersection where

class Succ a b | a -> b where
  sc :: a -> b

instance Succ Int Char where
  sc _ = 'i'

instance Succ Char String where
  sc ch = [ch]

instance Succ String Int where
  sc _ = 42

once z = sc z

twice z = sc $ sc z

thrice z = sc $ sc $ sc z

--two (s :: (Succ a b, Succ a1 a) => a1 -> b) z = s $ s z
two (s :: (Succ a b) => a -> b) z = s $ s z

n = 4 :: Int
