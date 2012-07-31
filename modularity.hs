{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances, RankNTypes, ScopedTypeVariables, ImpredicativeTypes #-}
module Modularity where

------------------------------------------------------------------------------
-- modular injection; how modular is Haskell's type inference and checking?
------------------------------------------------------------------------------
injector0 mod = let
  enlist x = [x]
  in
  mod (enlist, 4)

injector (mod :: (((forall a. a -> [a]), t1) -> t)) = let
  enlist x = [x]
  in
  mod (enlist, 4)

-- injection into this version fails
-- forall a. a -> [a] isn't treated as a 'subtype' of a -> [a]
-- I guess Haskell can't figure out that it can instantiate?
somemod0 ((lister :: a -> [a]), _) =
  lister 17

{-somemod1 (lister, _) =-}
  {-(lister 24, lister 'c')-}

somemod2 ((lister :: forall a. a -> [a]), _) =
  (lister 32, \a b -> (lister a, lister b))

-- Haskell really hates polymorphism!
--mod0 = injector somemod0
--mod1 = injector somemod1

-- Wow, Haskell can type mod1 correctly!
-- But it can't type the subterm: (\(lister, _) -> (lister 24, lister 'c'))
-- This subterm is equivalent to somemod1 above that also fails to type
mod1 = injector (\(lister, _) -> (lister 24, lister 'c'))
mod2 = injector somemod2

mod3 = let
  enlist x = [x]
  in
  (enlist 41, enlist 'c', enlist)

------------------------------------------------------------------------------
-- coercion for simulating something like intersection types
------------------------------------------------------------------------------
class Coerce a b | a -> b where
  co :: a -> b -> b
  co a b = b

class Cocoerce a b | b -> a where
  coco :: a -> b -> b
  coco a b = b

class Biject a b | a -> b, b -> a where
  bi :: a -> b -> b
  bi a b = b

class Bleh a b where
  bleh :: a -> b

instance Bleh Int String where
  bleh = show

instance Bleh String Char where
  bleh (c:cs) = c

instance Bleh String Int where
  bleh xs = length xs

instance Bleh Bool Char where
  bleh True = 'y'
  bleh False = 'n'

-- auto-coercing versions of bleh
cobleh arg = co arg $ bleh arg
cocobleh arg = coco arg $ bleh arg
bibleh arg = bi arg $ bleh arg

instance Coerce Int String where
instance Coerce String Char where
instance Coerce Bool Char where

instance Cocoerce Int String where
instance Cocoerce String Char where
instance Cocoerce String Int where

instance Biject Int String where
instance Biject String Char where
--instance Biject Bool Char where

val = 562 :: Int

test00 = cobleh val
--test01 = cocobleh val
test02 = bibleh val
test03 = cobleh val :: String
test04 = cocobleh val :: String
test05 = bibleh val :: String

--test11 = cobleh 74
--test12 = cocobleh 74
--test13 = bibleh 74
--test14 = cobleh 74 :: String
test15 = cocobleh 74 :: String
test16 = bibleh 74 :: String

test20 = bibleh $ bibleh val
test21 = cobleh $ cobleh val
test22 = cocobleh $ cocobleh val :: Char
test23 = cocobleh $ cocobleh 74 :: Char
test24 = bibleh $ bibleh 74 :: Char

------------------------------------------------------------------------------
-- how would implicit propagation of explicit staging work?
------------------------------------------------------------------------------
-- Would like to 'stage' certain functions in order to get the benefit of
-- modular/flow/intersection types; achieved by forcing normalization of
-- 'staged' terms to get an effect similar to let-generalization?  I think
-- this would generalize the technique of let-generalization itself:
--   unstage { (\f -> ...use f polymorphically...) literal }
-- becomes equivalent in polymorphic expressiveness to:
--   let f = literal in ...use f polymorphically...
-- without staging, normally the following is the case:
--   (\f -> ...f is monomorphic...) literal

-- polytup :: (a -> c & b -> d) -> a -> b -> (c, d)
polytup f a b = (f a, f b)
