module Regex where

-- TODO: groups

import Prelude hiding (seq)
import Data.Set as S

data Regex a = Null
             | Empty
             | Seq (Regex a) (Regex a)
             | Alt (Regex a) (Regex a)
             | Rep (Regex a)
             | One a
  deriving (Show, Eq)

seq Null _ = Null
seq _ Null = Null
seq Empty pat2 = pat2
seq pat1 Empty = pat1
seq pat1 pat2 = Seq pat1 pat2

alt Null pat2 = pat2
alt pat1 Null = pat1
alt pat1 pat2 = Alt pat1 pat2

rep Null = Empty
rep Empty = Empty
rep pat = Rep pat

toEmpty Null = Null
toEmpty Empty = Empty
toEmpty (Seq pat1 pat2) = seq (toEmpty pat1) (toEmpty pat2)
toEmpty (Alt pat1 pat2) = alt (toEmpty pat1) (toEmpty pat2)
toEmpty (Rep pat) = Empty
toEmpty (One _) = Null

deriv Null _ = Null
deriv Empty _ = Null
deriv (Seq pat1 pat2) atom = alt (seq (deriv pat1 atom) pat2)
                                 (seq (toEmpty pat1) (deriv pat2 atom))
deriv (Alt pat1 pat2) atom = alt (deriv pat1 atom) (deriv pat2 atom)
deriv (Rep pat) atom = seq (deriv pat atom) (rep pat)
deriv (One patom) atom = if patom == atom then Empty else Null

match pat [] = case toEmpty pat of
  Empty -> True
  _ -> False
match pat (fst:rest) = match (deriv pat fst) rest

tests = [
  deriv (One 'b') 'f' == Null,
  deriv (seq (One 'f') (One 'b')) 'f' == One 'b',
  deriv (alt (seq (One 'f') (One 'b')) (seq (One 'f') (rep (One 'z')))) 'f' == Alt (One 'b') (Rep (One 'z')),
  match (seq (One 'f') (rep (One 'b'))) "fbbb" == True,
  match (seq (One 'f') (rep (One 'b'))) "fbzbb" == False,
  match (seq (One 'f') (rep (alt (One 'b') (One 'z')))) "fbzbb" == True]
