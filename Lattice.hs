{-# LANGUAGE NoMonomorphismRestriction #-}
module Lattice where

import qualified Data.Set as S
import qualified Data.Map as M

class SetLike s where
  difference :: s -> s -> s
  intersection :: s -> s -> s
  union :: s -> s -> s

{-class MapLike (m x) where-}
{---  differenceWith :: m -> m -> m-}
  {-intersectionWith :: () -> m -> m -> m-}
  {-unionWith :: m -> m -> m-}
  {-lookup :: m -> k -> a-}

instance (Ord key) => SetLike (S.Set key) where
  difference = S.difference
  intersection = S.intersection
  union = S.union

instance (Ord key) => SetLike (M.Map key val) where
  difference = M.difference
  intersection = M.intersection
  union = M.union

{-instance MapLike (M.Map key val) where-}
{---  differenceWith = M.differenceWith-}
  {-intersectionWith = M.intersectionWith-}
  {-unionWith = M.unionWith-}
  {-lookup = M.lookup-}

class Lattice a where
  (<:) :: a -> a -> Bool
  join :: a -> a -> a
  meet :: a -> a -> a

class (Lattice a) => LowerBoundedLattice a where
  bottom :: a

class (Lattice a) => UpperBoundedLattice a where
  top :: a

instance Lattice Bool where
  False <: x = True
  True <: False = False
  True <: True = True
  join False x = x
  join True x = True
  meet False x = False
  meet True x = x

instance LowerBoundedLattice Bool where
  bottom = False

-- least fixed points

data Scc key = Scc { sccPending, sccFinished :: S.Set key, sccChanged :: Bool }
newScc = Scc S.empty S.empty False

data LFixSt key val = LFixSt { finished, pending, unfinished :: M.Map key val,
                               scc :: Scc }

getFixSt = get
putFixSt = put

fixLookup key = do
  st <- getFixSt
  case lkup $ finished st of
    Just val -> return val
    Nothing ->
      case lkup $ pending st of
        Just val -> do
          let st' = st{sccPending=S.insert key $ sccPending st}
          putFixSt st'
          return val
        Nothing ->
          let val = case lkup $ unfinished st of
                      Just val -> val
                      Nothing -> bottom

          let sccParent = scc st
          let st0 = st{pending=M.insert key val $ pending st, scc = newScc}
          putFixSt st0
          -- val = f key
          st <- getFixSt
          let sccCur = scc st
          let sccCur = sccCur{sccPending=S.delete key $ sccPending sccCur}
          if not . S.null $ sccPending sccCur then do
            let st1' = st{sccFinished=S.insert key sccFinished}
            putFixSt st1'
          else
            let fin = sccFinished st
            if not $ S.null fin then
              let pending' = S.difference st1 fin
              if sccChanged then
                let st2 = st1{pending=pending'}
                putFixSt st2
                fixLookup key
        {-
         - if recursive and any member of SCC changed: pending=pending-sccFinished; re-evaluate
         - otherwise move SCC/individual to finished
         - return value
         - -}
          return val
  where lkup = M.lookup key
