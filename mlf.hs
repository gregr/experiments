{-# LANGUAGE NoMonomorphismRestriction #-}
module MLF where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity

type Name = Int
data Quant = Rigid Poly | Univ (Maybe Poly) | Exis
data Mono = LocalVar Int | FreeVar Name | Arrow Mono Mono | Const Int -- | Pair Mono Mono | Skolem Int
type Poly = ([Quant], Mono)
--type InstEnv = []
--data Scope = Scope (M.Map Name Quantifier)
--type Scope = [(Name, Quantifier)]
--data Node = Node Scope Constr
--data Node = Node {nd_constr :: Constr, nd_parent :: Name}
