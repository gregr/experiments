module Term where

import Data.Map as M qualified
import Data.Set as S qualified

type Symbol = Integer
type Tag = Symbol
type Name = Symbol
type Binding val = (Name, val)

data Env val = Env [Binding val] -- constraint satisfiers are bindings

data Typing = Typing Term Env

data Variance = Invariant | Covariant | Contravariant

data Constr = Constr { constrTag :: Tag,
                       constrVar :: [Variance],
                       constrTy :: Typing }

type Proc = ([Name], Term)

data Value = Datum Constr [Value]
           | Closure Proc (Env Value)

data Term = Var Name
          | App Term [Term]
          | Abs Proc -- parallel, positional, keyword, and implicit args
          | Construct Constr [Term]
          | Destruct Constr Term Term
          | Project Term Term (Maybe Term)
          | -- build env/module
          | -- build variant (as an algebra?)

-- definitions:
--  bindings
--  variants (finite and otherwise)
--  classes
--  instances
--  specifying defaults/implicits
--  modules
--   linking
