import Data.Map as M
import Data.Set as S

type Name = String
type Lam = (Name, Term)

data Term = Var Name
          | App Term Term
          | Lam Lam
          | Val Value
  deriving Show

type Clo = (Lam, Env)
data Value = Clo Clo
           | Nada
  deriving Show

type Addr = Int
data Binding = Addr Addr
             | Local Value
  deriving Show
type Env = M.Map Name Binding
type Store = M.Map Addr Value

data Op = Halt
        | Eval Term Env Cont
        | Apply Clo Cont
  deriving Show

type Context = ([Op], Env)
type State = (Context, Store)
type StateMap = M.Map Context Store

inject term = Active (term, Halt, M.empty)

-- transit

tidl = ("x", Var "x")
tid = Lam tidl
tidid = App tid tid

runi (Active ctx) = transit ctx
runi st = st

run (Active ctx) = run $ transit ctx
run st = st

runlog st = runlog' [] st
  where runlog' hist (Active ctx) = runlog' (ctx:hist) $ transit ctx
        runlog' hist st = (st, reverse hist)
