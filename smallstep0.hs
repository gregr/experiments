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

type Env = M.Map Name Value

data Cont = Halt
          | Eval Term Env Cont
          | Apply Clo Cont
  deriving Show

type Context = (Term, Cont, Env)
data State = Done Value
           | Stuck Context String
           | Active Context
  deriving Show

inject term = Active (term, Halt, M.empty)

transit (Var nm, knt, env) =
  case M.lookup nm env of
    Just val -> Active (Val val, knt, env)
    Nothing -> Stuck (Var nm, knt, env) $ "reference to unbound variable: " ++ nm
transit (App proc arg, knt, env) = Active (proc, Eval arg env knt, env)
transit (Lam lam, knt, env) = Active (Val $ Clo (lam, env), knt, env)
transit (Val (Clo clo), Eval arg aenv knt, env) = Active (arg, Apply clo knt, aenv)
transit (Val arg, Apply ((nm, body), lenv) knt, env) = Active (body, knt, benv)
  where benv = M.insert nm arg lenv
transit (Val val, Halt, env) = Done val
transit ctx = Stuck ctx "invalid state"

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
