module CPS where
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

type Prog = Lam
type ProgM = M.Map Label Lam

unionmapM f xs = L.foldl' M.union M.empty $ map f xs
unionmapS f xs = L.foldl' S.union S.empty $ map f xs
labToLam_expr (LamEx (txt, lab)) = M.insert lab txt (labToLam_lam txt) 
labToLam_expr _ = M.empty
labToLam_call ((proc, args), _) = unionmapM labToLam_expr (proc:args)
labToLam_lam (_, call) = labToLam_call call

progm lam = labToLam_expr $ LamEx lam

type Var = String
type Ref = (Var, Label)
type LamTxt = ([Var], Call)
type Lam = (LamTxt, Label)
type CallTxt = (Expr, [Expr])
type Call = (CallTxt, Label)
type Label = Int

data Expr = RefEx Ref
          | LamEx Lam
-- data FullExpr = Expr Expr
--               | Call Call

free_lam ((bvars, call), _) = foldr S.delete (free_call call) bvars
free_call ((proc, args), _) = unionmapS free_expr (proc:args)
free_expr (RefEx (var, _)) = S.insert var S.empty
free_expr (LamEx lam) = free_lam lam
-- free (Expr expr) = free_expr expr
-- free (Call call) = free_call call
mget dict key = fromJust $ M.lookup key dict
bound progm lab = fst $ mget lab progm

type EvalSt = (Call, BEnv, Conf, Time)
type ApplySt = (Proc, [Den], Conf, Time)
data State = Eval EvalSt
           | Apply ApplySt
data Result = FinalSt Den
            | NextSt EvalSt

type Conf = VEnv
type VEnv = M.Map Bind Den
type Bind = (Var, Time)
type BEnv = M.Map Var Time

confve conf = conf
bind benv var = (var, (mget benv var))

type Den = Val
type Val = Proc
data Proc = Clo Clo | Halt
type Clo = (Lam, BEnv)
type Time = Int

tsucc _ tm = tm + 1

bindings benv vars = S.fromList $ map (bind benv) vars
touched_den Halt = S.empty
touched_den (Clo (lam, benv)) = bindings benv $ S.elems $ free_lam lam
touched_eval (call, benv, _, _) = bindings benv $ S.elems $ free_call call
touched_apply (proc, args, _, _) = unionmapS touched_den $ proc:args
touched_bind venv bd = touched_den $ mget venv bd

reachable_binds' ve seen bds =
    if S.null bds then seen
    else reachable_binds' ve (S.union seen bds') bds'
    where bds' = S.difference (unionmapS (touched_bind ve) (S.elems bds)) seen
reachable_binds'' ve seen bds = reachable_binds' ve (S.union seen bds) bds
reachable_binds ve bds = reachable_binds'' ve S.empty bds

--reachable_den venv seen den = reachable_binds'' venv seen $ touched_den den
reachable_eval st@(_, _, conf, _) =
    reachable_binds (confve conf) $ touched_eval st
reachable_apply st@(_, _, conf, _) =
    reachable_binds (confve conf) $ touched_apply st

zipWith_ _ [] [] = []
zipWith_ f (x:xs) (y:ys) = f x y : zipWith_ f xs ys
zipWith_ _ _ _ = error "zipWith_ list args must be of the same length"
zip_ xs ys = zipWith_ (,) xs ys

evalArg (RefEx (var, _)) benv venv = mget venv $ bind benv var
evalArg (LamEx lam) benv _ = Clo (lam, benv)

evalToApply :: EvalSt -> ApplySt
evalToApply st@(((pEx, aExs), _), benv, conf, tm) = (proc, args, conf, tm')
    where proc = evalArg pEx benv $ confve conf
          args = map (\a -> evalArg a benv $ confve conf) aExs
          tm' = tsucc st tm
applyToEval (Clo (((vars, call), _), benv), args, conf, tm) =
    NextSt (call, benv', conf', tm)
    where benv' = L.foldl' (\be var -> M.insert var tm be) benv vars
          conf' = L.foldl' (\ve (var, val) -> M.insert (var, tm) val ve)
                  (confve conf) $ zip_ vars args
applyToEval (Halt, [arg], _, _) = FinalSt arg
applyToEval _ = error "halted with multiple values"

progInitSt prog args = ((Clo (prog, M.empty)), args++[Halt], M.empty, 0)
evalProg prog args = applyToEval $ progInitSt prog args
