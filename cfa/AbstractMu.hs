--{-# OPTIONS_GHC -XNoMonomorphismRestriction #-}

module AbstractMu where
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

type Prog = Lam
type ProgM = M.Map Label Lam

unionS xs = L.foldl' S.union S.empty xs
unionmapM f xs = L.foldl' M.union M.empty $ map f xs
unionmapS f xs = unionS $ map f xs
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
            deriving (Show, Ord, Eq)
-- data FullExpr = Expr Expr
--               | Call Call

free_lam ((bvars, call), _) = S.difference (free_call call) $ S.fromList bvars
free_call ((proc, args), _) = unionmapS free_expr (proc:args)
free_expr (RefEx (var, _)) = S.insert var S.empty
free_expr (LamEx lam) = free_lam lam
-- free (Expr expr) = free_expr expr
-- free (Call call) = free_call call
mget dict key = fromJust $ M.lookup key dict
bound progm lab = fst $ mget lab progm

type EvalSt = (Call, Conf, Time)
type ApplySt = (Proc, [Den], Conf, Time)
data State = Eval EvalSt
           | Apply ApplySt
data Result = FinalSt Den
            | NextSt EvalSt
              deriving (Show, Ord, Eq)
type EvalConfs = M.Map (Call, Time) Conf

type Conf = (VEnv, Measure)
type VEnv = M.Map Bind Den
type Bind = Var
type Measure = M.Map Bind Count
data Count = One | Many deriving (Show, Ord, Eq)

addCount :: Count -> Count -> Count
addCount _ _ = Many

veJoin ve1 ve2 = M.unionsWith S.union [ve1, ve2]
muJoin mu1 mu2 = M.unionsWith addCount [mu1, mu2]
confJoin (v1, m1) (v2, m2) = (veJoin v1 v2, muJoin m1 m2)

reachableMap kvs reached = M.difference kvs $ M.difference kvs reached
confReachable (ve, mu) reached = (reachable ve, reachable mu)
    where reachable = flip reachableMap reached

newconf = (M.empty, M.empty)
confve (ve, _) = ve
confmu (_, mu) = mu

type Den = S.Set Val
type Val = Proc
data Proc = Clo Clo | Halt deriving (Show, Ord, Eq)
type Clo = Lam
data Time = Time deriving (Show, Ord, Eq)

tsucc _ tm = tm

touched_den Halt = S.empty
touched_den (Clo lam) = free_lam lam
touched_eval (call, _, _) = free_call call
touched_apply (proc, args, _, _) =
    unionmapS touched_den $ S.elems $ unionS $ S.singleton proc:args
touched_bind venv bd = unionmapS touched_den $ S.elems $ mget venv bd

reachable_binds' ve seen bds =
    if S.null bds then seen
    else reachable_binds' ve (S.union seen bds') bds'
    where bds' = S.difference (unionmapS (touched_bind ve) (S.elems bds)) seen

reachable_binds'' ve seen bds = reachable_binds' ve (S.union seen bds) bds
reachable_binds ve bds = reachable_binds'' ve S.empty bds

--reachable_den venv seen den = reachable_binds'' venv seen $ touched_den den
reachable_eval st@(_, conf, _) =
    reachable_binds (confve conf) $ touched_eval st
reachable_apply st@(_, _, conf, _) =
    reachable_binds (confve conf) $ touched_apply st

zipWith_ _ [] [] = []
zipWith_ f (x:xs) (y:ys) = f x y : zipWith_ f xs ys
zipWith_ _ _ _ = error "zipWith_ list args must be of the same length"
zip_ xs ys = zipWith_ (,) xs ys

evalArg (RefEx (var, _)) venv = mget venv $ var
evalArg (LamEx lam) _ = S.singleton $ Clo lam

evalToApply :: EvalSt -> S.Set ApplySt
evalToApply st@(((pEx, aExs), _), conf, tm) = S.fromList apps
    where apps = map (\proc -> (proc, args, conf, tm')) $ S.elems procDen
          procDen = evalArg pEx $ confve conf
          args = map (\a -> evalArg a $ confve conf) aExs
          tm' = tsucc st tm
applyToEval :: ApplySt -> Result
applyToEval (Clo ((vars, call), _), args, conf, tm) =
    NextSt (call, (ve', mu'), tm)
    where ve' = veJoin ve veNew
          mu' = muJoin (confmu conf) muNew
          muNew = M.difference (M.fromList $ map (flip (,) One) vars) rebound
          rebound = M.filter (\x -> x) $ M.intersectionWith (==) ve veNew
          veNew = M.fromList $ zip_ vars args
          ve = (confve conf)
applyToEval (Halt, [arg], _, _) = FinalSt arg
applyToEval _ = error "halted with multiple values"

progInitSt prog args = ((Clo prog), args++[S.singleton Halt], newconf, Time)
evalProg prog args = applyToEval' $ progInitSt prog args

hasNext (FinalSt _) = False
hasNext _ = True
nextEval (NextSt st) = st
nextEval _ = error "attempted to continue from final state"

evalToConf seen key =
    case M.lookup key seen of
      Nothing -> newconf
      Just conf -> conf
isSubConf (ve1, _) (ve2, _) = M.isSubmapOfBy S.isSubsetOf ve1 ve2

setToMap ss = M.fromList $ map (\key -> (key, ())) $ S.elems ss

collected_eval st@(call, conf, tm) = (call, confReachable conf reached, tm)
    where reached = setToMap $ reachable_eval st
collected_apply st@(proc, args, conf, tm) = (proc, args, conf', tm)
    where conf' = confReachable conf reached
          reached = setToMap $ reachable_apply st
-- only need one of these, right?
evalToApply' = evalToApply . collected_eval
applyToEval' = applyToEval . collected_apply

widened confGlobal seen st@(call, conf, tm) =
    if isSubConf conf $ evalToConf seen key
    then Nothing
    else Just (conf', seen', (call, conf', tm))
    where seen' = M.insert key conf' seen
          conf' = confJoin confGlobal conf
          key = (call, tm)
-- non-widening...
widened' confGlobal seen st@(call, conf, tm) =
    if isSubConf conf $ conf'
    then Nothing
    else Just (cfg, seen', st')
    where seen' = M.insert key conf'' seen
          cfg = confJoin confGlobal conf
          st' = (call, conf'', tm)
          conf'' = confJoin conf' conf
          conf' = evalToConf seen key
          key = (call, tm)

reachable_states' confGlobal seen [] = (confGlobal, seen)
reachable_states' confGlobal seen (st:todo) =
    case widened' confGlobal seen st of
      Nothing -> reachable_states' confGlobal seen todo
      Just (confGlobal', seen', st') ->
          reachable_states' confGlobal' seen' todo'
          where todo' = S.elems $ S.union (S.fromList todo) nexts
                nexts = S.map nextEval $ S.filter hasNext $ results
                results = S.map applyToEval' $ evalToApply' st'
reachable_states states = reachable_states' newconf M.empty states
reachable_states_prog :: Clo -> [S.Set Proc] -> (Conf, EvalConfs)
reachable_states_prog prog args =
    reachable_states $ map nextEval $ filter hasNext [evalProg prog args]

----------------------------------------------------------------
mkRef var lab = RefEx (var, lab)
mkLam formals call lab = LamEx ((formals, call), lab)
mkCall proc args lab = ((proc, args), lab)
mkLet var expr call lab0 lab1 = mkCall (mkLam [var] call lab0) [expr] lab1

-- (let* ((id (lambda (x) x))
--        (a (id (lambda (z) (halt z))))
--        (b (id (lambda (y) (halt y)))))
--  (halt b))

LamEx example = (mkLam ["halt"] (mkLet "id" (mkLam ["x", "k"] (mkCall (mkRef "k" 0) [(mkRef "x" 1)] 7) 11)
                 (mkCall (mkRef "id" 2)
                         [(mkLam ["z"] (mkCall (mkRef "halt" 20) [(mkRef "z" 3)] 8) 17),
                          (mkLam ["a"]
                                 (mkCall (mkRef "id" 4)
                                         [(mkLam ["y"] (mkCall (mkRef "halt" 21) [(mkRef "y" 5)] 9) 18),
                                          (mkLam ["b"] (mkCall (mkRef "halt" 22) [(mkRef "b" 6)] 10) 19)] 12) 13)] 14) 15 16) 23)

(confGlob, states) = reachable_states_prog example []

--showConf conf = M.fromList conf

-- ("a",fromList [Clo ((["y"],((RefEx ("halt",21),[RefEx ("y",5)]),9)),18),Clo ((["z"],((RefEx ("halt",20),[RefEx ("z",3)]),8)),17)])

-- ("b",fromList [Clo ((["y"],((RefEx ("halt",21),[RefEx ("y",5)]),9)),18),Clo ((["z"],((RefEx ("halt",20),[RefEx ("z",3)]),8)),17)])

-- ("halt",fromList [Halt])

-- ("id",fromList [Clo ((["x","k"],((RefEx ("k",0),[RefEx ("x",1)]),7)),11)])

-- ("k",fromList [Clo ((["a"],((RefEx ("id",4),[LamEx ((["y"],((RefEx ("halt",21),[RefEx ("y",5)]),9)),18),LamEx ((["b"],((RefEx ("halt",22),[RefEx ("b",6)]),10)),19)]),12)),13),Clo ((["b"],((RefEx ("halt",22),[RefEx ("b",6)]),10)),19)])

-- ("x",fromList [Clo ((["y"],((RefEx ("halt",21),[RefEx ("y",5)]),9)),18),Clo ((["z"],((RefEx ("halt",20),[RefEx ("z",3)]),8)),17)])

-- collected

-- ("b",fromList [Clo ((["y"],((RefEx ("halt",21),[RefEx ("y",5)]),9)),18),Clo ((["z"],((RefEx ("halt",20),[RefEx ("z",3)]),8)),17)])

-- ("halt",fromList [Halt])

-- ("id",fromList [Clo ((["x","k"],((RefEx ("k",0),[RefEx ("x",1)]),7)),11)])

-- ("k",fromList [Clo ((["a"],((RefEx ("id",4),[LamEx ((["y"],((RefEx ("halt",21),[RefEx ("y",5)]),9)),18),LamEx ((["b"],((RefEx ("halt",22),[RefEx ("b",6)]),10)),19)]),12)),13),Clo ((["b"],((RefEx ("halt",22),[RefEx ("b",6)]),10)),19)])

-- ("x",fromList [Clo ((["y"],((RefEx ("halt",21),[RefEx ("y",5)]),9)),18),Clo ((["z"],((RefEx ("halt",20),[RefEx ("z",3)]),8)),17)])

-- new collected (context widened)

-- ("a",fromList [Clo ((["y"],((RefEx ("halt",21),[RefEx ("y",5)]),9)),18),Clo ((["z"],((RefEx ("halt",20),[RefEx ("z",3)]),8)),17)])

-- ("b",fromList [Clo ((["y"],((RefEx ("halt",21),[RefEx ("y",5)]),9)),18),Clo ((["z"],((RefEx ("halt",20),[RefEx ("z",3)]),8)),17)])

-- ("halt",fromList [Halt])

-- ("id",fromList [Clo ((["x","k"],((RefEx ("k",0),[RefEx ("x",1)]),7)),11)])

-- ("k",fromList [Clo ((["a"],((RefEx ("id",4),[LamEx ((["y"],((RefEx ("halt",21),[RefEx ("y",5)]),9)),18),LamEx ((["b"],((RefEx ("halt",22),[RefEx ("b",6)]),10)),19)]),12)),13),Clo ((["b"],((RefEx ("halt",22),[RefEx ("b",6)]),10)),19)])

-- ("x",fromList [Clo ((["y"],((RefEx ("halt",21),[RefEx ("y",5)]),9)),18),Clo ((["z"],((RefEx ("halt",20),[RefEx ("z",3)]),8)),17)])],

-- fromList [("a",Many),("b",One),("halt",Many),("id",Many),("k",Many),("x",Many)])
