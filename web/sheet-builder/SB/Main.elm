module SB.Main where

import Dict exposing (Dict)
import Set exposing (Set)
import Html exposing (..)

main = text "test"

type Term env leaf
  = TAtom Atom
  | TList (List leaf)
  | TModule (ModuleTerm env leaf)
  | TModuleApply leaf
  | TModuleUnite leaf leaf
  | TListLength leaf
  | TListAppend leaf leaf
  | TGet leaf (Path leaf)
  | TPut1 leaf leaf leaf
  | TDelete leaf (Path leaf)
type alias GlobalTerm = Term Env Ref
type alias LocalTerm = Term () Ident
type alias TemporalTerm = Term () TemporalIdent
type alias Ident = { levelsUp : Int, index : IdentIndex }
type IdentIndex = Param Int | LocalIdent Ref
type TemporalIdent = TIIdent Ident | TIPreviousState

type Value = VRef Ref | VAtom Atom | VList (List Ref) | VModule (ModuleTerm Env Ref)
type alias ModuleTerm env leaf =
  { definition : Ref
  , args : Bindings leaf
  , env : env
  }
type alias EnvFrame =
  { definition : Ref
  , bindings : Bindings Ref
  , locals : Dict Ref Ref
  , results : List Ref
  }
type alias Env = List EnvFrame
type alias ProcedureStep = { locals : Dict Ref TemporalTerm, result : Ref }
type alias ModuleDef =
  { params : List Name
  , locals : Dict Ref LocalTerm
  , procedure : List ProcedureStep
  , uid : Int
  }
type alias Bindings rhs = List (Name, rhs)
type Atom
  = AUnit
  | ABool Bool
  | AString String
  | ANumber Number
type Number = NInt Int | NFloat Float
type alias Name = String
type alias Ref = Int
type alias Path segment = List segment

type alias Program =
  { modules : Dict Ref ModuleDef
  , uid : Int
  }
type alias ComputationSource = { env : Env, local : Ref }
type alias Computation = { term : GlobalTerm, source : ComputationSource }
type alias EvalState =
  { values : Dict Ref Value
  , computations : Dict Ref Computation
  , program : Program
  , uid : Int
  }

type Navigation
  = Ascend Int
  | Descend (Path Atom)
  | Shift Int
  | ZoomIn
  | ZoomOut
  | ModuleEnter
  | ModuleLeave
  | DefinitionEnter Name
  | ActionNext
  | ActionPrevious
  | ActionIterationNext Int
  | ActionIterationPrevious Int

forFoldM_ pure bind acc xs op =
  let loop acc xs = case xs of
    [] -> pure acc
    yy::ys -> bind (op yy acc) (\next -> loop next ys)
  in loop acc xs
-- Would need two copies of bind to satisfy type checker...
-- TODO: also, reverse result
--forM_ pure bind bind2 xs op =
  --forFoldM_ pure bind [] xs (\xx acc -> bind2 (op xx) (\yy -> pure (yy :: acc)))
forM_ pure bind xs op =
  let loop results xs = case xs of
    [] -> pure <| List.reverse results
    yy::ys -> bind (op yy) (\result -> loop (result :: results) ys)
  in loop [] xs

pure0 = Ok
join0 result = case result of
  Err err -> Err err
  Ok ok -> ok
(@>>=) = Result.andThen
infixl 1 @>>=
(@*>) p0 p1 = p0 @>>= \_ -> p1
infixl 4 @*>
(@<*) p0 p1 = p0 @>>= \r0 -> p1 @>>= \_ -> pure0 r0
infixl 4 @<*
(@<*>) p0 p1 = p0 @>>= \f0 -> p1 @>>= \r1 -> pure0 (f0 r1)
infixl 4 @<*>
(@<$>) f0 p1 = pure0 f0 @<*> p1
infixl 4 @<$>
forM0 = forM_ pure0 (@>>=)

pure1 val state = (val, state)
($>>=) p0 fp1 state = let (result, state') = p0 state
                      in fp1 result state'
infixl 1 $>>=
($*>) p0 p1 = p0 $>>= \_ -> p1
infixl 4 $*>
($<*) p0 p1 = p0 $>>= \r0 -> p1 $>>= \_ -> pure1 r0
infixl 4 $<*
($<*>) p0 p1 = p0 $>>= \f0 -> p1 $>>= \r1 -> pure1 (f0 r1)
infixl 4 $<*>
($<$>) f0 p1 = pure1 f0 $<*> p1
infixl 4 $<$>
forM1 = forM_ pure1 ($>>=)

pure val state = (Ok val, state)
fail msg = pure1 (Err msg)
(>>=) p0 fp1 state = let (result, state') = p0 state
                     in case result of
                       Err err -> (Err err, state')
                       Ok ok -> fp1 ok state'
infixl 1 >>=
(*>) p0 p1 = p0 >>= \_ -> p1
infixl 4 *>
(<*) p0 p1 = p0 >>= \r0 -> p1 >>= \_ -> pure r0
infixl 4 <*
(<*>) p0 p1 = p0 >>= \f0 -> p1 >>= \r1 -> pure (f0 r1)
infixl 4 <*>
(<$>) f0 p1 = pure f0 <*> p1
infixl 4 <$>
forFoldM = forFoldM_ pure (>>=)
forM = forM_ pure (>>=)

termMap envOp op term = case term of
  TAtom atom -> TAtom atom
  TList xs -> TList <| List.map op xs
  TModule mt ->
    TModule {mt | args = List.map (\(name, arg) -> (name, op arg)) mt.args
                , env = envOp mt.env}
  TModuleApply mod -> TModuleApply <| op mod
  TModuleUnite m0 m1 -> TModuleUnite (op m0) (op m1)
  TListLength list -> TListLength (op list)
  TListAppend l0 l1 -> TListAppend (op l0) (op l1)
  TGet src ps -> TGet (op src) (List.map op ps)
  TPut1 src seg val -> TPut1 (op src) (op seg) (op val)
  TDelete src ps -> TDelete (op src) (List.map op ps)

envEmpty = []
envResolveIdent env {levelsUp, index} =
  let (bindings, locals) =
        Maybe.withDefault ([], Dict.empty) <|
        (\frame -> (frame.bindings, frame.locals)) `Maybe.map`
        List.head (List.drop levelsUp env)
  in -1 `Maybe.withDefault` case index of
    Param idx -> snd `Maybe.map` List.head (List.drop idx bindings)
    LocalIdent ref -> Dict.get ref locals
envResolveTemporalIdent env previous ident = case ident of
  TIIdent id -> envResolveIdent env id
  TIPreviousState -> previous
envResolveLocalTerm env = termMap (\_ -> env) (envResolveIdent env)
envResolveTemporalTerm env previous =
  termMap (\_ -> env) (envResolveTemporalIdent env previous)
envResolveModule env {locals, procedure} =
  let localToGlobal =
        Maybe.withDefault Dict.empty (.locals `Maybe.map` List.head env)
      global lref = -1 `Maybe.withDefault` Dict.get lref localToGlobal
      lts = Dict.toList locals
      lts' = List.map (\(lref, term) ->
                       (global lref, { term = envResolveLocalTerm env term
                                     , source = { env = env
                                                , local = lref } })) lts
      (plts, _) = forM1 procedure
        (\{locals, result} previous ->
          let lts = Dict.toList locals
              lts' = List.map (\(lref, term) ->
                (global lref, { term = envResolveTemporalTerm env previous term
                              , source = { env = env
                                         , local = lref } })) lts
              rref = global result
          in (lts', rref)) -1
  in Dict.fromList <| List.concat <| lts' :: plts

estateEmpty =
  { values = Dict.empty
  , computations = Dict.empty
  , uid = 0
  }
estateRefNewMulti count estate =
  let next = estate.uid + count
  in ([estate.uid .. next - 1], {estate | uid = next})
estateRefsForLocals locals =
  Dict.fromList << List.map2 (,) (Dict.keys locals) $<$>
  estateRefNewMulti (Dict.size locals)
estateRefTermGet ref estate =
  let term = case Dict.get ref estate.computations of
        Just {term} -> term
        Nothing -> TAtom AUnit
  in (term, estate)
estateRefEnvGet ref estate =
  let env = case Dict.get ref estate.computations of
        Just {source} -> source.env
        Nothing -> []
  in (env, estate)
estateRefComputationsSet computations estate =
  ((), {estate | computations = Dict.union computations estate.computations})
estateRefValueGet ref estate =
  (("Missing value for: " ++ toString ref) `Result.fromMaybe`
   Dict.get ref estate.values, estate)
estateRefValueSet value ref estate =
  ((), { estate | values = Dict.insert ref value estate.values })
estateModulesGet estate = (estate.program.modules, estate)
estateApply {definition, args, env} =
  estateModulesGet $>>= \modules ->
    pure1 (("Unknown module: " ++ toString definition) `Result.fromMaybe`
          Dict.get definition modules) >>=
      \def ->
      let {params, locals, procedure} = def
          stepLocals = List.map .locals procedure
          argDict = Dict.fromList args
          binding param =
            (,) param @<$>
            (("Unbound parameter: " ++ toString param) `Result.fromMaybe`
            Dict.get param argDict)
      in pure1 (forM0 params binding) >>=
        \bindings -> estateRefsForLocals locals $>>=
        \localsRefs -> forM1 stepLocals estateRefsForLocals $>>=
        \stepLocalsRefs ->
        let allLocalsRefs = List.foldl Dict.union localsRefs stepLocalsRefs
            stepResultsRefs =
              List.map (\{result} -> -1 `Maybe.withDefault`
                        Dict.get result allLocalsRefs) procedure
            frame = { definition = definition
                    , bindings = bindings
                    , locals = allLocalsRefs
                    , results = stepResultsRefs }
            env' = frame :: env
            comps = envResolveModule env' def
            final = -1 `Maybe.withDefault`
              List.head (List.reverse stepResultsRefs)
        in estateRefComputationsSet comps $*> pure final

vsimple ref value = case value of
  VAtom atom -> VAtom atom
  _ -> VRef ref
vatom value = case value of
  VAtom atom -> pure atom
  VRef ref -> estateRefValueGet ref >>= vatom
  _ -> fail <| "expected an atom but found: " ++ toString value
vlist value = case value of
  VList xs -> pure xs
  VRef ref -> estateRefValueGet ref >>= vlist
  _ -> fail <| "expected a list but found: " ++ toString value
vmod value = case value of
  VModule mod -> pure mod
  VRef ref -> estateRefValueGet ref >>= vmod
  _ -> fail <| "expected a module but found: " ++ toString value

nint num = case num of
  NInt int -> int
  NFloat float -> round float
anumber atom = case atom of
  ANumber num -> Ok num
  _ -> Err "expected a number"
aint atom = nint @<$> anumber atom
astring atom = case atom of
  AString string -> Ok string
  _ -> Err "expected a string"
vint val = vatom val >>= aint >> pure1
vstring val = vatom val >>= astring >> pure1

vget1 pending segment vsrc = (case vsrc of
  VModule mod -> vstring segment >>=
    (\index -> ("unbound field: " ++ toString index)
    `Result.fromMaybe` Dict.get index (Dict.fromList mod.args)) >> pure1
  VList xs -> vint segment >>=
    (\index -> ("index out of bounds: " ++ toString index)
    `Result.fromMaybe` List.head (List.drop index xs)) >> pure1
  _ -> fail <| "cannot get '" ++ toString segment ++
    "' of simple value: " ++ toString vsrc) >>= evalRef pending
vget pending segments vsrc = forFoldM vsrc segments (vget1 pending)
vput1 pending val segment vsrc = case vsrc of
  VModule mod -> VModule <<
    (\index -> {mod | args = Dict.toList (Dict.insert index val
                                          (Dict.fromList mod.args))}) <$>
    vstring segment
  VList xs -> VList <<
    (\index -> List.append (List.take index xs) (val :: List.drop index xs))
    <$> vint segment
  _ -> fail <| "cannot put '" ++ toString segment ++
    "' of simple value: " ++ toString vsrc

moduleUnite m0 m1 =
  {m0 | args = Dict.toList <| Dict.fromList <| m0.args ++ m1.args}

evalRef pending ref = estateRefValueGet ref $>>= \mval -> case mval of
  Ok val -> pure <| vsimple ref val
  Err _ ->
    if Set.member ref pending then pure1 <| Err "TODO: cyclic computation"
    else (estateRefTermGet ref $>>= eval (Set.insert ref pending)) >>=
         \val -> estateRefValueSet val ref $*> pure (vsimple ref val)

eval pending term = case term of
  TAtom atom -> pure <| VAtom atom
  TList xs -> pure <| VList xs
  TModule mt -> pure <| VModule mt
  TModuleApply mod ->
    evalRef pending mod >>= vmod >>= estateApply >>= evalRef pending
  TModuleUnite m0 m1 ->
    VModule <$> (moduleUnite <$>
      (evalRef pending m0 >>= vmod) <*> (evalRef pending m1 >>= vmod))
  TListLength list -> VAtom << ANumber << NInt << List.length <$>
    (evalRef pending list >>= vlist)
  TListAppend l0 l1 ->
    VList <$> (List.append <$>
      (evalRef pending l0 >>= vlist) <*> (evalRef pending l1 >>= vlist))
  TGet src path -> evalRef pending src >>=
    \vsrc -> forM path (evalRef pending) >>=
    \segments -> vget pending segments vsrc
  TPut1 src segment val -> evalRef pending src >>=
    \vsrc -> evalRef pending segment >>=
    \vseg -> vput1 pending val vseg vsrc
  _ -> pure <| VAtom AUnit
