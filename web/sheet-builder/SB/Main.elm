module SB.Main where

import Dict exposing (Dict)
import Html exposing (..)

main = text "test"

type Term env leaf
  = TAtom Atom
  | TList (List leaf)
  | TModule (ModuleTerm env leaf)
  | TModuleApply leaf
  | TModuleUnite leaf leaf
  | TModuleKeys leaf
  | TListLength leaf
  | TListAppend leaf leaf
  | TGet leaf (Path leaf)
  | TPut leaf (Path leaf) leaf
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
type alias Name = Atom
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
forM = forM_ pure (>>=)

termMap envOp op term = case term of
  TAtom atom -> TAtom atom
  TList xs -> TList <| List.map op xs
  TModule mt ->
    TModule {mt | args = List.map (\(name, arg) -> (name, op arg)) mt.args
                , env = envOp mt.env}
  TModuleApply mod -> TModuleApply <| op mod
  TModuleUnite m0 m1 -> TModuleUnite (op m0) (op m1)
  TModuleKeys mod -> TModuleKeys (op mod)
  TListLength list -> TListLength (op list)
  TListAppend l0 l1 -> TListAppend (op l0) (op l1)
  TGet src ps -> TGet (op src) (List.map op ps)
  TPut src ps val -> TPut (op src) (List.map op ps) (op val)
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
estateApply {source, env} {modules} =
  let {definition, args} = source
  in pure1 (("Unknown module: " ++ toString definition) `Result.fromMaybe`
            Dict.get definition modules) >>=
     \definition ->
     let {params, locals, procedure} = definition
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
            comps = envResolveModule env' definition
            final = -1 `Maybe.withDefault`
              List.head (List.reverse stepResultsRefs)
        in estateRefComputationsSet comps $*> pure final

-- TODO: evaluation w/ provenance
-- provenance includes: originating ModuleContext: the step provides the Term

-- decouple computational history from editor history?
-- factor out zoom/path info; the rest is computational?

-- TODO: small state previews
