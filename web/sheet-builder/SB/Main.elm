module SB.Main where

import Dict exposing (Dict)
import Html exposing (..)

main = text "test"

type Term leaf
  = TAtom Atom
  | TIdent Ident
  | TList (List leaf)
  | TModule (ModuleTerm leaf)
  | TModuleApply leaf
  | TModuleUnite leaf leaf
  | TModuleKeys leaf
  | TListLength leaf
  | TListAppend leaf leaf
  | TGet leaf (Path leaf)
  | TPut leaf (Path leaf) leaf
  | TDelete leaf (Path leaf)
type SimpleTerm = STerm (Term SimpleTerm)
type TemporalTerm
  = TTTerm (Term TemporalTerm)
  | TTPreviousState

type Value = VRef Ref | VAtom Atom | VList (List Ref) | VModule ModuleClosure
type alias ModuleTerm leaf = { definition : ModuleDef, args : Bindings leaf }
type alias ModuleClosure = { source : ModuleTerm Ref, env : Env }
type alias EnvFrame = { bindings : Bindings Ref, local : Dict Ref Ref }
type alias Env = List EnvFrame
type alias ModuleDef =
  { params : List Name
  , locals : Dict Ref Term
  , procedure : List TemporalTerm
  }
type alias Bindings rhs = List (Name, rhs)
type Atom
  = AUnit
  | ABool Bool
  | AString String
  | ANumber Number
type Number = NInt Int | NFloat Float
type IdentIndex = Param Int | LocalIdent Ref
type alias Ident = { levelsUp : Int, index : IdentIndex }
type alias Name = Atom
type alias Ref = Int
type alias Path segment = List segment

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



type Source = SourceTerm Ref | SourceAction Ref
type alias Computation = { source : Source, env : Env, state : Ref }

type alias EvalState =
  { values : Dict Ref Value
  , computations : Dict Ref Computation
  , currentState : Ref
  , env : Env
  , uid : Int
  }

forFoldM_ pure bind acc xs op =
  let loop acc xs = case xs of
    [] -> pure acc
    yy::ys -> bind (op yy acc) (\next -> loop next ys)
  in loop acc xs
-- Would need two copies of bind to satisfy type checker...
--forM_ pure bind bind2 xs op =
  --forFoldM_ pure bind [] xs (\xx acc -> bind2 (op xx) (\yy -> pure (yy :: acc)))
forM_ pure bind xs op =
  let loop results xs = case xs of
    [] -> pure results
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

envEmpty = []
envEqual e0 e1 =
  let all bools = List.foldl (&&) True bools
      bindingsEq bs0 bs1 = all <| List.map2 (==) bs0 bs1
  in all <| List.map2 bindingsEq e0 e1
envExtendParams params args env =
  let argDict = Dict.fromList args
      binding param =
        (,) param @<$>
        (("Unbound parameter: " ++ toString param) `Result.fromMaybe`
         Dict.get param argDict)
  in flip (::) env @<$> forM0 params binding

estateEmpty =
  { values = Dict.empty
  , terms = Dict.fromList [(0, TAtom AUnit)]
  , env = envEmpty
  , current = 0
  , outer = []
  , uid = 1
  }
estateRefNew estate = (estate.uid, {estate | uid = estate.uid + 1})
estateRefNewMulti count estate =
  let next = estate.uid + count
  in ([estate.uid .. next - 1], {estate | uid = next})
estateRefTermGet ref estate =
  let term = case Dict.get ref estate.terms of
        Just term -> term
        Nothing -> TAtom AUnit  -- Debug.crash/log?
  in (term, estate)
estateRefTermSet term ref estate =
  ((), { estate | terms = Dict.insert ref term estate.terms })
estateRefTermNew term = estateRefNew $>>= estateRefTermSet term
estateRefValueGet ref estate = (Dict.get ref estate.values, estate)
estateRefValueSet value ref estate =
  ((), { estate | values = Dict.insert ref value estate.values })
estateEnvUpdate update estate =
  (pure1 (update estate.env) >>=
   \env estate -> (pure0 (), {estate | env = env})) estate
estateEnvExtendParams params args =
  estateEnvUpdate (envExtendParams params args)

--estatePerform mod.procedure

  -- traverse/perform actions and remember states: procedure : List Action

--estateLeave estate = ...
-- if changes, build new module term, pop estate, put new module term at path


-- TODO: evaluation w/ provenance
-- provenance includes: originating ModuleContext: the step provides the Term

-- decouple computational history from editor history?
-- factor out zoom/path info; the rest is computational?

-- TODO: small state previews
