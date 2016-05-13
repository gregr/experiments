module SB.Main where

import Dict exposing (Dict)
import Html exposing (..)

main = text "test"

type Term
  = TAtom Atom
  | TList (List Term)
  | TModule ModuleTerm
  | TCurrent
  | TParam Param
  | TModuleApply Term
  | TModuleKeys Term
  | TListLength Term
  | TGet Term (Path Term)
type Action
  = ARepeat Term (List Action)
  | AWhen Term (List Action)
  | AStep Step (Path Term)
type Step
  = SListAppend Term
  | SModuleAdd Term Term
  | SPut Term
  | SDelete
  | SDefineParam Name
  | SUndefineParam Name
  | SDefineRecursive Name
  | SUndefineRecursive Name
type Value = VAtom Atom | VList (List Ref) | VModule Closure
type alias ModuleTerm = Module ()
type alias Closure = Module ClosureEnv
type alias ClosureEnv = List (Bindings Ref)
type Module env = Module
  { env : env
  , params : List Name
  , args : Bindings Term
  , recDefs : Bindings ModuleTerm
  , procedure : List Action
  }
type alias Bindings rhs = List (Name, rhs)
type Atom
  = AUnit
  | ABool Bool
  | AString String
  | ANumber Number
type alias Path segment = List segment
type Number = NInt Int | NFloat Float
type alias Param = { levelsUp : Int, index : Int }
type alias Name = Atom
type alias Ref = Int

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

type alias ActionHistoryResult =
  { history : ActionHistory, result : EvalState }
type alias ActionHistory = List ActionHistoryState
type alias ActionHistoryState =
  { initialState : EvalState
  , action : Action
  , trace : ActionHistoryTrace
  }
type ActionHistoryTrace
  = AHRepeat (List ActionHistory)
  | AHWhen ActionHistory
  | AHStep

type ACProp = ACRepeat Term Int | ACWhen Term
type alias ActionState = (Action, EvalState)
type alias ActionContext =
  { earlier : List ActionState
  , later : List ActionState
  }
type alias ActionCursor =
  { focus : ActionState
  , context : ActionContext
  , contextOuter : List (ACProp, ActionContext)
  }
type alias ModuleContext =
  { value : Ref  -- TODO: covered by estate.current?
  , subpath : Path Atom
  , zooms : List (Path Atom)
  , action : ActionCursor
  , mod : Closure
  }
type alias ModuleCursor =
  { context : ModuleContext
  , contextOuter : List { defpath : Maybe Name, context : ModuleContext }
  }

type Editor = Editor
  { cursor : ModuleCursor
  , history : List Editor
  , undone : List Editor
  }

type alias EvalState =
  { values : Dict Ref Value
  --, origins : Dict Ref SomeKindOfContext  -- TODO: provenance
  , env : ClosureEnv
  , current : Ref
  , outer : List Ref
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
