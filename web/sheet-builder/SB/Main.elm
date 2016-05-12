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

-- TODO: small state previews
