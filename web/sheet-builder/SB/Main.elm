import SB.Model exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as J

main = view editor

view editor = text <| toString editor

type alias TermUI =
  { ref : Ref
  , style : TermUIStyle
  }
type alias TermUIStyle =
  { layout : Maybe LayoutDirection
  , context : Maybe ContextVisibility
  }
type LayoutDirection = Vertical | Horizontal
-- collapse this to boolean? show deps or show formula/computation
type ContextVisibility
  = ValueVisible
  | FormulaVisible
  | DependenciesVisible
type alias Body = List (Name, TermUI)
type alias Editor = { root : Ref, env : Environment, bodies : Dict Ref Body }

tuisDefault = { layout = Nothing, context = Nothing }
tuiDefault ref = { ref = ref, style = tuisDefault }

sheetEmpty arg =
  TSheet { elements = Set.empty
         , input = arg
         , output = ARef arg }
newSheet = sheetEmpty $<$> newTerm (Literal AUnit) $>>= newTerm

editorEmpty =
  let (root, env) = newSheet envEmpty
  in { root = root, env = env, bodies = Dict.empty }
editor = editorEmpty

-- editor operations

-- views and issues:
-- literals, local/external (namespaced) identifiers, formulas and parameterized values, dependency bodies, list values, iterations (length, body)
-- root (stack? current fullscreen termUI; multiple frames?), focus, hover, composition-in-progress
-- new, copy, mirror, modify
-- bodies: name, move/shift

--type alias SimpleTermComposer = { focus }
-- composition stack for jumping out to create new nested terms, then back to original context?
