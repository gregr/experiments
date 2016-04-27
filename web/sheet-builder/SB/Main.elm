import SB.Model exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as J

main = viewValue <| VList [LCElements [AString "test", AInt 55, ABool True, AFloat 3.4]]

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

viewRef ref = text "TODO: ref"
viewUnit = span [] []
viewBool vb = input [type' "checkbox", checked vb] []
viewString vs = input [value vs] []
viewInt vi = input [type' "number", value (toString vi)] []
viewFloat vf = input [type' "number", value (toString vf)] []

viewAtom atom = case atom of
  ARef ref -> viewRef ref
  AUnit -> viewUnit
  ABool vb -> viewBool vb
  AString vs -> viewString vs
  AInt vi -> viewInt vi
  AFloat vf -> viewFloat vf

viewListComponent part = case part of
  LCElements atoms -> List.map viewAtom atoms
  LCSplice ref -> [text "TODO: splice"]
viewListComponents parts = List.concatMap viewListComponent parts
viewList parts = ul [] <| List.map (\item -> li [] [item]) <| viewListComponents parts

viewSheet { elements, input, output } =
  div [] [div [] [viewRef input]
         ,div [] <| List.map viewRef <| Set.toList elements
         ,div [] [viewAtom output]]

viewValue value = case value of
  VAtom atom -> viewAtom atom
  VList parts -> viewList parts
  VSheet sheet -> viewSheet sheet

-- editor operations

-- views and issues:
-- literals, local/external (namespaced) identifiers, formulas and parameterized values, dependency bodies, list values, iterations (length, body)
-- root (stack? current fullscreen termUI; multiple frames?), focus, hover, composition-in-progress
-- new, copy, mirror, modify
-- bodies: name, move/shift

--type alias SimpleTermComposer = { focus }
-- composition stack for jumping out to create new nested terms, then back to original context?
