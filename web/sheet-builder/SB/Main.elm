import SB.Model exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as J

viewDict vv dct = ul [] <| List.map (\(key, val) -> li [] [text (toString key ++ " => "), vv val]) <| Dict.toList dct
viewEnv {terms, finished, uid} =
  ul [] [text "terms"
        ,viewDict viewTerm terms
        ,text "finished"
        ,viewDict viewValue finished
        ,text <| toString uid]
testView =
  let (result, env) = test
      d0 = case result of
            Ok value -> viewValue value
            Err msg -> text msg
      d1 = viewEnv env
  in div [] [div [] [text <| "ref: " ++ toString testRef], div [] [d0], div [] [d1]]
example = viewValue <| VList [LCElements [AString "test", ANumber <| NInt 55, ABool True, ANumber <| NFloat 3.4]]

main = div [] [div [] [example], div [] [testView]]

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

viewRef ref = text <| "TODO: ref " ++ toString ref
viewUnit = span [] [text "()"]
viewBool vb = input [type' "checkbox", checked vb] []
viewString vs = input [value vs] []
viewInt vi = input [type' "number", value (toString vi)] []
viewFloat vf = input [type' "number", value (toString vf)] []

viewAtom atom = case atom of
  ARef ref -> viewRef ref
  AUnit -> viewUnit
  ABool vb -> viewBool vb
  AString vs -> viewString vs
  ANumber (NInt vi) -> viewInt vi
  ANumber (NFloat vf) -> viewFloat vf

viewListComponent part = case part of
  LCElements atoms -> List.map viewAtom atoms
  LCSplice ref -> [text "TODO: splice"]
viewListComponents parts = List.concatMap viewListComponent parts
viewList parts = ul [] <| List.map (\item -> li [] [item]) <| viewListComponents parts

viewSheet { elements, input, output } =
  div [] [text "TODO: sheet"
         ,div [] [viewRef input]
         ,div [] <| List.map viewRef <| Set.toList elements
         ,div [] [viewAtom output]]

viewValue value = case value of
  VAtom atom -> viewAtom atom
  VList parts -> viewList parts
  VSheet sheet -> viewSheet sheet

viewBinaryOp op lhs rhs =
  let vl = viewAtom lhs
      vr = viewAtom rhs
      parts = case op of
                BArithmetic aop -> case aop of
                  BAdd -> [vl, text "+", vr]
                  BSub -> [vl, text "-", vr]
                  BMul -> [vl, text "*", vr]
                  BDiv -> [vl, text "/", vr]
                  BQuo -> [vl, text "//", vr]
                  BRem -> [vl, text "/r", vr]
                  BMod -> [vl, text "%", vr]
                  BLog -> [text "log", vl, vr]
                  BExp -> [vl, text "^", vr]
                _ -> [text <| toString op, vl, vr]
  in span [] <| List.intersperse (text " ") parts

viewTerm term = case term of
  Literal atom -> viewAtom atom
  TList parts -> viewList parts
  TIteration {procedure, length} -> span [] [text "Iteration: ", viewRef procedure, text " ", viewAtom length]
  TSheet sheet -> viewSheet sheet
  UnaryOp op atom -> span [] [text <| "(" ++ toString op ++ ")", viewAtom atom]
  BinaryOp op lhs rhs -> viewBinaryOp op lhs rhs
  SheetWith sref arg -> span [] [text <| "SheetWith " ++ toString sref ++ " ", viewAtom arg]
  Access lref index -> span [] [viewRef lref, text "[", viewAtom index, text "]"]
  _ -> text <| "TODO: viewTerm: " ++ toString term

-- editor operations

-- views and issues:
-- literals, local/external (namespaced) identifiers, formulas and parameterized values, dependency bodies, list values, iterations (length, body)
-- root (stack? current fullscreen termUI; multiple frames?), focus, hover, composition-in-progress
-- new, copy, mirror, modify
-- bodies: name, move/shift

--type alias SimpleTermComposer = { focus }
-- composition stack for jumping out to create new nested terms, then back to original context?
