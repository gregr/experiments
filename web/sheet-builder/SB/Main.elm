import SB.Model exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as J

viewDict vv dct = ul [] <| List.map (\(key, val) -> li [] [text (toString key ++ " => "), vv val]) <| Dict.toList dct
viewEnv env =
  let {terms, finished, uid} = env
  in ul [] [text "terms"
           ,viewDict (viewTerm env Set.empty) terms
           ,text "finished"
           ,viewDict (viewValue env Set.empty) finished
           ,text <| toString uid]
testView =
  let (result, env) = test
      d0 = case result of
            Ok value -> viewValue env Set.empty value
            Err msg -> text msg
      d1 = viewEnv env
  in div [] [div [] [text <| "ref: " ++ toString testRef], div [] [d0], div [] [d1]]
example = viewValue envEmpty Set.empty <| VList [LCElements [AString "test", ANumber <| NInt 55, ABool True, ANumber <| NFloat 3.4]]

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

viewUnit = span [] [text "()"]
viewBool vb = input [type' "checkbox", checked vb] []
viewString vs = input [value vs] []
viewInt vi = input [type' "number", value (toString vi)] []
viewFloat vf = input [type' "number", value (toString vf)] []

viewRef env pending ref  = text <| "TODO: ref " ++ toString ref
viewValueRef = viewRef
viewTermRef = viewRef

viewAtom viewRef env pending atom = case atom of
  ARef ref -> viewRef env pending ref
  AUnit -> viewUnit
  ABool vb -> viewBool vb
  AString vs -> viewString vs
  ANumber (NInt vi) -> viewInt vi
  ANumber (NFloat vf) -> viewFloat vf
viewValueAtom = viewAtom viewValueRef
viewTermAtom = viewAtom viewTermRef

viewListComponent viewAtom part = case part of
  LCElements atoms -> List.map viewAtom atoms
  LCSplice ref -> [text "TODO: splice"]
viewListComponents viewAtom parts = List.concatMap (viewListComponent viewAtom) parts
viewList viewAtom env pending parts
  = ul [] <| List.map (\item -> li [] [item]) <| viewListComponents (viewAtom env pending) parts
viewValueList = viewList viewValueAtom
viewTermList = viewList viewTermAtom

viewSheet viewRef viewAtom env pending { elements, input, output } =
  div [] [text "TODO: sheet"
         ,div [] [viewRef env pending input]
         ,div [] <| List.map (viewRef env pending) <| Set.toList elements
         ,div [] [viewAtom env pending output]]
viewValueSheet = viewSheet viewValueRef viewValueAtom
viewTermSheet = viewSheet viewTermRef viewTermAtom

viewValue env pending value = case value of
  VAtom atom -> viewValueAtom env pending atom
  VList parts -> viewValueList env pending parts
  VSheet sheet -> viewValueSheet env pending sheet

viewBinaryOp env pending op lhs rhs =
  let vl = viewTermAtom env pending lhs
      vr = viewTermAtom env pending rhs
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

viewTerm env pending term = case term of
  Literal atom -> viewTermAtom env pending atom
  TList parts -> viewTermList env pending parts
  TIteration {procedure, length} -> span [] [text "Iteration: ", viewTermRef env pending procedure, text " ", viewTermAtom env pending length]
  TSheet sheet -> viewTermSheet env pending sheet
  UnaryOp op atom -> span [] [text <| "(" ++ toString op ++ ")", viewTermAtom env pending atom]
  BinaryOp op lhs rhs -> viewBinaryOp env pending op lhs rhs
  SheetWith sref arg -> span [] [text <| "SheetWith " ++ toString sref ++ " ", viewTermAtom env pending arg]
  Access lref index -> span [] [viewTermRef env pending lref, text "[", viewTermAtom env pending index, text "]"]
  _ -> text <| "TODO: viewTerm: " ++ toString term

-- editor operations

-- views and issues:
-- literals, local/external (namespaced) identifiers, formulas and parameterized values, dependency bodies, list values, iterations (length, body)
-- root (stack? current fullscreen termUI; multiple frames?), focus, hover, composition-in-progress
-- new, copy, mirror, modify
-- bodies: name, move/shift

--type alias SimpleTermComposer = { focus }
-- composition stack for jumping out to create new nested terms, then back to original context?
