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
testRoots =
  let style = { tuisDefault | context = Just DependenciesVisible }-- FormulaVisible }
      tuis = List.map (\ref -> { ref = ref, style = style }) testRefs
  in tuis
testView =
  let (rtrs, env) = forM1 [] testRoots
        (\tui trs ->
          flip (::) trs $<$> ((,) tui $<$> evalRef Set.empty tui.ref)) testEnv
      present (tui, result) = case result of
        Ok value -> div [] [text <| "ref: " ++ toString tui.ref
                           ,viewTermUI env Set.empty styleDefault tui]
        Err msg -> text <| "error: " ++ msg
      vtuis = List.map present <| List.reverse rtrs
      venv = viewEnv env
  in div [] <| vtuis ++ [div [] [venv]]
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
List ContextVisibility -- include new value-specific properties (openness, length/window, etc.)
type alias Body = List (Name, TermUI) -- open/closed list for formula args
type alias Editor = { root : Ref, env : Environment, bodies : Dict Ref Body }

styleDefault = { layout = Vertical, context = ValueVisible }
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

listItem item = li [] [item]

refBody ref env =
  let deps = List.sort <| Set.toList <| refTermDirectDeps ref env
      depName ref = "ref " ++ toString ref
      names = List.map depName deps
      tuis = List.map tuiDefault deps
  in List.map2 (,) names tuis

viewBody env pending ntuis =
  let item (name, tui) =
    li [] [text <| name ++ ": ",viewTermUI env pending styleDefault tui]
  in ul [] <| List.map item ntuis

viewTermUI env pending parentStyle {ref, style} =
  let layout = Maybe.withDefault parentStyle.layout style.layout
      context = Maybe.withDefault parentStyle.context style.context
      style' = {layout = layout, context = context}
      value = VAtom (ARef ref) `Maybe.withDefault` refValue ref env
      vv = div [] [text ("value: "), viewValue env pending value]
      tv = div [] [text ("formula: "), viewTerm env pending (refTerm ref env)]
  in case context of
    ValueVisible -> div [] [vv]
    FormulaVisible -> div [] [tv, vv]
    DependenciesVisible ->
      div [] [viewBody env pending <| refBody ref env, tv, vv]

viewUnit = span [] [text "()"]
viewBool vb = input [type' "checkbox", checked vb] []
viewString vs = input [value vs] []
viewInt vi = input [type' "number", value (toString vi)] []
viewFloat vf = input [type' "number", value (toString vf)] []

viewValueRef env pending ref =
  if Set.member ref pending then text <| "cyclic ref: " ++ toString ref
  else text ("unevaluated ref: " ++ toString ref) `Maybe.withDefault`
    (viewValue env (Set.insert ref pending) `Maybe.map` refValue ref env)
viewTermRef env pending ref = text <| "ref: " ++ toString ref

viewAtom viewRef env pending atom = case atom of
  ARef ref -> viewRef env pending ref
  AUnit -> viewUnit
  ABool vb -> viewBool vb
  AString vs -> viewString vs
  ANumber (NInt vi) -> viewInt vi
  ANumber (NFloat vf) -> viewFloat vf
viewValueAtom x = viewAtom viewValueRef x
viewTermAtom x = viewAtom viewTermRef x

viewListComponent viewAtom part = case part of
  LCElements atoms -> List.map viewAtom atoms
  LCSplice ref -> [text "TODO: splice"]
viewListComponents viewAtom parts = List.concatMap (viewListComponent viewAtom) parts
viewList viewAtom env pending parts
  = ol [start 0] <| List.map listItem <| viewListComponents (viewAtom env pending) parts
viewValueList x = viewList viewValueAtom x
viewTermList x = viewList viewTermAtom x

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

-- TODO:
-- crawl and eval tuis in open bodies (DependenciesVisible) reachable from roots
-- implicit bodies
-- explicit bodies
-- iterations, sheets and context/dependency bodies
-- namespaced identifiers in terms
-- tracking bodies for newly-instantiated sheets and their contents
-- styling
-- editing/composition
