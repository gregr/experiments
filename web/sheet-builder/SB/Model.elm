module SB.Model where

import Dict exposing (Dict)
import Set exposing (Set)

type TermT ref
  = Literal (Atom ref)
  | TList (List (ListConstruction ref))
  | TSheet (Sheet ref)
  | Identify (Identifier ref)
  | Access (Accessor ref)
  | UnaryOp UOp (TermT ref)
  | BinaryOp BOp (TermT ref) (TermT ref)
  --| Apply
type UOp
  = UNot
  | UAddInvert
  | URound
  | UFloor
  | UCeiling
type BOp
  = BCompareEQ
  | BCompareLT
  | BCompareLTE
  | BCompareGT
  | BCompareGTE
  | BAdd
  | BSubtract
  | BMultiply
  | BDivide
  | BStringConcat
  | BStringSubrange
  | BStringReplace
type Atom ref
  = ARef ref
  | AUnit
  | ABool Bool
  | AString String
  | AInt Int
  | AFloat Float
type alias Accessor ref = { collection : ref, key : Atom ref }
type alias Identifier ref = { namespace : ref, nref : NamedRef ref }
type alias NamedRef ref = { name : Name, ref : ref }

type Value = VAtom (Atom Ref) | VList (List Ref) | VSheet (Sheet Ref)
type ListConstruction ref = LCLiteral (List ref) | LCIterated (Iteration ref)
type alias Sheet ref =
  { orientation : LayoutOrientation
  , elements : List (NamedRef ref)
  }
type LayoutOrientation = Vertical | Horizontal

type alias Name = String
type alias Ref = Int

type alias FlatTerm = TermT Ref
-- TODO: alternative terms
--type alias Program = Dict Ref FlatTerm
--type alias ProgramState = Dict Ref TermState
--type alias TermState =
  --{ term : FlatTerm
  --, result : Maybe Value
  ----, dependencies : Set Ref
  ----, dependants : Set Ref
  --}

-- TODO:
-- deletion, (partial)transferring dependencies

type alias Level = Int
type IRef = IAbsRef Ref | IRelRef (Level, Ref) | IPos Level
type alias IterTerm = TermT IRef

type alias Iteration ref =
  { body : IterTerm
  , locals : List IterTerm
  , length : ref
  }

afloat atom = case atom of
  AInt int -> toFloat int
  AFloat float -> float
  -- ARef ref -> -- TODO: recurse on referenced value
  _ -> 0  -- TODO: Nothing

eval term = case term of
  Literal atom -> atom
  BinaryOp op lhs rhs ->
    let vlhs = afloat <| eval lhs
        vrhs = afloat <| eval rhs
        vres = case op of
          BAdd -> vlhs + vrhs
          BSubtract -> vlhs - vrhs
          BMultiply -> vlhs * vrhs
          BDivide -> vlhs / vrhs
          _ -> 0  -- TODO
    in AFloat vres
  _ -> AUnit

example = BinaryOp BMultiply (Literal <| AFloat 4.1) (Literal <| AInt 3)
test = eval example
