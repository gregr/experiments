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
  | BArithmetic (Float -> Float -> Float)
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
type ListConstruction ref
  = LCElement (Atom ref)
  | LCIteration (Iteration ref)
  | LCSplice ref
type alias Sheet ref = { elements : List (NamedRef ref) }
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

type alias Environment =
  { terms : Dict Ref FlatTerm
  , finished : Dict Ref (Maybe Value)
  , dormant : Set Ref
  , pending : Set Ref
  , schedule : List Ref
  }
envEmpty =
  { terms = Dict.empty
  , finished = Dict.empty
  , dormant = Set.empty
  , pending = Set.empty
  , schedule = []
  }

-- TODO: needs to eval ...
--aderef env atom = case atom of
  --ARef ref -> case Dict.get env ref of
    --Nothing -> (env, AUnit)
    --Just value -> case value of
      --VAtom (ARef ref) -> aderef env ref
      --VAtom atom -> (env, atom)
      --_ -> -- TODO: compute
  --_ -> (env, atom)

afloat atom = case atom of
  AInt int -> Ok <| toFloat int
  AFloat float -> Ok float
  -- ARef ref -> -- TODO: recurse on referenced value
  _ -> Err "type error: afloat"

arithApply op vlhs vrhs =
  afloat vlhs `Result.andThen`
  \flhs -> afloat vrhs `Result.andThen`
  \frhs -> Ok <| AFloat <| op flhs frhs

-- TODO: separate pending computation scheduling
eval env term = case term of
  Literal atom -> (env, Ok atom)
  BinaryOp op lhs rhs ->
    let (env', mlhs) = eval env lhs
        (env'', mrhs) = eval env rhs
        mresult =
          mlhs `Result.andThen`
          \vlhs -> mrhs `Result.andThen`
          \vrhs -> case op of
            BArithmetic op -> arithApply op vlhs vrhs  -- TODO
            _ -> Err "TODO"
    in (env'', mresult)
  _ -> (env, Err "TODO")

example = BinaryOp (BArithmetic (*)) (Literal <| AFloat 4.1) (Literal <| AInt 3)
test = eval envEmpty example
