module SB.Model where

import Dict exposing (Dict)
import Set exposing (Set)

type TermT ref
  = Literal (Atom ref)
  | TList (List (ListConstruction ref))
  | TSheet (Sheet ref)
  | Identify (Identifier ref)
  | Access (Accessor ref)
  | UnaryOp UOp (Atom ref)
  | BinaryOp BOp (Atom ref) (Atom ref)
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
    let result = case op of
      BArithmetic op -> arithApply op lhs rhs  -- TODO
      _ -> Err "TODO"
    in (env, result)
  _ -> (env, Err "TODO")

example = BinaryOp (BArithmetic (*)) (AFloat 4.1) (AInt 3)
test = eval envEmpty example

{-
Notes:

new nodes
  mirror identical ref
  new ref wrapping another (default behavior)
  shallow copy of term head and locally-defined refs
    preserves subterm structure
    supports instantiation of procedural abstractions without local refs

example-oriented abstraction by extending sheets
  sheet structure
    parameters with arguments they're currently bound to
      can be thought of as publicly visible properties, and as example inputs
    encapsulated internal definitions, like a haskell where-clause
      likely provides most of a sheet's spatial workspace
      definitions are possibly labeled
    result body, which can refer to the encapsulated definitions
  operations
    with: replace some arguments; arguments may be added even if unused
    input: retrieve arguments
    output: retrieve result
  ultimately all operations could use this approach
    primitives having opaque internals

multiple dormant alternative terms may live inside a ref entry

what is the right label structure to support auto-updating renamed identifiers?
-}
