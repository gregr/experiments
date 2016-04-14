module SB.Model where

import Dict exposing (Dict)
import Set exposing (Set)

type Term = Literal Atom | Identify Name --| App | Access -- TODO: Identify, namespace expression, similar to Access
type Atom = ARef Ref | AUnit () | ABool Bool | AString String | ANumber Number
type Number = NInt Int | NFloat Float

type Value = VAtom Atom | VCompound Compound
type Compound = CList (List Ref) | CDict Dictionary | CSheet Sheet
type alias Sheet = { data : Dictionary, container : Container } --, embedded : Bool }
type alias Dictionary = Dict Name (List Ref)
type UI = DataRef Name | UIContainer Container --| Widget
type alias Container = { orientation : LayoutOrientation, elements : List UI }
type LayoutOrientation = Vertical | Horizontal

type alias Name = String
type alias Ref = Int
type alias Program = Dict Ref Term  -- TODO: alternative terms
type alias ProgramState = Dict Ref TermState
type alias TermState =
  { term : Term
  , result : Maybe Value
  --, dependencies : Set Ref
  --, dependants : Set Ref
  }

-- TODO: patterns, derivation and parameterized generation
