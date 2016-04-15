module SB.Model where

import Dict exposing (Dict)
import Set exposing (Set)

-- TODO: Identifier refs should be limited to abs or rel references, not to full terms
type TermT leaf = Literal Atom | Construct (Compound leaf) | Iterate Iteration | Identify (Identifier leaf) | Access (Accessor leaf) | Apply
type Atom = ARef Ref | AUnit () | ABool Bool | AString String | ANumber Number
type Number = NInt Int | NFloat Float

type alias Identifier ref = { namespace : ref, name : Name, ref : ref }
type alias Accessor leaf = { collection : leaf, key : leaf }

type Value = VAtom Atom | VCompound (Compound Ref)
type Compound leaf = CList (List leaf) | CDict (Dictionary leaf) | CSheet (Sheet leaf)
type alias Sheet leaf = { data : (Dictionary leaf), container : Container }
type alias Dictionary value = Dict Name (List value)
type UI = DataRef Name | UIContainer Container --| Widget
type alias Container = { orientation : LayoutOrientation, elements : List UI }
type LayoutOrientation = Vertical | Horizontal

type alias Name = String
type alias Ref = Int
type alias Program = Dict Ref FlatTerm  -- TODO: alternative terms
type alias ProgramState = Dict Ref TermState
type alias TermState =
  { term : FlatTerm
  , result : Maybe Value
  --, dependencies : Set Ref
  --, dependants : Set Ref
  }

type alias FlatTerm = TermT Ref

-- TODO:
-- sheet-based namespacing
-- deletion, (partial)transferring dependencies

type IterTerm = ITerm (TermT IterTerm) | IRelRef Ref | IPos Int  -- current position in iteration Int levels up

type alias Iteration =
  { body : IterTerm
  , length : Int
  }
