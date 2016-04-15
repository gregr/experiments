module SB.Model where

import Dict exposing (Dict)
import Set exposing (Set)

type TermT ref
  = Literal (Atom ref)
  | Construct (Compound ref)
  | Iterate Iteration
  | Identify (Identifier ref)
  | Access (Accessor ref)
  | Apply
type Atom ref
  = ARef ref
  | AUnit ()
  | ABool Bool
  | AString String
  | ANumber Number
type Number = NInt Int | NFloat Float
type alias Accessor expr = { collection : expr, key : expr }
type alias Identifier ref = { namespace : ref, nref : NamedRef ref }
type alias NamedRef ref = { name : Name, ref : ref }

type Value = VAtom (Atom Ref) | VCompound (Compound Ref)
type Compound ref = CList (List ref) | CSheet (Sheet ref)
type alias Sheet ref =
  { orientation : LayoutOrientation
  , elements : List (NamedRef ref)
  }
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
-- deletion, (partial)transferring dependencies

type alias Level = Int
type IRef = IAbsRef Ref | IRelRef (Level, Ref)
type IterTerm = ITerm (TermT IRef) | IPos Level

type alias Iteration =
  { body : IterTerm
  , locals : List IterTerm
  , length : Int
  }
