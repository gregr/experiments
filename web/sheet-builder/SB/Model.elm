module SB.Model where

import Dict exposing (Dict)
import Set exposing (Set)

type Term
  = Literal Atom
  | TList (List ListConstruction)
  | TSheet Sheet
  | UnaryOp UOp Atom
  | BinaryOp BOp Atom Atom
  --| SheetWith Ref Atom
  --| SheetInput Ref
  --| SheetOutput Ref
  --| Access Accessor
  --| Apply
  --| Identify (Identifier ref)  TODO: need to be able to push name changes
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
type Atom
  = ARef Ref
  | AUnit
  | ABool Bool
  | AString String
  | AInt Int
  | AFloat Float
type alias Accessor ref = { collection : ref, key : Atom ref }
type alias Identifier ref = { namespace : ref, nref : NamedRef ref }
type alias NamedRef ref = { name : Name, ref : ref }

type Value = VAtom Atom | VList (List Ref) | VSheet Sheet
type ListConstruction
  = LCElements (List Atom)
  | LCSplice Ref
  | LCIteration Iteration
type alias Sheet =
  { elements : List Ref -- TODO: labels
  , owned : Set Ref -- not all embedded elements are owned
  , input : List Ref  -- each parameter comes with an example
  , output : Ref
  }
type LayoutOrientation = Vertical | Horizontal

type alias Name = String
type alias Ref = Int

type alias Iteration =
  { procedure : Ref
  , length : Ref
  }

type alias Environment =
  { terms : Dict Ref Term
  , finished : Dict Ref (Maybe Value)
  , pending : Set Ref
  , scheduleIn : List Ref
  , scheduleOut : List Ref
  , uid : Int
  }
envEmpty =
  { terms = Dict.empty
  , finished = Dict.empty
  , pending = Set.empty
  , scheduleIn = []
  , scheduleOut = []
  , uid = 0
  }
schedulePush ref env = { env | pending = Set.insert ref env.pending,
                               scheduleIn = ref :: env.scheduleIn }
schedulePop env = case env.scheduleOut of
  [] -> case List.reverse env.scheduleIn of
          [] -> Nothing
          refs -> schedulePop { env | scheduleIn = [], scheduleOut = refs }
  ref::refs -> Just (ref, { env | scheduleOut = refs,
                                  pending = Set.remove ref env.pending})

nextRef env = (env.uid, { env | uid = env.uid + 1 })
updateTerm ref term env = { env | terms = Dict.insert ref term env.terms }
newTerm term env =
  let (ref, env') = nextRef env
      env'' = updateTerm ref term env'
  in (ref, env'')
refTerm ref { terms } = case Dict.get ref terms of
  Just term -> term
  Nothing -> Literal AUnit

dependencyRefs term = case term of
  Literal (ARef ref) -> Set.singleton ref
  BinaryOp _ lhs rhs ->
    Set.union (dependencyRefs <| Literal lhs) (dependencyRefs <| Literal rhs)
  _ -> Set.empty
-- TODO: account for visibility
rootRefs term = case term of
  --TList lcs ->
  --TSheet sheet ->
  _ -> dependencyRefs term

resultFoldl op acc xs = case xs of
  [] -> Ok acc
  yy::ys -> op yy acc `Result.andThen` \acc' -> resultFoldl op acc' ys

schedule ref env =
  let loop ref (seen, env) =
        if (Set.member ref env.pending || Dict.member ref env.finished) then
           Ok (seen, env)
        else if Set.member ref seen then Err "TODO: cyclic dependency"
        else
          let seen' = Set.insert ref seen
              deps = dependencyRefs <| refTerm ref env
          in resultFoldl loop (seen', env) (Set.toList deps) `Result.andThen`
             \(seen'', env') -> Ok (seen'', schedulePush ref env')
  in loop ref (Set.empty, env) `Result.andThen` \(_, env') -> Ok env'

--evalTerm term env = case term of
  --Literal atom -> (Ok atom, env)
  --BinaryOp op lhs rhs ->
    --let result = case op of
      --BArithmetic op -> arithApply op lhs rhs  -- TODO
      --_ -> Err "TODO"
    --in (result, env)
  --_ -> (Err "TODO", env)
--evalRef ref env = case Dict.get ref env.terms of
  --Nothing -> (Err "TODO: missing term for ref", env)
  --Just term -> evalTerm term env `Result.andThen`
    --\atom -> ... value ...
--evalSchedule1 env = schedulePop env `Maybe.andThen`
  --\(ref, env') -> Just <| evalRef ref env'

afloat atom = case atom of
  AInt int -> Ok <| toFloat int
  AFloat float -> Ok float
  _ -> Err "type error: afloat"

arithApply op alhs arhs =
  afloat alhs `Result.andThen`
  \flhs -> afloat arhs `Result.andThen`
  \frhs -> Ok <| AFloat <| op flhs frhs

-- TODO: separate pending computation scheduling
evalAtom atom pending env = case atom of
  ARef ref -> case Dict.get ref env.finished of
    Just value -> case value of
      VAtom atom -> (Ok atom, env)
      _ -> (Ok <| ARef ref, env)
    Nothing ->
      if Set.member ref pending then (Err "TODO: cyclic computation", env)
      else case Dict.get ref env.terms of
        Nothing -> (Err "TODO: missing term for ref", env)
        Just term ->
          let (result, env') = eval term (Set.insert ref pending) env
          in case result of
            Err _ -> (result, env')
            Ok atom ->
              let finished' = Dict.insert ref (VAtom atom) env'.finished
              in (Ok atom, { env' | finished = finished' })
  _ -> (Ok <| atom, env)

eval term pending env = case term of
  Literal atom -> evalAtom atom pending env
  BinaryOp op lhs rhs ->
    let (rlhs, env') = evalAtom lhs pending env
        (rrhs, env'') = evalAtom rhs pending env'
        result = rlhs `Result.andThen`
        \alhs -> rrhs `Result.andThen`
        \arhs -> case op of
          BArithmetic op -> arithApply op alhs arhs
          _ -> Err "TODO: eval binary op"
    in (result, env'')
  _ -> (Err "TODO: eval term", env)

example0 = BinaryOp (BArithmetic (*)) (AFloat 4.1) (AInt 3)
(r0, exampleEnv) = newTerm example0 envEmpty
example = BinaryOp (BArithmetic (+)) (AFloat 5) (ARef r0)
test = eval example Set.empty exampleEnv

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
      need a convenient way to support keyword args for descriptiveness
    input: retrieve arguments
    output: retrieve result
  ultimately all operations could use this approach
    primitives having opaque internals

multiple dormant alternative terms may live inside a ref entry

what is the right label structure to support auto-updating renamed identifiers?

pattern matching case analysis? at least simple boolean conditionals
  [sealed] algebraic data?

deletion, (partial)transferring dependencies

ref-counting gc (that can recognize embedded cycles)?

optional compositional visual representations provided by default?
  numbers
    points on a scale
    circular/rectangular areas
      bars a special case of this
    angles
    color
  numeric lists
    plotted on a chart: cartesian or polar coordinates
    vectors
    rectangular areas
    angular sweeps
  generic lists
    visualized change or difference
      juxtaposition
      superimposition
      deltas
      paths
      motion
    stat summaries
      brackets
      candlesticks
      histograms
-}
