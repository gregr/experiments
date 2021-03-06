module SB.Model where

import Dict exposing (Dict)
import Set exposing (Set)

type Term
  = Literal Atom
  | TList (List ListComponent)
  | TIteration Iteration
  | TSheet Sheet
  | UnaryOp UOp Atom
  | BinaryOp BOp Atom Atom
  | SheetWith Ref Atom
  | SheetInput Ref
  | SheetOutput Ref
  | Access Ref Atom
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
  | BArithmetic BArithmeticOp
  | BStringConcat
  | BStringSubrange
  | BStringReplace
type BArithmeticOp
  = BAdd | BSub | BMul | BDiv | BQuo | BRem | BMod | BExp | BLog
type Atom
  = ARef Ref
  | AUnit
  | ABool Bool
  | AString String
  | ANumber Number
type Number = NInt Int | NFloat Float
type alias Identifier = { namespace : Ref, nref : NamedRef }
type alias NamedRef = { name : Name, ref : Ref }

type Value = VAtom Atom | VList (List ListComponent) | VSheet Sheet
type ListComponent = LCElements (List Atom) | LCSplice Ref
type alias Sheet =
  { elements : Set Ref
  , input : Ref
  , output : Atom
  }

type alias Name = String
type alias Ref = Int

type alias Iteration =
  { procedure : Ref
  , length : Atom
  }

type alias Environment =
  { terms : Dict Ref Term
  , finished : Dict Ref (Maybe Value)
  --, pending : Set Ref
  --, scheduleIn : List Ref
  --, scheduleOut : List Ref
  , uid : Int
  }
envEmpty =
  { terms = Dict.empty
  , finished = Dict.empty
  --, pending = Set.empty
  --, scheduleIn = []
  --, scheduleOut = []
  , uid = 0
  }

nextRef env = (env.uid, { env | uid = env.uid + 1 })
updateTerm ref term env = { env | terms = Dict.insert ref term env.terms }
newTerm term env =
  let (ref, env') = nextRef env
      env'' = updateTerm ref term env'
  in (ref, env'')
refTerm ref { terms } = case Dict.get ref terms of
  Just term -> term
  Nothing -> Literal AUnit
refValue ref { finished } = Dict.get ref finished
finishRef ref val env = { env | finished = Dict.insert ref val env.finished }

atomDirectDeps atom = case atom of
  ARef ref -> Set.singleton ref
  _ -> Set.empty
listComponentDirectDeps lc = case lc of
  LCElements atoms -> List.foldl (Set.union << atomDirectDeps) Set.empty atoms
  LCSplice ref -> Set.singleton ref
termDirectDeps term = case term of
  Literal atom -> atomDirectDeps atom
  BinaryOp _ lhs rhs -> atomDirectDeps lhs `Set.union` atomDirectDeps rhs
  TList lcs -> List.foldl (Set.union << listComponentDirectDeps) Set.empty lcs
  TIteration {procedure, length} ->
    Set.insert procedure <| atomDirectDeps length
  TSheet {elements, input, output} ->
    Set.insert input (elements `Set.union` atomDirectDeps output)
  SheetWith ref atom -> Set.insert ref <| atomDirectDeps atom
  SheetInput ref -> Set.singleton ref
  SheetOutput ref -> Set.singleton ref
  Access ref atom -> Set.insert ref <| atomDirectDeps atom
  _ -> Set.empty
refTermDirectDeps ref env = termDirectDeps <| refTerm ref env

--schedulePush ref env = { env | pending = Set.insert ref env.pending,
                               --scheduleIn = ref :: env.scheduleIn }
--schedulePop env = case env.scheduleOut of
  --[] -> case List.reverse env.scheduleIn of
          --[] -> Nothing
          --refs -> schedulePop { env | scheduleIn = [], scheduleOut = refs }
  --ref::refs -> Just (ref, { env | scheduleOut = refs,
                                  --pending = Set.remove ref env.pending})

--dependencyRefs term = case term of
  --Literal (ARef ref) -> Set.singleton ref
  --BinaryOp _ lhs rhs ->
    --Set.union (dependencyRefs <| Literal lhs) (dependencyRefs <| Literal rhs)
  --_ -> Set.empty

-- TODO: account for visibility
--rootRefs term = case term of
  --_ -> dependencyRefs term

--schedule ref env =
  --let loop ref (seen, env) =
        --if (Set.member ref env.pending || Dict.member ref env.finished) then
           --Ok (seen, env)
        --else if Set.member ref seen then Err "TODO: cyclic dependency"
        --else
          --let seen' = Set.insert ref seen
              --deps = dependencyRefs <| refTerm ref env
          --in resultFoldl loop (seen', env) (Set.toList deps) `Result.andThen`
             --\(seen'', env') -> Ok (seen'', schedulePush ref env')
  --in loop ref (Set.empty, env) `Result.andThen` \(_, env') -> Ok env'

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
--resultFoldl = foldM pure0 (@>>=)

forM_ pure bind acc xs op =
  let loop acc xs = case xs of
    [] -> pure acc
    yy::ys -> bind (op yy acc) (\next -> loop next ys)
  in loop acc xs

pure0 = Ok
join0 result = case result of
  Err err -> Err err
  Ok ok -> ok
(@>>=) = Result.andThen
infixl 1 @>>=
(@*>) p0 p1 = p0 @>>= \_ -> p1
infixl 4 @*>
(@<*) p0 p1 = p0 @>>= \r0 -> p1 @>>= \_ -> pure0 r0
infixl 4 @<*
(@<*>) p0 p1 = p0 @>>= \f0 -> p1 @>>= \r1 -> pure0 (f0 r1)
infixl 4 @<*>
(@<$>) f0 p1 = pure0 f0 @<*> p1
infixl 4 @<$>

pure1 val state = (val, state)
($>>=) p0 fp1 state = let (result, state') = p0 state
                      in fp1 result state'
infixl 1 $>>=
($*>) p0 p1 = p0 $>>= \_ -> p1
infixl 4 $*>
($<*) p0 p1 = p0 $>>= \r0 -> p1 $>>= \_ -> pure1 r0
infixl 4 $<*
($<*>) p0 p1 = p0 $>>= \f0 -> p1 $>>= \r1 -> pure1 (f0 r1)
infixl 4 $<*>
($<$>) f0 p1 = pure1 f0 $<*> p1
infixl 4 $<$>
forM1 = forM_ pure1 ($>>=)

pure val state = (Ok val, state)
(>>=) p0 fp1 state = let (result, state') = p0 state
                     in case result of
                       Err err -> (Err err, state')
                       Ok ok -> fp1 ok state'
infixl 1 >>=
(*>) p0 p1 = p0 >>= \_ -> p1
infixl 4 *>
(<*) p0 p1 = p0 >>= \r0 -> p1 >>= \_ -> pure r0
infixl 4 <*
(<*>) p0 p1 = p0 >>= \f0 -> p1 >>= \r1 -> pure (f0 r1)
infixl 4 <*>
(<$>) f0 p1 = pure f0 <*> p1
infixl 4 <$>

nint num = case num of
  NInt int -> int
  NFloat float -> round float
nfloat num = case num of
  NInt int -> toFloat int
  NFloat float -> float
anumber atom = case atom of
  ANumber num -> Ok num
  _ -> Err "expected a number"
aint atom = nint @<$> anumber atom
afloat atom = nfloat @<$> anumber atom

baopCompute aop =
  let flop op lhs rhs = NFloat <| op (nfloat lhs) (nfloat rhs)
      iop op lhs rhs = NInt <| op (nint lhs) (nint rhs)
      nop io fo lhs rhs = case (lhs, rhs) of
                            (NInt nl, NInt nr) -> NInt <| io nl nr
                            _ -> flop fo lhs rhs
  in case aop of
    BAdd -> nop (+) (+)
    BSub -> nop (-) (-)
    BMul -> nop (*) (*)
    BDiv -> flop (/)
    BQuo -> iop (//)
    BRem -> iop rem
    BMod -> iop (%)
    BLog -> flop logBase
    BExp -> nop (^) (^)
arithApply op alhs arhs =
  ANumber @<$> (baopCompute op @<$> anumber alhs @<*> anumber arhs)

valueAtom ref value = case value of
  VAtom atom -> atom
  _ -> ARef ref

-- TODO: separate pending computation scheduling
evalRef pending ref env = (case refValue ref env of
  Just value -> pure <| valueAtom ref value
  Nothing ->
    if Set.member ref pending then pure1 <| Err "TODO: cyclic computation"
    else eval (Set.insert ref pending) (refTerm ref env) >>=
    \value env -> pure (valueAtom ref value) (finishRef ref value env)) env

evalAtom pending atom = case atom of
  ARef ref -> evalRef pending ref
  _ -> pure atom

vlist value = case value of
  VList parts -> Ok parts
  _ -> Err "expected reference to point to a list"
vsheet value = case value of
  VSheet sheet -> Ok sheet
  _ -> Err "expected reference to point to a sheet"

acompound vcompound atom env = case atom of
  ARef ref -> case refValue ref env of
    Nothing -> Err "reference points to nothing"
    Just value -> vcompound value
  _ -> Err "expected a reference"
alist = acompound vlist
asheet = acompound vsheet

-- TODO: how do we use this without a tediously-large type annotation?
--evalCompound ac cref pending z =
  --(evalRef cref pending >>= \atom env -> (ac atom env, env)) z
--evalList x = evalCompound alist x
--evalSheet x = evalCompound asheet x
evalList pending cref = evalRef pending cref >>= \atom env -> (alist atom env, env)
evalSheet pending cref = evalRef pending cref >>= \atom env -> (asheet atom env, env)

-- TODO: negative indices, iterations
listAccess pending lref index =
  let access ref index parts =
        evalList pending ref >>= \prefix -> retrieve index (prefix ++ parts)
      retrieve index parts = case parts of
        [] -> pure1 <| Err "index out of bounds"
        LCElements elements::parts ->
          let len = List.length elements
          in if index < len
             then pure1 <| Result.fromMaybe "impossible?" <| List.head <| List.drop index elements
             else retrieve (index - len) parts
        LCSplice ref::parts -> access ref index parts
  in access lref index [] >>= (<$>) VAtom << evalAtom pending

refCopy refmap ref = Maybe.withDefault ref <| Dict.get ref refmap
atomCopy refmap atom = case atom of
  ARef ref -> ARef <| refCopy refmap ref
  _ -> atom
lcCopy refmap lc = case lc of
  LCElements atoms -> LCElements <| List.map (atomCopy refmap) atoms
  LCSplice ref -> LCSplice <| refCopy refmap ref
termCopy refmap term = case term of
  Literal atom -> Literal <| atomCopy refmap atom
  BinaryOp op lhs rhs ->
    BinaryOp op (atomCopy refmap lhs) (atomCopy refmap rhs)
  TList lcs -> TList <| List.map (lcCopy refmap) lcs
  TIteration { procedure, length } ->
    TIteration { procedure = refCopy refmap procedure,
                 length = atomCopy refmap length }
  SheetWith sref arg -> SheetWith (refCopy refmap sref) (atomCopy refmap arg)
  SheetInput sref -> SheetInput (refCopy refmap sref)
  SheetOutput sref -> SheetOutput (refCopy refmap sref)
  _ -> term

refInstantiate refmap ref env = case Dict.get ref refmap of
  Nothing -> env
  Just nref -> let term = refTerm ref env
               in uncurry (updateTerm nref) <| termInstantiate refmap term env
atomInstantiate refmap atom env = case atom of
  ARef ref -> refInstantiate refmap ref env
  _ -> env
sheetInstantiate refmap sheet arg =
  let genref oref (rmap, env) =
        (nextRef $>>= \nref -> pure1 <| Dict.insert oref nref rmap) env
  in (\env -> Set.foldl genref (refmap, env) sheet.elements) $>>= \refmap0 ->
     newTerm (Literal arg) $>>= \param ->
       let refmap1 = Dict.insert sheet.input param refmap0
           output = atomCopy refmap1 sheet.output
           elements = Set.map (refCopy refmap1) sheet.elements
       in \env -> ({ sheet | elements = elements,
                             input = param,
                             output = output }
                  , Set.foldl (refInstantiate refmap1) env sheet.elements)
termInstantiate refmap term = case term of
  TSheet sheet -> TSheet $<$> sheetInstantiate refmap sheet (ARef sheet.input)
  _ -> pure1 <| termCopy refmap term

iterate pending procedure count =
  let sheetAt index = newTerm <| SheetWith procedure <| ANumber <| NInt index
      elementAt index = ARef $<$> (sheetAt index $>>= newTerm << SheetOutput)
      relements = forM1 [] [0..count - 1]
        (\index acc -> flip (::) acc $<$> elementAt index)
      elements = List.reverse $<$> relements
  in pure0 << VList << flip (::) [] << LCElements $<$> elements

eval pending term = case term of
  Literal atom -> VAtom <$> evalAtom pending atom
  BinaryOp op lhs rhs ->
    let bop = \alhs arhs -> case op of
          BArithmetic op -> VAtom `Result.map` arithApply op alhs arhs
          _ -> Err "TODO: eval binary op"
    in join0 $<$> (bop <$> evalAtom pending lhs <*> evalAtom pending rhs)
  TList lcs -> (,) <| Ok <| VList lcs
  TIteration { procedure, length } ->
    join0 $<$> (aint <$> evalAtom pending length) >>= iterate pending procedure
  TSheet sheet -> (,) <| Ok <| VSheet sheet
  SheetWith sref arg ->
    evalSheet pending sref >>= \sheet ->
      pure0 << VSheet $<$> sheetInstantiate Dict.empty sheet arg
  SheetInput sref -> evalSheet pending sref >>=
    \sheet -> VAtom <$> evalRef pending sheet.input
  SheetOutput sref -> evalSheet pending sref >>=
    \sheet -> VAtom <$> evalAtom pending sheet.output
  Access lref index ->
    join0 $<$> (aint <$> evalAtom pending index) >>= listAccess pending lref
  _ -> (,) <| Err "TODO: eval term"

example =
  newTerm (Literal <| ANumber <| NInt 3) $>>= \rparam ->
  newTerm (BinaryOp (BArithmetic BMul) (ANumber <| NFloat 4.1) (ARef rparam)) $>>= \r0 ->
  newTerm (BinaryOp (BArithmetic BAdd) (ANumber <| NFloat 5) (ARef r0)) $>>= \r1 ->
  newTerm (TList [LCElements [ARef r0, AString "and", ARef r1]]) $>>= \r2 ->
  newTerm (Literal <| AString "example sheet") $>>= \r3 ->
  newTerm (TSheet { elements = Set.fromList [r3, r0, r1, r2]
                  , input = rparam
                  , output = ARef r2
                  }) $>>= \r4 ->
  newTerm (SheetOutput r4) $>>= \r5 ->
  newTerm (Access r5 <| ANumber <| NInt 2) $>>= \raccess0 ->
  newTerm (SheetWith r4 (ARef raccess0)) $>>= \r6 ->
  newTerm (Literal <| ARef r6) $>>= \r7 ->
  newTerm (Literal <| ARef r7) $>>= \r8 ->
  newTerm (Literal <| ARef r8) $>>= \r9 ->
  newTerm (SheetInput r9) $>>= \r10 ->
  newTerm (SheetOutput r9) $>>= \r11 ->
  newTerm (Access r11 <| ANumber <| NFloat 2.1) $>>= \r12 ->
  newTerm (Literal <| AUnit) $>>= \rparam1 ->
  newTerm (BinaryOp (BArithmetic BMul) (ANumber <| NFloat 1.5) (ARef rparam1)) $>>= \r13 ->
  newTerm (TSheet { elements = Set.fromList [r13]
                  , input = rparam1
                  , output = ARef r13
                  }) $>>= \r14 ->
  newTerm (TIteration { procedure = r14, length = ANumber <| NInt 10 }) $>>= \r15 ->
  newTerm (Access r15 (ANumber <| NInt 7)) $>>= \r16 ->
    pure1 [r16, r12, r2]
(testRefs, testEnv) = example envEmpty

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
