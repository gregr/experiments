module Expr where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

type Symbol = [Int]
--type Name = Symbol
type Name = String -- temporary until symbols have a string mapping
garbageName = ""
type Binding val = (Name, val)

type Env val = M.Map Name val
--data Env val = Env [[Binding val]] -- constraint satisfiers are bindings
envEmpty = M.empty
envInsert env key val = M.insert key val env
envInserts = L.foldl' $ \env' (key, val) -> envInsert env' key val
envLookup env key dfault = fromMaybe dfault $ M.lookup key env
envLookupFail env key msg = envLookup env key (error msg)

data Typing = Typing Expr EEnv

{-data Variance = Invariant | Covariant | Contravariant-}

{-data Constr = Constr { constrTag :: Symbol,-}
                       {---constrVar :: [Variance],-}
                       {-constrTy :: Typing }-}

{-

type ascription vs. term "ascription" (need a better word (could also use in place of type ascription): specification? description?)
SpecifyType (:, <:), SpecifyTerm (:>)

in this dependent type, the constructor (->) is passed A, and some type determined by B(x), where x is a term of type A
x:A -> B(x)

using the same notation in this term this seems weird, because it looks like we pass x, but assert its type is A, rather than passing A and naming a sub-term of term A
Constr x:A B(x)

(:) used in two ways; instead, use another operator to note the term name in a dependent type? maybe <: vs. :> is appropriate?


-}

-- these can be in terms of dependent constructors
{-data Pi = Pi Name Expr Expr-}
{-data Sigma = Sigma Name Expr Expr-}

-- data Sigma = Sigma [Binding Expr] (Maybe Expr) -- Sigma x:A B(x)

-- dependent: Tuple; how to access elements? project? destruct?
-- non-dependent: Record, Variant (sum of tuples)
-- optimizations: Contiguous Array, Sparse Map for field lookup by name

-- non-deterministic computing, logic vars, term inference, etc.
-- additive (normal) sets: {} is bottom, {x} is x, {x, y} is x or y
-- subtractive (inverse) sets: {} is top, {x} is anything but x

-- dispatch: path or instruction sequences to describe how to unpackage a (possibly dependent) product while accessing the case/records by label
--   are conditions needed? at some point you only want to conditionally inspect...
--   inspect (dispatch on (next N? conditionally?) field), skip (ignore the first N fields? or skip to field N?), apply/use (pass the next N fields as args to the target function/destructor)
-- alternative dispatch (much simpler): products (tagged or otherwise) are splittable types; tagged products are just products with an extra symbol at the head
--   split at N: (a, b, ...N, y, z) becomes left: (a, b, ...N) and right: (y, z), where a singleton tuple (x) is the same as x
--     split takes an index, a 2-arg (left, right) receiver function, and a product to split
--
-- story for algebraic types
-- an algebraic definition defines a dependent product of the form: [(ThisTagNamespace :> tag) Payload(tag)]
-- thus, the memory representation of algebraic data looks like:
--   addr-A: [tag][addr-B] (2-element contiguous memory)
--   addr-B: Payload(tag)
-- tags and ptr to payload are separate from payload itself, and could likely be held flat in registers rather than heap
--   tag could also (sometimes? often?) be encoded directly in ptr's unused bits
-- this decoupling seems to make it easier to erase unnecessary information from runtime representation without needing multiple representations (tagged vs. tagless) for each data type
-- it also allows for a nice implementation of splitting, operating via simple pointer arithmetic, giving an efficient story for random access indexing and slices
--
-- maybe slices are a better than splits? actually, if you allow singleton tuples, add a deref/pop/access/select/index operation to unpack the first element, and add a join/concat operation, you can explain slices, and functional slice updates (that may even change size); join operator could also allow for (inefficient?) construction of large arrays
--
-- maybe a left/right pair of operators is nicer than split? maybe split should return a pair rather than passing the left/right results to a receiver function?
--
-- records: functional update, projection/retraction(specify what remains)/deletion(specify what to remove)/elide/omit, extension/join, field access/retrieve/select/dispatch
--   project, and omit sound good for the shrinking case; join is probably fine for extension via asymmetrically combining two records
--
-- support types that are both tuples and records? record-like behavior for chosen tuples exposed via typeclass?
--
-- next questions:
-- scripts, modules, procedures...
-- module linking and completion/record-reification as separate steps?
-- modules whose available bindings only include external ones (via wrapping in a procedure and passing it externally evaluated field values?)
-- modules whose available bindings only include internal/linked ones
-- environments and variable referencing
-- modules to explain letrec
-- unlike previously thought, defining and linking modules should probably be effect-less
--
-- two recursively linked modules produce a new module with two environment threads to extend in the future; values from each original module close over different environments, and the recursively referenced names that are linked end up referenced in both environments; optimization could potentially rewrite the mutually recursive code to obtain a single environment thread?
-- records and modules both respond to the same linking interfaces, but only completed modules have fields to access like records
--
-- barriers to name pollution in newly-defined modules; onion-like layers; expand-in-env, import/require/parameterize should be innermost?
-- this-env (expands? seems tough) produces the current environment as a record
-- expand-in-env (for loading modules from file, see below); linking is not enough to take care of base language injection? may not be able to even define anything in a module
-- parameter list to shield from names in expansion env
-- expand-in-env won't work for inlined inner modules directly, since they will have been expanded before expand-in-env has an env to use
--   special module expanders should be defined as syntax (in terms of expand-in-env), available when expanding the outer module, to ensure the special expander is used for the inner module
--   for instance, something with a name like expand-in-kernel-env (probably the most common choice when not expanding in the current env)
-- kernel module containing the primitives everything else is defined with; bootstrapping entails running a script with this module as the environment, and building everything else
--
-- how the hell does all this fit nicely into exprs/terms? seems like you need non-expr values
-- ffi and other opaque functions must at least list their types, including effects, allowing for abstract interpretation; and they can be represented as nullary constructor terms
--
-- maybe modules should specify let/letrec (possibly several, possibly alternating) sections for their definitions; this explains records as modules
--
-- if mutually-recursive linking of procedures can be explained, then have modules be procedures, add letrec directly, and records become simple? but procedures and records have the same linking operation? hmm...
--
-- another explanation of plain records: let, with a body extracting this-env, projected to only show the let-binders as fields; all record ops are now module ops
--
-- types and security: how to avoid sealed type constituent information from leaking over the wire?
--   for instance, refined types such that you know a function accepts or returns only a subset of the full set of a sealed type's constructors
--   maybe it's enough to prevent refined and flow types from being sent into unsafe hands (over the wire, to a separate vat, whatever)

type ESet = S.Set Expr
type EEnv = Env Expr
type Proc = ([Name], Expr) -- params (some implicit?); body expression to evaluate
type Mod = ([Name], [Binding Expr], DepGraph) -- params: handle positional, keyword, and implicit; list of definitions; definition deps
type DepGraph = [(Name, [Name], [Name])] -- a, bs, cs; a is directly dependent on bs, and indirectly dependent on cs
-- DepGraph is needed for module type, but what about module value? maybe simply 'undefined' is fine for a module value

--type CodesA annot = [(Code, annot)]
--type Codes = CodesA ()
type Codes = [Code]
type CProc = ([Name], Codes, Name) -- params, body, return cont name

data Value = ValProc CProc EEnv
           | ValModule Mod EEnv
           | ValRecord EEnv
           -- todo: an efficient array-like tuple?
           -- todo: efficient sets
           | ValSetPositive ESet
           | ValSetNegative ESet
  deriving (Show)

-- todo: use a finally-tagless approach
-- continuation env, value env, local value stack, local continuation, return continuation label, annotation
--data ExecState0 cann sann = ExecState (Env (ExecState0 cann sann)) EEnv [Expr] (CodesA cann) Name sann
data ExecState0 annot = ExecState { stateConts :: Env (ExecState0 annot),
                                    stateVals :: EEnv, stateLocals :: [Expr],
                                    stateCodes :: Codes, stateContLab :: Name,
                                    stateAnnot :: annot }
  deriving (Show)
type ExecState = ExecState0 ()
-- todo: due to CProc and CModule, this will need to be recursively annotated
data Code = Halt
          | Return
          | CVar Name
          | CLit Value
          | CApply Int
          | CAbstract CProc
          | CTuple Int
  deriving (Show)

initExecState codes = ExecState envEmpty envEmpty [] codes garbageName
procToCProc (names, body) contLabel = (names, exprToCodes body ++ [Return], contLabel)
argsToCodes args = concat [exprToCodes arg | arg <- args]
exprToCodes (ExprA expr _) = case expr of
  Var name        -> [CVar name]
  Literal val     -> [CLit val]
  Apply proc args -> cargs ++ cproc ++ [CApply $ length args]
    where cproc = exprToCodes proc
          cargs = argsToCodes args
  Abstract proc -> [CAbstract $ procToCProc proc "todo"]
  Tuple args    -> argsToCodes args ++ [CTuple $ length args]

cstep es@(ExecState conts vals locals (code : codes) label annot) = case code of
  Halt   -> es -- stay at the same state intentionally
  Return -> push cont result -- todo: got data-lens?
    where [result] = locals -- assert single value in locals stack
          cont = envLookupFail conts label "Return: missing label" -- todo: really need string formatting
          [] = codes -- assert
  CVar name -> push next val
    where val = envLookupFail vals name "CVar: missing name"
  CLit val -> push next $ ExprA (Literal val) () -- todo: expr annotations need borrowing from Code
  CApply nargs -> let ExprA (Literal (ValProc (pnames, pcodes, plabel) penv)) _ = head locals
                      surplus = nargs - length pnames
                      nargs' = min nargs $ length pnames
                      (args, rest) = splitAt nargs' $ tail locals
                      pnames' = drop nargs' pnames
                      penv' = envInserts penv $ zip pnames args
                      cproc' = ExprA (Literal (ValProc (pnames', pcodes, plabel) penv')) ()
                      codes' = if surplus > 0 then CApply surplus : codes else codes
                      cont = case codes' of -- can also tail call with [Halt] ... maybe substitute empty code list for Halt and Return; Halt as a cont label
                        [Return] -> envLookupFail conts label "Tail Call: missing label"
                        _ -> next { stateLocals = rest, stateCodes = codes' }
                      pconts = envInsert conts plabel cont in
    if surplus < 0 then push next cproc'
    else ExecState pconts penv' [] pcodes plabel annot -- todo: annotation
  CAbstract cproc -> push next $ ExprA (Literal $ ValProc cproc vals) ()
  {-CTuple nargs    ->-}
  where next = es { stateCodes = codes }
        push state val = state { stateLocals = val : stateLocals state }

cexec = iterate cstep -- todo: find the halting state
ceval codes = cexec $ initExecState codes ()
eval expr = ceval $ exprToCodes expr ++ [Halt]

mkExpr expr = ExprA expr ()
idcomb = mkExpr $ Abstract (["id"], mkExpr $ Var "id")
testid = mkExpr $ Apply idcomb [idcomb]
ucomb = mkExpr $ Abstract (["u"], mkExpr $ Apply varu [varu])
  where varu = mkExpr $ Var "u"
ycomb = mkExpr $ Abstract (["f"], mkExpr $ Apply ucomb [mkExpr $ Abstract (["h"], mkExpr $ Apply (mkExpr $ Var "f") [mkExpr $ Apply ucomb [mkExpr $ Var "h"]])])
testUid = mkExpr $ Apply ucomb [idcomb]
testUU = mkExpr $ Apply ucomb [ucomb]
--testY =

test = eval testUid !! 400

type Expr = ExprA () -- todo: temporary to avoid having to parameterize everything while still sketching things out
data ExprA annot = ExprA (Expr0 (ExprA annot)) annot -- annotated expressions
  deriving (Show)
data Expr0 expr = Var Name
                | Literal Value -- literally reference a more efficient value representation; have this even though pure values should all be representable syntactically
                | Apply expr [expr]
                -- | ApplyImplicit expr [expr]
                | Abstract Proc
                | Record [(expr, expr)] -- module constructor that supports non-symbol field names (as type-level values?); need for this might just be temporary
                | Module Mod
                | ModuleAccess expr expr expr -- 'default' expr can be 'undefined' to induce type error for accessing missing fields
                | ModuleProject expr expr -- module; list of names; new module keeping only the listed names
                | ModuleOmit expr expr -- module; list of names; new module removing the listed names
                | ModuleImport [expr] expr -- add the fields of the listed modules to the target module, creating a new module
                | ModuleLink [expr] -- asymmetric mutually-recursive linking of multiple modules
                | Tuple [expr] -- dependent constructor, ie: Constr x:A y:B | F(x, y)
                | TupleAccess expr expr
                | TupleSplit expr expr -- see rambling above
                | TupleConcat [expr]
                | SetPositive [expr] -- empty is bottom/void/undefined
                | SetNegative [expr] -- empty is top/any
                | TypeAscribe expr expr -- term; type
                | TypeStructural expr -- Denote a structural type: the type of a tuple is a tuple of the element types; without this mark, the type of such a type would not clearly be 'Set'
                | TypeSet -- the type of types; temporary: throw this away after adding nominal types
                {-| TypeSeal expr expr -- Hide the structural type of value behind a nominal type; sealer and value to seal; resulting type depends on the sealer-}
                {-| TypeUnseal expr expr-}
                -- | build variant (as an algebra?)
  deriving (Show)

-- definitions:
--  bindings
--  variants (finite and otherwise)
--  classes
--  instances
--  specifying defaults/implicits
--  modules
--   linking
