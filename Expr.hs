module Expr where

import qualified Data.Map as M
import qualified Data.Set as S

type Symbol = [Integer]
type Name = Symbol
type Binding val = (Name, val)

data Env val = Env [Binding val] -- constraint satisfiers are bindings

data Typing = Typing Expr Env

data Variance = Invariant | Covariant | Contravariant

data Constr = Constr { constrTag :: Symbol,
                       --constrVar :: [Variance],
                       constrTy :: Typing }

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

type Proc = ([Name], Expr)

data Value = Datum Constr [Value]
           | Closure Proc (Env Value)

data Expr = Var Name
          | App Expr [Expr]
          | AppImplicit Expr [Expr]
          | Abs Expr Expr -- module; body expression to evaluate within module's env
          | ModuleConstruct Expr [Binding Expr] -- parent module/env; list of definitions: (name, expr) pairs; be able to handle positional, keyword, and implicit args
          | ModuleAccess Expr Expr Expr -- 'default' Expr can be 'undefined' to induce type error for accessing missing fields
          | ModuleProject Expr Expr
          | ModuleOmit Expr Expr
          | ModuleImport [Expr] Expr
          | ModuleLink [Expr] -- asymmetric mutually-recursive linking
          | TupleConstruct [Expr] -- dependent constructor, ie: Constr x:A y:B | F(x, y)
          | TupleAccess Expr Expr
          | TupleSplit Expr Expr -- see rambling above
          | TupleConcat [Expr]
          | TypeSeal Expr Expr -- Hide the structural type of value behind a nominal type; sealer and value to seal; resulting type depends on the sealer
          | TypeUnseal Expr Expr
          | -- build env/module
          | -- build variant (as an algebra?)

-- definitions:
--  bindings
--  variants (finite and otherwise)
--  classes
--  instances
--  specifying defaults/implicits
--  modules
--   linking
