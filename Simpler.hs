module Simpler where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

{-I am an inarticulate clod.  Without a direct way to express my ideas, I fall-}
{-flat on my face into a mound of feces I've piled up for myself.-}

{-I should probably write a program to generate the right haskell for expressing-}
{-the language's atoms.  It's far too tedious and error-prone otherwise.-}


{-type Namespace = [Int]-}
{-type Symbol = Int-}
type Symbol = [Int]
type Namespace = Symbol
type SymToString = M.Map Symbol String
type NamespaceInfo = (Int, SymToString)
type NamespaceInfoMap = M.Map Namespace NamespaceInfo

type Row = M.Map Symbol Term
emptyRow = M.empty
type Range = (Symbol, Symbol)
data Record = RecRow Row
           -- | RecRange Range Term
            | RecDefault Term
            | RecGlue [Record]
  deriving (Show)

recordSimplify rec = let (left : rest) = flatten' rec in
  case simplify left rest of
    [single] -> single
    recs -> RecGlue recs
  where flatten' (RecGlue recs) = concatMap flatten' recs
        flatten' rec = [rec]
        simplify left [] = [left]
        simplify left@(RecDefault _) _ = [left]
        simplify (RecRow left) (RecRow right : rest) = simplify combined rest
          where combined = RecRow $ M.union left right
        simplify left rest = left : simplify (head rest) (tail rest)

type Env = Record
type Proc = Int
type Closure = (Proc, Env)

-- TODO: express type values in terms of products (sigmas with tuples/records)
-- combine sealeds with sigmas? namespaces markable as sealed; such a tag implies sealing
data Value = Sym Symbol
           -- | SymTy Namespace
           | Rec Record
           -- | RecTy Namespace [(Symbol, Term)] (Maybe Term)
           | Sig Term Term
           -- | SigTy Term (Term -> Term)
           | Clo Closure
           -- | ProcTy Term Term
           -- | Sealed Namespace Term
           -- | Type -- type of all types
  deriving (Show)

tupleRow items = M.fromList $ zip (map (: []) [0..]) items
tuple items = Literal . Rec . RecRow $ tupleRow items
unit = tuple []
--list

tagged tag datum = Literal $ Sig tag datum

gen_syms = map (: []) [0..]
gen_tags = map (Literal . Sym) gen_syms

sumty ns alts = (sig, [constrs])
  where row = M.fromList $ zip gen_syms alts
        sig = Sig ns $ Abstract [[0]] $ Access (Literal . Rec $ RecRow row) $ Var [0]
        constrs = ()

--[(false_tag, _), (true_tag, _)] = sumty [unitty, unitty]

-- what a joke
(tyty_tag : voidty_tag : symty_tag : recty_tag : sigty_tag : procty_tag : pity_tag : nat_tag : int_tag : boolty_tag : maybety_tag : listty_tag : tytag_gen) = gen_tags
voidty_nsinfo = (0, M.empty)
voidty = tagged voidty_tag unit
tyty = tagged tyty_tag unit
symty nspace = tagged symty_tag nspace
recty nspace row dflt = tagged recty_tag $ tuple [nspace, row, dflt]
sigty term0 term1 = tagged sigty_tag $ tuple [term0, term1]
pity term0 term1 = tagged pity_tag $ tuple [term0, term1]
procty term0 term1 = tagged procty_tag $ tuple [term0, term1]
boolty = tagged boolty_tag unit
maybety term = tagged maybety_tag term
listty term = tagged listty_tag term

tuplety tys = recty nat_tag $ tuple tys
unitty = tuplety []

{-TODO: record type rhs should maybe be more like: Sigma(Type{Symbol}, \NS -> List (NS, Type))-}
  {-supports efficiently providing a set of valid keys using type info-}
  {-record mapping keys to Maybes would require trying every key in the NS to find valid ones-}
    {-well, what about when every key IS specified?-}
    {-maybe better to specify direct representation of rows vs. defaults vs. ranges-}
{-is existential quantification necessary? or use sigmas?-}
{-is sigma-rhs also a sigma? Sigma(Type, \A -> (A -> Type))-}

voidty_rhs = unitty
tyty_rhs = unitty
symty_rhs = tuplety [symty tyty_tag]
-- what the hell am I doing here?
--recty_rhs = sigty (symty tyty_tag) $ Abstract [[0]] $ recty (Var [0]) (list $ tuplety [symty, tyty]) (maybety tyty)
sigty_rhs = sigty tyty $ Abstract [[0]] $ procty (Var [0]) tyty
--type_row_base = M.fromList [] -- create base variant row for Type

type Name = Symbol
data Term = Literal Value
          | Var Name
          | Apply Term [Term]
          | Abstract [Name] Term
          | LetRec [(Name, Term)] Term
          | Access Term Term
          -- record/tuple constructing, splitting, joining, hiding, indexing, linking
          -- sigma construction, dispatch
          -- seals sealing/unsealing
          -- term/value sets (mainly for analysis?)
          -- structural type types
          -- refinement types
          -- type, effect, region, closure, etc. annotations
  deriving (Show)
