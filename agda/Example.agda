-- commandline: agda Example.agda -i /usr/share/agda-stdlib/ -i . -I

module Example where

open import Data.Nat

data Maybe (A : Set) : Set where
  nothing : Maybe A
  just    : A -> Maybe A

maybe : forall {A B : Set} -> B -> (A -> B) -> Maybe A -> B
maybe n j nothing = n
maybe n j (just x) = j x

data List (A : Set) : Set where
  nil : List A
  cons : A -> List A -> List A

data TList : Set₁ where
  tnil : TList
  tcons : Set -> TList -> TList

tlistToArrow : TList -> Set -> Set
tlistToArrow tnil arr = arr
tlistToArrow (tcons ty tys) arr = tlistToArrow tys (ty -> arr)
-- tlistToArrow (tcons ty tnil) arr = tlistToArrow tys (ty -> arr)

id : forall {A : Set} -> A -> A
id x = x

id2 : forall {A : Set} -> A -> A
id2 = \x -> x

--₃ ₂ ₁

data Void : Set where

data Unit : Set where
  unit : Unit

data Bool : Set where
  false : Bool
  true : Bool

data Pair (A B : Set) : Set where
  pair : A -> B -> Pair A B


-- ApplyArrow : TList -> Set -> Set
-- ApplyArrow Ts R = (tlistToArrow Ts R) -> R

-- data G : Set where
--   gr : G

-- Apply : List G -> TList -> Set
-- Apply nil acc = ApplyArrow acc G
-- Apply (cons x xs) acc = Apply xs (tcons G acc)

ApplyFunc : {A : Set} -> (R : Set) -> List A -> Set
ApplyFunc R nil = R
ApplyFunc {A} R (cons x xs) = A -> ApplyFunc {A} R xs

-- PairToArrow : Pair TList Set -> Set
-- PairToArrow (pair Ts R) = (tlistToArrow Ts R) -> R

apply : {A R : Set} -> (args : List A) -> ApplyFunc R args -> R
apply nil f = f
apply (cons x xs) f = apply xs (f x)

testApply : List Bool
testApply = apply (cons true (cons false nil)) (\x y -> cons y (cons x nil))

data TupU : Set₁ where
  UNIT : TupU
  PAIR : Set -> TupU -> TupU

tupEl : TupU -> Set
tupEl UNIT = Unit
tupEl (PAIR A b) = Pair A (tupEl b)

ApplyTupFunc : (tu : TupU) -> (R : Set) -> Set
ApplyTupFunc (UNIT) R = R
ApplyTupFunc (PAIR A B) R = A -> ApplyTupFunc B R

applyTup : {tu : TupU} -> {R : Set} -> (args : tupEl tu) -> ApplyTupFunc tu R -> R
applyTup {UNIT} unit f = f
applyTup {PAIR _ _} (pair x xs) f = applyTup xs (f x)

testApplyTup0 = applyTup unit true

testApplyTup : {R : Set} -> (Bool -> Unit -> Bool -> R) -> R
testApplyTup = applyTup (pair true (pair unit (pair false unit)))

test = testApplyTup (\b0 unit b1 -> cons b1 (cons b0 nil))

{-data ArrayType : Set -> Set where-}
  {-anil : ArrayType Unit-}
  {-acons : forall {A B : _} -> (a : A) -> (b : B) -> ArrayType (Pair A B)-}

{-inspect : (A : Set) -> Bool-}
{-inspect Unit = true-}
--inspect Void = false

-- ApplyTupFunc : (P : Set) -> (R : Set) -> P -> Set
-- ApplyTupFunc (Pair A B) R (pair a b) = A -> ApplyFuncTup B R b
-- ApplyTupFunc _ R _ = R

-- applyTup : {P R : Set} -> (args : P) -> ApplyTupFunc R args -> R
-- applyTup unit f = f
-- applyTup {Pair _ B} (pair a b) f = applyTup {B} b (f a)

tof : {A : Set} -> (arg : A) -> Set
tof {A} _ = A

EorP : (b : Bool) -> Set
EorP true = Unit
EorP false = Pair Unit Unit

getEorP : (b : Bool) -> EorP b
getEorP true = unit
getEorP false = pair unit unit

data EQ {A : Set} (a : A) : A -> Set where
  refl : EQ a a

bleh : EQ true true
bleh = refl

{-testb : (a b : Bool) -> EQ a b -> Unit-}
{-testb _ _ refl = unit-}
--testb true true refl = unit
--testb false false refl = unit

{-isUnit : Unit -> Bool-}
{-isUnit unit = true-}
{-alwaysE : Unit -> Unit-}
{-alwaysE unit = getEorP (isUnit unit)-}
