{-# OPTIONS_GHC -XMultiParamTypeClasses -XFunctionalDependencies #-}

data Ty1 = Ty1 Int Float
     deriving Show
data Ty1r = Ty1r0 Int
          | Ty1r1 Float
     deriving Show

data Ty2 = Ty2 Char String
     deriving Show
data Ty2r = Ty2r0 Char
          | Ty2r1 String
     deriving Show

class Proj t r | t -> r where
      proj :: t -> String -> r

instance Proj Ty1 Ty1r where
         proj (Ty1 int _) "f0" = Ty1r0 int
         proj (Ty1 _ flt) "f1" = Ty1r1 flt

instance Proj Ty2 Ty2r where
         proj (Ty2 ch _) "f0" = Ty2r0 ch
         proj (Ty2 _ str) "f1" = Ty2r1 str

t1 = Ty1 4 7.0
t2 = Ty2 '!' "yay"

f0 t = proj t "f0"
f1 t = proj t "f1"
